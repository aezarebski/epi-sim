{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE MultiWayIf      #-}
{-# LANGUAGE RecordWildCards #-}

module Epidemic.Data.Observations
  ( Observation(..)
  , observations
  , UnsequencedObs(..)
  , unsequencedObservations
  , ReconstructedTree(..)
  , reconstructedTree
  , SequencedObs(..)
  , sequencedObservations
  , aggregated
  ) where

import qualified Data.Aeson               as Json
import qualified Data.List                as List
import           Epidemic.Data.Events     (EpidemicEvent (..),
                                           EpidemicTree (..), hasSequencedObs)
import           Epidemic.Data.Population (asPeople, numPeople)
import           Epidemic.Data.Time       (AbsoluteTime, TimeInterval (..),
                                           TimeStamp (..), inInterval)
import           GHC.Generics             (Generic)

-- | Observations relating to sequenced samples.
--
--     * ObsBranch - a bifurcation in the reconstructed tree
--     * ObsBurr - a sampled (sequenced) ancestor
--     * ObsLeafRemoved - a removed (sequenced) individual
--     * ObsLeafNotRemoved - non-removed without any sequenced descendent
--     * ObsLeafScheduled - scheduled sequenced sample
--     * ObsOccurrenceScheduled - scheduled unsequenced sample
--     * ObsOccurrenceRemoved - occurrence with removal
--     * ObsOccurrenceNotRemoved - occurrence without removal
--
data Observation
  = ObsBranch AbsoluteTime
  | ObsBurr AbsoluteTime
  | ObsLeafRemoved AbsoluteTime
  | ObsLeafNotRemoved AbsoluteTime
  | ObsLeafScheduled AbsoluteTime Int
  | ObsOccurrenceScheduled AbsoluteTime Int
  | ObsOccurrenceRemoved AbsoluteTime
  | ObsOccurrenceNotRemoved AbsoluteTime
  deriving (Show, Eq, Generic, Ord)

instance Json.FromJSON Observation

instance Json.ToJSON Observation

instance TimeStamp Observation where
  absTime obs =
    case obs of
      ObsBranch at                -> at
      ObsBurr at                  -> at
      ObsLeafRemoved at           -> at
      ObsLeafNotRemoved at        -> at
      ObsLeafScheduled at _       -> at
      ObsOccurrenceScheduled at _ -> at
      ObsOccurrenceRemoved at     -> at
      ObsOccurrenceNotRemoved at  -> at

-- | The observations due to non-sequenced observations.
newtype UnsequencedObs = UnsequencedObs [Observation]

-- | The non-sequenced observations from an epidemic.
unsequencedObservations :: EpidemicTree -> Either String UnsequencedObs
unsequencedObservations et =
  case et of
    Branch _ lt rt ->
      do (UnsequencedObs unseqOsL) <- unsequencedObservations lt
         (UnsequencedObs unseqOsR) <- unsequencedObservations rt
         Right . UnsequencedObs . List.sort $ unseqOsL <> unseqOsR

    Burr _ st -> unsequencedObservations st

    Leaf e ->
      case e of
        Removal {} -> Right $ UnsequencedObs []
        IndividualSample {..} ->
          if not indSampSeq
          then Right . UnsequencedObs $
               if indSampRemoved
               then [ObsOccurrenceRemoved indSampTime]
               else [ObsOccurrenceNotRemoved indSampTime]
          else Right $ UnsequencedObs []
        PopulationSample {..} ->
          if not popSampSeq
          then Right $ UnsequencedObs [ObsOccurrenceScheduled popSampTime (numPeople popSampPeople)]
          else Right $ UnsequencedObs []
        _ -> Left $ "Invalid event on leaf: " <> show e

    Shoot {} -> Right $ UnsequencedObs []

-- | The observations due to sequenced observations.
newtype SequencedObs = SequencedObs [Observation]

-- | The sequenced observations from an epidemic extracted from a reconstructed
-- tree.
sequencedObservations :: ReconstructedTree -> Either String SequencedObs
sequencedObservations rt =
  case rt of
    RBranch obs rrt lrt ->
      do (SequencedObs rso) <- sequencedObservations rrt
         (SequencedObs lso) <- sequencedObservations lrt
         Right . SequencedObs . List.sort $ obs : rso <> lso

    RBurr obs Nothing -> Right $ SequencedObs [obs]
    RBurr obs (Just srt) ->
      do (SequencedObs sso) <- sequencedObservations srt
         Right . SequencedObs $ obs:sso

    RLeaf obs -> Right $ SequencedObs [obs]

-- | A representation of the reconstructed tree, ie the tree where the leaves
-- correspond to sequenced observations.
--
--     * Branch - an bifurcation
--     * Burr - sequenced individual without removal
--     * Leaf - sequenced removal
--
data ReconstructedTree
  = RBranch Observation ReconstructedTree ReconstructedTree
  | RBurr Observation (Maybe ReconstructedTree)
  | RLeaf Observation
  deriving (Show, Eq)

-- | The reconstructed phylogeny obtained by pruning an 'EpidemicTree' which
-- represents the whole transmission tree of the epidemic.
reconstructedTree :: EpidemicTree -> Either String ReconstructedTree
reconstructedTree et =
  case et of
    Branch ee@Infection {..} lt rt
      | hasSequencedObs lt && hasSequencedObs rt ->
        do lrt <- reconstructedTree lt
           rrt <- reconstructedTree rt
           Right $ RBranch (ObsBranch infTime) lrt rrt
      -- if one of the branches does not have any sequenced observations then we
      -- do not need this infection node in the reconstruction.
      | hasSequencedObs lt -> reconstructedTree lt
      | hasSequencedObs rt -> reconstructedTree rt
      | otherwise -> Left $ "bad infection event in branch: " <> show ee
    Branch ee _ _ -> Left $ "bad branching event: " <> show ee

    Burr ee@IndividualSample {..} st ->
      case (indSampSeq, not indSampRemoved, hasSequencedObs st) of
        (True,True,True) -> do rst <- reconstructedTree st
                               Right $ RBurr (ObsBurr indSampTime) (Just rst)
        (True,True,False) -> Right $ RBurr (ObsBurr indSampTime) Nothing
        _ -> Left $ "invalid individual sample in burr: " <> show ee
    Burr ee _ -> Left $ "non-individual sample in burr: " <> show ee

    Leaf ee ->
      case ee of
        IndividualSample {..} ->
          if indSampRemoved && indSampSeq
          then Right . RLeaf $ ObsLeafRemoved indSampTime
          else Left $ "bad individual sample while reconstructing: " <> show ee
        PopulationSample {..} ->
          if popSampSeq
          then Right . RLeaf $ ObsLeafScheduled popSampTime (numPeople popSampPeople)
          else Left $ "bad population sampled while reconstructing: " <> show ee
        _ -> Left $ "Invalid leaf event encountered: " <> show ee

    Shoot ee ->
      Left $ "EpidemicTree appears to only be a shoot with event " <> show ee

-- | The events that were observed during the epidemic.
observations :: EpidemicTree -> Either String [Observation]
observations et = do
  (UnsequencedObs unseqObs) <- unsequencedObservations et
  reconTree <- reconstructedTree et
  (SequencedObs seqObs) <- sequencedObservations reconTree
  Right . List.sort $ unseqObs <> seqObs

-- | Aggregate the sequenced and unsequenced individual level samples
aggregated :: [TimeInterval] -> [TimeInterval] -> [Observation] -> [Observation]
aggregated = error "not implemented"
-- aggregated seqAggInts unseqAggInts = List.sort . aggUnsequenced . aggSequenced
--   where
--     aggUnsequenced = _aggregate unseqAggInts False
--     aggSequenced = _aggregate seqAggInts True

-- | Aggregate observations in each of the intervals given the correct
-- sequencing status.
_aggregate :: [TimeInterval] -> Bool -> [Observation] -> [Observation]
_aggregate = error "not implemented"
-- _aggregate intervals onlySequenced obs = List.foldl' f obs intervals
--   where
--     f os i = _aggregateInInterval i onlySequenced os

-- | Aggregate all the observations that fall in the interval and have the
-- correct sequencing status.
_aggregateInInterval :: TimeInterval -> Bool -> [Observation] -> [Observation]
_aggregateInInterval = error "not implemented"
-- _aggregateInInterval interval@TimeInterval {..} onlySequenced obs =
--   let asPopulationSample os absT =
--         Observation $
--         PopulationSample
--           absT
--           (asPeople [indSampPerson ee | Observation ee <- os])
--           onlySequenced
--       (_, aggTime) = timeIntEndPoints
--       toBeAggregated o =
--         case o of
--           Observation (IndividualSample {..}) ->
--             inInterval interval o &&
--             (if onlySequenced
--                then indSampSeq
--                else not indSampSeq)
--           _ -> False
--       (obs2Agg, otherObs) = List.partition toBeAggregated obs
--       newPopSample = asPopulationSample obs2Agg aggTime
--    in newPopSample : otherObs
