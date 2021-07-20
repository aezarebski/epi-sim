{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE RecordWildCards #-}

module Epidemic.Data.Observations
  ( Observation(..)
  , ReconstructedTree(..)
  , reconstructedTree
  , PointProcessEvents(..)
  , pointProcessEvents
  , reconstructedTreeEvents
  , observedEvents
  , aggregated
  ) where

import qualified Data.Aeson               as Json
import qualified Data.List                as List
import           Epidemic.Data.Events     (EpidemicEvent (..),
                                           EpidemicTree (..), maybeEpidemicTree)
import           Epidemic.Data.Population (asPeople)
import           Epidemic.Data.Time       (TimeInterval (..), TimeStamp (..),
                                           inInterval)
import           GHC.Generics

-- | A wrapper for an 'EpidemicEvent' to indicate that this is an even that was
-- observed rather than just an event of the epidemic process.
newtype Observation =
  Observation EpidemicEvent
  deriving (Show, Ord, Eq, Generic)

instance Json.FromJSON Observation

instance Json.ToJSON Observation

instance TimeStamp Observation where
  absTime (Observation ee) = absTime ee

-- | A representation of the events that can be observed in an epidemic but
-- which are not included in the reconstructed tree, ie the unsequenced
-- observations.
newtype PointProcessEvents =
  PointProcessEvents [Observation]

-- | Extract the events from an epidemic tree which are observed but not part of
-- the reconstructed tree, ie the ones that are not sequenced.
pointProcessEvents :: EpidemicTree -> Either String PointProcessEvents
pointProcessEvents Shoot {} = return $ PointProcessEvents []
pointProcessEvents (Leaf e) =
  case e of
    Removal {} ->
      return $ PointProcessEvents []
    IndividualSample {..} ->
      return $ PointProcessEvents [Observation e | not indSampSeq]
    PopulationSample {..} ->
      return $ PointProcessEvents [Observation e | not popSampSeq]
    _ -> Left "encountered invalid leaf in epidemic tree."
pointProcessEvents (Branch _ lt rt) = do
  (PointProcessEvents lEs) <- pointProcessEvents lt
  (PointProcessEvents rEs) <- pointProcessEvents rt
  let allEs = List.sort $ lEs <> rEs
    in return $ PointProcessEvents allEs
pointProcessEvents (Burr e t) = do
  (PointProcessEvents es) <- pointProcessEvents t
  case e of
    IndividualSample {..} ->
      if not indSampRemoved
      then return . PointProcessEvents . List.sort $ [Observation e | not indSampSeq] <> es
      else Left "encountered burr with removal in epidemic tree"
    _ -> Left "encountered non-individual sample burr in epidemic tree."


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
reconstructedTree Shoot {} = Left "EpidemicTree is only a Shoot"
reconstructedTree (Leaf e) =
  case e of
    IndividualSample {..} ->
      if indSampSeq && indSampRemoved
        then Right $ RLeaf (Observation e)
        else Left $ "Leaf with non-sequenced or non-removed individual sample: \n" <> show e
    PopulationSample {..} ->
      if popSampSeq
        then Right $ RLeaf (Observation e)
        else Left "Leaf with non-sequenced event population sample"
    _ -> Left "Bad leaf encountered when trying to reconstruct tree."
reconstructedTree (Branch e lt rt) =
  case e of
    Infection {..}
      | hasSequencedObs lt && hasSequencedObs rt ->
        do rlt <- reconstructedTree lt
           rrt <- reconstructedTree rt
           return $ RBranch (Observation e) rlt rrt
      | hasSequencedObs lt -> reconstructedTree lt
      | hasSequencedObs rt -> reconstructedTree rt
      | otherwise -> Left "neither sub-tree has a sequenced node."
    _ -> Left "non-infection event in branch when trying to reconstruct tree."
reconstructedTree (Burr e t) =
  case e of
    IndividualSample {..} ->
      if | indSampSeq && indSampRemoved && hasSequencedObs t ->
           do
             st <- reconstructedTree t
             return $ RBurr (Observation e) (Just st)
         | indSampSeq && indSampRemoved && not (hasSequencedObs t) ->
           return $ RBurr (Observation e) Nothing
         | otherwise -> Left "invalid individual sample encountered in burr."
    _ -> Left "non-individual sample encountered in burr."

-- | Predicate for whether an 'EpidemicTree' has any node which corresponds to a
-- sequenced observation and hence should be included in a @ReconstructedTree@.
hasSequencedObs :: EpidemicTree -> Bool
hasSequencedObs Shoot {} = False
hasSequencedObs (Leaf e) =
  case e of
    IndividualSample {..} -> indSampSeq
    PopulationSample {..} -> popSampSeq
    _                     -> False
hasSequencedObs (Branch _ lt rt) = hasSequencedObs lt || hasSequencedObs rt
hasSequencedObs (Burr e t) =
  case e of
    IndividualSample {..} -> indSampSeq || hasSequencedObs t
    _                     -> False

-- | The events that were observed during the epidemic, ie those in the
-- reconstructed tree and any unsequenced samples. If this is not possible an
-- error message will be returned.
observedEvents :: [EpidemicEvent] -> Either String [Observation]
observedEvents epiEvents = do
  epiTree <- maybeEpidemicTree epiEvents
  (PointProcessEvents unseqObss) <- pointProcessEvents epiTree
  reconTreeEvents <-
    if hasSequencedObs epiTree
      then reconstructedTreeEvents <$> reconstructedTree epiTree
      else Right []
  return $ List.sort . List.nub $ unseqObss ++ reconTreeEvents

-- | A sorted list of all of the observations in the reconstructed tree.
reconstructedTreeEvents :: ReconstructedTree -> [Observation]
reconstructedTreeEvents rt =
  case rt of
    RBranch obs rtl rtr ->
      List.sort $
      obs : (reconstructedTreeEvents rtl ++ reconstructedTreeEvents rtr)
    RLeaf obs -> [obs]
    RBurr obs mt ->
      case mt of
        Nothing -> [obs]
        Just t -> List.sort $ obs : reconstructedTreeEvents t

-- | Aggregate the sequenced and unsequenced individual level samples
aggregated :: [TimeInterval] -> [TimeInterval] -> [Observation] -> [Observation]
aggregated seqAggInts unseqAggInts = List.sort . aggUnsequenced . aggSequenced
  where
    aggUnsequenced = _aggregate unseqAggInts False
    aggSequenced = _aggregate seqAggInts True

-- | Aggregate observations in each of the intervals given the correct
-- sequencing status.
_aggregate :: [TimeInterval] -> Bool -> [Observation] -> [Observation]
_aggregate intervals onlySequenced obs = List.foldl' f obs intervals
  where
    f os i = _aggregateInInterval i onlySequenced os

-- | Aggregate all the observations that fall in the interval and have the
-- correct sequencing status.
_aggregateInInterval :: TimeInterval -> Bool -> [Observation] -> [Observation]
_aggregateInInterval interval@TimeInterval {..} onlySequenced obs =
  let asPopulationSample os absT =
        Observation $
        PopulationSample
          absT
          (asPeople [indSampPerson ee | Observation ee <- os])
          onlySequenced
      (_, aggTime) = timeIntEndPoints
      toBeAggregated o =
        case o of
          Observation (IndividualSample {..}) ->
            inInterval interval o &&
            (if onlySequenced
               then indSampSeq
               else not indSampSeq)
          _ -> False
      (obs2Agg, otherObs) = List.partition toBeAggregated obs
      newPopSample = asPopulationSample obs2Agg aggTime
   in newPopSample : otherObs
