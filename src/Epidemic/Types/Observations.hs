{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE RecordWildCards #-}

module Epidemic.Types.Observations
  ( Observation(..)
  , ReconstructedTree(..)
  , maybeReconstructedTree
  , PointProcessEvents(..)
  , pointProcessEvents
  , reconstructedTreeEvents
  , observedEvents
  , aggregated
  ) where

import qualified Data.Aeson                as Json
import qualified Data.List                 as List
import           Epidemic.Types.Events     (EpidemicEvent (..),
                                            EpidemicTree (..),
                                            maybeEpidemicTree)
import           Epidemic.Types.Population (asPeople)
import           Epidemic.Types.Time       (TimeInterval (..), TimeStamp (..),
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
pointProcessEvents :: EpidemicTree -> PointProcessEvents
pointProcessEvents Shoot {} = PointProcessEvents []
pointProcessEvents (Leaf e) =
  case e of
    IndividualSample {..} ->
      PointProcessEvents [Observation e | not indSampSeq]
    PopulationSample {..} ->
      PointProcessEvents [Observation e | not popSampSeq]
    _ -> PointProcessEvents []
pointProcessEvents (Branch _ lt rt) =
  let (PointProcessEvents lEs) = pointProcessEvents lt
      (PointProcessEvents rEs) = pointProcessEvents rt
      allEs = List.sort $ lEs ++ rEs
   in PointProcessEvents allEs

-- | A representation of the reconstructed tree, ie the tree where the leaves
-- correspond to sequenced observations.
data ReconstructedTree
  = RBranch Observation ReconstructedTree ReconstructedTree
  | RLeaf Observation
  deriving (Show, Eq)

-- | The reconstructed phylogeny obtained by pruning an 'EpidemicTree' which
-- contains represents the transmission tree of the epidemic. In the case where
-- there are no sequenced samples in the epidemic then there is no tree to
-- reconstruct which is why this function is in the either monad.
maybeReconstructedTree :: EpidemicTree -> Either String ReconstructedTree
maybeReconstructedTree Shoot {} = Left "EpidemicTree is only a Shoot"
maybeReconstructedTree (Leaf e) =
  case e of
    IndividualSample {..} ->
      if indSampSeq
        then Right $ RLeaf (Observation e)
        else Left "Leaf with non-sequenced event individual sample"
    PopulationSample {..} ->
      if popSampSeq
        then Right $ RLeaf (Observation e)
        else Left "Leaf with non-sequenced event population sample"
    _ -> Left "Bad leaf in the EpidemicTree"
maybeReconstructedTree (Branch e@Infection {} lt rt)
  | hasSequencedLeaf lt && hasSequencedLeaf rt = do
    rlt <- maybeReconstructedTree lt
    rrt <- maybeReconstructedTree rt
    Right $ RBranch (Observation e) rlt rrt
  | hasSequencedLeaf lt = maybeReconstructedTree lt
  | hasSequencedLeaf rt = maybeReconstructedTree rt
  | otherwise = Left "Neither subtree has a sequenced leaf"
maybeReconstructedTree Branch {} = Left "EpidemicTree is a bad branch"

-- | Predicate for whether an 'EpidemicTree' has any leaf which corresponds to a
-- sequenced observation and hence should be included in a @ReconstructedTree@.
hasSequencedLeaf :: EpidemicTree -> Bool
hasSequencedLeaf Shoot {} = False
hasSequencedLeaf (Leaf e) =
  case e of
    IndividualSample {..} -> indSampSeq
    PopulationSample {..} -> popSampSeq
    _                     -> False
hasSequencedLeaf (Branch _ lt rt) = hasSequencedLeaf lt || hasSequencedLeaf rt

-- | The events that were observed during the epidemic, ie those in the
-- reconstructed tree and any unsequenced samples. If this is not possible an
-- error message will be returned.
observedEvents :: [EpidemicEvent] -> Either String [Observation]
observedEvents epiEvents = do
  epiTree <- maybeEpidemicTree epiEvents
  let (PointProcessEvents unseqObss) = pointProcessEvents epiTree
  reconTreeEvents <-
    if hasSequencedLeaf epiTree
      then reconstructedTreeEvents <$> maybeReconstructedTree epiTree
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
