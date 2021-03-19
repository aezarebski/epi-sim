{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}

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

import Control.Monad (liftM)
import qualified Data.Aeson as Json
import qualified Data.ByteString.Builder as BBuilder
import qualified Data.List as List
import qualified Data.Vector as V
import Epidemic.Types.Events
  ( EpidemicEvent(..)
  , EpidemicTree(..)
  , isIndividualSample
  , maybeEpidemicTree
  )
import Epidemic.Types.Population (People(..), asPeople, personByteString)
import Epidemic.Types.Time
  ( AbsoluteTime(..)
  , TimeDelta(..)
  , TimeInterval(..)
  , TimeStamp(..)
  , timeDelta
  , inInterval
  )
import GHC.Generics

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
      PointProcessEvents $
      if not indSampSeq
        then [Observation e]
        else []
    PopulationSample {..} ->
      PointProcessEvents $
      if not popSampSeq
        then [Observation e]
        else []
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
    _ -> False
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
      then (liftM reconstructedTreeEvents) $ maybeReconstructedTree epiTree
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
aggregated seqAggInts unseqAggInts =
  aggregateUnsequenced unseqAggInts . aggregateSequenced seqAggInts

aggregateSequenced :: [TimeInterval] -> [Observation] -> [Observation]
aggregateSequenced = undefined

_aggregateSequenced :: TimeInterval -> [Observation] -> Observation
_aggregateSequenced = undefined

aggregateUnsequenced :: [TimeInterval] -> [Observation] -> [Observation]
aggregateUnsequenced = undefined

_aggregateUnsequenced :: TimeInterval -> [Observation] -> Observation
_aggregateUnsequenced = undefined

asPopulationSample :: [Observation] -> AbsoluteTime -> Observation
asPopulationSample obs absTime =
  let ees = [ee | Observation ee <- obs]
   in if all isIndividualSample ees
        then let people = asPeople $ map indSampPerson ees
              in if all indSampSeq ees
                   then Observation $ PopulationSample absTime people True
                   else Observation $ PopulationSample absTime people False
        else error "bad observation "
