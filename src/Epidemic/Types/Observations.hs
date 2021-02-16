{-# LANGUAGE DeriveGeneric #-}

module Epidemic.Types.Observations
  ( Observation(..)
  , ReconstructedTree(..)
  , maybeReconstructedTree
  , PointProcessEvents(..)
  , pointProcessEvents
  , observedEvents
  ) where

import qualified Data.List as List
import qualified Data.Vector as V
import qualified Data.Aeson as Json
import qualified Data.ByteString.Builder as BBuilder
import Epidemic.Types.Events
  ( EpidemicEvent(..)
  , EpidemicTree(..)
  , Newick(..)
  )
import Epidemic.Types.Parameter (TimeDelta(..), timeDelta)
import Epidemic.Types.Population (People(..), personByteString)
import GHC.Generics

-- | A wrapper for an 'EpidemicEvent' to indicate that this is an even that was
-- observed rather than just an event of the epidemic process.
newtype Observation =
  Observation EpidemicEvent
  deriving (Show, Ord, Eq, Generic)

instance Json.FromJSON Observation

instance Json.ToJSON Observation

-- | A representation of the events that can be observed in an epidemic but
-- which are not included in the reconstructed tree, i.e. the `Occurrence` and
-- 'Disaster' events.
newtype PointProcessEvents =
  PointProcessEvents [Observation]

-- | Extract the events from an epidemic tree which are observed but not part of
-- the reconstructed tree.
pointProcessEvents :: EpidemicTree -> PointProcessEvents
pointProcessEvents Shoot {} = PointProcessEvents []
pointProcessEvents (Leaf e) =
  case e of
    Occurrence {} -> PointProcessEvents [Observation e]
    Disaster {} -> PointProcessEvents [Observation e]
    _ -> PointProcessEvents []
pointProcessEvents (Branch _ lt rt) =
  let (PointProcessEvents lEs) = pointProcessEvents lt
      (PointProcessEvents rEs) = pointProcessEvents rt
      allEs = List.sort $ lEs ++ rEs
   in PointProcessEvents allEs

-- | A representation of the reconstructed tree, ie the tree where the leaves
-- correspond to the 'Sampling' and 'Catastrophe' events.
data ReconstructedTree
  = RBranch Observation ReconstructedTree ReconstructedTree
  | RLeaf Observation
  deriving (Show, Eq)

-- | The reconstructed phylogeny obtained by pruning an 'EpidemicTree' which
-- contains represents the transmission tree of the epidemic. In the case where
-- there are no sequenced samples in the epidemic then there is no tree to
-- reconstruct which is why this function is in the maybe monad.
maybeReconstructedTree :: EpidemicTree -> Maybe ReconstructedTree
maybeReconstructedTree Shoot {} = Nothing
maybeReconstructedTree (Leaf e) =
  case e of
    Sampling {} -> Just $ RLeaf (Observation e)
    Catastrophe {} -> Just $ RLeaf (Observation e)
    _ -> Nothing
maybeReconstructedTree (Branch e@Infection {} lt rt)
  | hasSequencedLeaf lt && hasSequencedLeaf rt = do
    rlt <- maybeReconstructedTree lt
    rrt <- maybeReconstructedTree rt
    Just $ RBranch (Observation e) rlt rrt
  | hasSequencedLeaf lt = maybeReconstructedTree lt
  | hasSequencedLeaf rt = maybeReconstructedTree rt
  | otherwise = Nothing
maybeReconstructedTree Branch {} = Nothing

-- | Predicate for whether an 'EpidemicTree' has any leaf which corresponds to a
-- sequenced observation and hence should be included in a @ReconstructedTree@.
hasSequencedLeaf :: EpidemicTree -> Bool
hasSequencedLeaf Shoot {} = False
hasSequencedLeaf (Leaf e) =
  case e of
    Sampling {} -> True
    Catastrophe {} -> True
    _ -> False
hasSequencedLeaf (Branch _ lt rt) = hasSequencedLeaf lt || hasSequencedLeaf rt

observedEvents :: [EpidemicEvent] -> Either String [Observation]
observedEvents = undefined


-- observedEvents :: [EpidemicEvent] -- ^ All of the simulation events
--                -> Maybe [EpidemicEvent]
-- observedEvents eEvents = do
--   epiTree <- maybeEpidemicTree eEvents
--   reconTree <- maybeReconstructedTree epiTree
--   let (PointProcessEvents nonReconTreeEvents) = pointProcessEvents epiTree
--   let reconTreeEvents = reconstructedTreeEvents reconTree
--   return . sort . nub $ nonReconTreeEvents ++ reconTreeEvents

-- -- | The events from the nodes of a reconstructed tree __not__ in time sorted
-- -- order.
-- reconstructedTreeEvents :: ReconstructedTree -> [EpidemicEvent]
-- reconstructedTreeEvents node = case node of
--   (RBranch e lt rt) -> e:(reconstructedTreeEvents lt ++ reconstructedTreeEvents rt)
--   (RLeaf e) -> [e]






-- observedEvents :: [EpidemicEvent] -- ^ All of the simulation events
--                -> [EpidemicEvent]
-- observedEvents [] = []
-- observedEvents events = sort sampleTreeEvents''
--   where
--     sampleTreeEvents'' =
--       sampleTreeEvents . sampleTree $ transmissionTree events (Person initialIdentifier)


instance Newick ReconstructedTree where
  asNewickString (t, _) (RLeaf (Observation e)) =
    let branchLength a b = BBuilder.doubleDec td
          where
            (TimeDelta td) = timeDelta a b
     in case e of
          (Sampling t' p) ->
            Just
              ((personByteString p) <> colonBuilder <> branchLength t t', [e])
          Infection {} -> Nothing
          Removal {} -> Nothing
          (Catastrophe t' ps) ->
            Just
              ( catastrophePeopleBuilder ps <> colonBuilder <> branchLength t t'
              , [e])
          Occurrence {} -> Nothing
          Disaster {} -> Nothing
          Extinction {} -> Nothing
          StoppingTime {} -> Nothing
  asNewickString (t, _) (RBranch (Observation e) lt rt) =
    case e of
      (Infection t' p1 p2) -> do
        (leftNS, leftEs) <- asNewickString (t', p1) lt
        (rightNS, rightEs) <- asNewickString (t', p2) rt
        let branchLength = BBuilder.doubleDec td
              where
                (TimeDelta td) = timeDelta t t'
        return
          ( leftBraceBuilder <>
            leftNS <>
            commaBuilder <>
            rightNS <> rightBraceBuilder <> colonBuilder <> branchLength
          , List.sort $ leftEs ++ rightEs)
      _ -> Nothing


ampersandBuilder :: BBuilder.Builder
ampersandBuilder = BBuilder.charUtf8 '&'

catastrophePeopleBuilder :: People -> BBuilder.Builder
catastrophePeopleBuilder (People persons) =
  mconcat $
  List.intersperse ampersandBuilder [personByteString p | p <- V.toList persons]

colonBuilder :: BBuilder.Builder
colonBuilder = BBuilder.charUtf8 ':'

leftBraceBuilder :: BBuilder.Builder
leftBraceBuilder = BBuilder.charUtf8 '('

rightBraceBuilder :: BBuilder.Builder
rightBraceBuilder = BBuilder.charUtf8 ')'

commaBuilder :: BBuilder.Builder
commaBuilder = BBuilder.charUtf8 ','



