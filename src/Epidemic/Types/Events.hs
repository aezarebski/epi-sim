{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Epidemic.Types.Events
  ( EpidemicEvent(Infection, Removal, IndividualSample,
              PopulationSample)
  , popSampPeople
  , popSampSeq
  , indSampPerson
  , indSampSeq
  , EpidemicTree(Branch, Leaf, Shoot)
  , maybeEpidemicTree
  , eventTime
  , ReconstructedTree(RBranch, RLeaf)
  , maybeReconstructedTree
  , eventsInRTree
  , PointProcessEvents(PointProcessEvents)
  , pointProcessEvents
  , derivedFrom
  , Newick
  , asNewickString
  ) where

import qualified Data.Aeson as Json
import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as BBuilder
import qualified Data.List as List
import qualified Data.Vector as V
import Epidemic.Types.Parameter
import Epidemic.Types.Population
import GHC.Generics

type Infector = Person

type Infectee = Person

-- | Description of possible events that can occur in the process.
data EpidemicEvent
  = Infection AbsoluteTime Infector Infectee
  | Removal AbsoluteTime Person
  | IndividualSample
      { indSampTime :: AbsoluteTime
      , indSampPerson :: Person
      , indSampSeq :: Bool
      }
  | PopulationSample
      { popSampTime :: AbsoluteTime
      , popSampPeople :: People
      , popSampSeq :: Bool
      }
  deriving (Show, Generic, Eq)

instance Json.FromJSON EpidemicEvent

instance Json.ToJSON EpidemicEvent

-- | Epidemic Events are ordered based on which occurred first.
instance Ord EpidemicEvent where
  e1 <= e2 = eventTime e1 <= eventTime e2

-- | The time an event occurred.
eventTime :: EpidemicEvent -> AbsoluteTime
eventTime e =
  case e of
    Infection time _ _ -> time
    Removal time _ -> time
    IndividualSample {..} -> indSampTime
    PopulationSample {..} -> popSampTime

-- | The events that occurred as a result of the existance of the given person.
derivedFrom ::
     Person
  -> [EpidemicEvent] -- ^ ordered epidemic events
  -> [EpidemicEvent]
derivedFrom person = derivedFromPeople (asPeople [person])

-- | The events that occurred as a result of the existance of a group of people
derivedFromPeople ::
     People
  -> [EpidemicEvent] -- ^ ordered epidemic events
  -> [EpidemicEvent]
derivedFromPeople _ [] = []
derivedFromPeople people (e:es) =
  case e of
    Infection _ p1 p2 ->
      if includesPerson people p1 || includesPerson people p2
        then let people' = addPerson p2 (addPerson p1 people)
              in e : derivedFromPeople people' es
        else derivedFromPeople people es
    Removal _ p ->
      let derivedEvents = derivedFromPeople people es
       in if includesPerson people p
            then e : derivedEvents
            else derivedEvents
    IndividualSample {..} ->
      let derivedEvents = derivedFromPeople people es
       in if includesPerson people indSampPerson
            then e : derivedEvents
            else derivedEvents
    PopulationSample {..} ->
      let derivedEvents = derivedFromPeople people es
       in if haveCommonPeople people popSampPeople
            then e : derivedEvents
            else derivedEvents

-- | The whole transmission tree including the unobserved leaves. Lineages that
-- are still extant are modelled as /shoots/ and contain a 'Person' as their
-- data rather than an event.
data EpidemicTree
  = Branch EpidemicEvent EpidemicTree EpidemicTree
  | Leaf EpidemicEvent
  | Shoot Person
  deriving (Show, Eq)

-- | If possible return an 'EpidemicTree' describing the /sorted/ list of
-- 'EpidemicEvents'.
maybeEpidemicTree ::
     [EpidemicEvent] -- ^ ordered epidemic events
  -> Maybe EpidemicTree
maybeEpidemicTree [] = Nothing
maybeEpidemicTree [e] =
  case e of
    Infection _ p1 p2 -> Just (Branch e (Shoot p1) (Shoot p2))
    Removal {} -> Just (Leaf e)
    IndividualSample {} -> Just (Leaf e)
    PopulationSample {..} ->
      if nullPeople popSampPeople
        then Nothing
        else Just (Leaf e)
maybeEpidemicTree (e:es) =
  case e of
    Infection _ p1 p2 ->
      let infectorEvents = derivedFrom p1 es
          infecteeEvents = derivedFrom p2 es
       in do leftTree <-
               if null infectorEvents
                 then Just (Shoot p1)
                 else maybeEpidemicTree infectorEvents
             rightTree <-
               if null infecteeEvents
                 then Just (Shoot p2)
                 else maybeEpidemicTree infecteeEvents
             return $ Branch e leftTree rightTree
    Removal {} -> Just (Leaf e)
    IndividualSample {} -> Just (Leaf e)
    PopulationSample {..} ->
      if nullPeople popSampPeople
        then maybeEpidemicTree es
        else Just (Leaf e)

-- | The phylogeny reconstructed from all of the sequenced samples.
data ReconstructedTree
  = RBranch EpidemicEvent ReconstructedTree ReconstructedTree
  | RLeaf EpidemicEvent
  deriving (Show, Eq)

maybeReconstructedTree :: EpidemicTree -> Maybe ReconstructedTree
maybeReconstructedTree Shoot {} = Nothing
maybeReconstructedTree (Leaf e) =
  case e of
    IndividualSample {..} ->
      if indSampSeq
        then Just (RLeaf e)
        else Nothing
    PopulationSample {..} ->
      if popSampSeq
        then Just (RLeaf e)
        else Nothing
    _ -> Nothing
maybeReconstructedTree (Branch e@Infection {} lt rt)
  | hasSequencedLeaf lt && hasSequencedLeaf rt = do
    rlt <- maybeReconstructedTree lt
    rrt <- maybeReconstructedTree rt
    Just $ RBranch e rlt rrt
  | hasSequencedLeaf lt = maybeReconstructedTree lt
  | hasSequencedLeaf rt = maybeReconstructedTree rt
  | otherwise = Nothing
maybeReconstructedTree Branch {} = Nothing

-- | The events from a 'ReconstructedTree'
eventsInRTree :: ReconstructedTree -> [EpidemicEvent]
eventsInRTree = undefined

-- | Predicate for whether an 'EpidemicTree' has a leaf representing a sequenced
-- sample. This is used to determine if the tree needs to be included in the
-- 'ReconstructedTree'.
hasSequencedLeaf :: EpidemicTree -> Bool
hasSequencedLeaf Shoot {} = False
hasSequencedLeaf (Leaf e) =
  case e of
    IndividualSample {..} -> indSampSeq
    PopulationSample {..} -> popSampSeq
    _ -> False
hasSequencedLeaf (Branch _ lt rt) = hasSequencedLeaf lt || hasSequencedLeaf rt

-- | The non-sequenced events.
newtype PointProcessEvents =
  PointProcessEvents [EpidemicEvent]

-- | Extract the non-sequenced events from an epidemic tree.
pointProcessEvents :: EpidemicTree -> PointProcessEvents
pointProcessEvents Shoot {} = PointProcessEvents []
pointProcessEvents (Leaf e) =
  case e of
    IndividualSample {..} ->
      PointProcessEvents $
      if indSampSeq
        then []
        else [e]
    PopulationSample {..} ->
      PointProcessEvents $
      if popSampSeq
        then []
        else [e]
    _ -> PointProcessEvents []
pointProcessEvents (Branch _ lt rt) =
  let (PointProcessEvents lEs) = pointProcessEvents lt
      (PointProcessEvents rEs) = pointProcessEvents rt
      allEs = List.sort $ lEs ++ rEs
   in PointProcessEvents allEs

class Newick t
  -- | Return a representation of the tree in Newick format.
  where
  asNewickString ::
       (AbsoluteTime, Person) -- ^ The person and time of the root of the tree
    -> t
    -> Maybe (BBuilder.Builder, [EpidemicEvent])

ampersandBuilder :: BBuilder.Builder
ampersandBuilder = BBuilder.charUtf8 '&'

colonBuilder :: BBuilder.Builder
colonBuilder = BBuilder.charUtf8 ':'

leftBraceBuilder :: BBuilder.Builder
leftBraceBuilder = BBuilder.charUtf8 '('

rightBraceBuilder :: BBuilder.Builder
rightBraceBuilder = BBuilder.charUtf8 ')'

commaBuilder :: BBuilder.Builder
commaBuilder = BBuilder.charUtf8 ','

catastrophePeopleBuilder :: People -> BBuilder.Builder
catastrophePeopleBuilder (People persons) =
  mconcat $
  List.intersperse ampersandBuilder [personByteString p | p <- V.toList persons]

instance Newick ReconstructedTree where
  asNewickString (t, _) (RLeaf e) =
    let branchLength a b = BBuilder.doubleDec td
          where
            (TimeDelta td) = timeDelta a b
     in case e of
          IndividualSample {..} ->
            if indSampSeq
              then Just
                     ( (personByteString indSampPerson) <>
                       colonBuilder <> branchLength t indSampTime
                     , [e])
              else Nothing
          PopulationSample {..} ->
            if popSampSeq
              then Just
                     ( catastrophePeopleBuilder popSampPeople <>
                       colonBuilder <> branchLength t popSampTime
                     , [e])
              else Nothing
          _ -> Nothing
  asNewickString (t, _) (RBranch e lt rt) =
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
