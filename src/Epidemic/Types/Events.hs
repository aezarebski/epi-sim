{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Epidemic.Types.Events
  ( EpidemicEvent(Infection, Removal, IndividualSample,
              PopulationSample)
  , popSampPeople
  , popSampSeq
  , popSampTime
  , indSampPerson
  , indSampSeq
  , indSampTime
  , EpidemicTree(Branch, Leaf, Shoot)
  , maybeEpidemicTree
  , isExtinctionOrStopping
  , eventTime
  , derivedFrom
  ) where

import qualified Data.Aeson as Json
import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as BBuilder
import qualified Data.List as List
import qualified Data.Vector as V
import Epidemic.Types.Parameter
import Epidemic.Types.Population
import Epidemic.Types.Time (AbsoluteTime(..), TimeDelta(..), timeDelta)
import GHC.Generics

-- | Events that can occur in an epidemic with their absolute time.
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
  | Extinction -- ^ epidemic went extinct time time can be recovered from the preceeding removal
  | StoppingTime -- ^ the simulation reached the stopping time
  deriving (Show, Generic, Eq)

instance Json.FromJSON EpidemicEvent

instance Json.ToJSON EpidemicEvent

-- | Predicate for whether an @EpidemicEvent@ is one of the terminal events of
-- extinction or the stopping time having been reached.
isExtinctionOrStopping :: EpidemicEvent -> Bool
isExtinctionOrStopping e =
  case e of
    Extinction -> True
    StoppingTime -> True
    _ -> False

-- | Epidemic Events are ordered based on which occurred first. Since
-- 'Extinction' and 'StoppingTime' events are there as placeholders they are
-- placed as the end of the order.
instance Ord EpidemicEvent where
  Extinction <= Extinction = True
  Extinction <= StoppingTime = True
  Extinction <= _ = False
  StoppingTime <= Extinction = False
  StoppingTime <= StoppingTime = True
  StoppingTime <= _ = False
  e1 <= e2 = eventTime e1 <= eventTime e2

-- | The absolute time an event occurred.
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
  -> Either String EpidemicTree
maybeEpidemicTree [] =
  Left "There are no EpidemicEvents to construct a tree with."
maybeEpidemicTree [e] =
  case e of
    Infection _ p1 p2 -> Right (Branch e (Shoot p1) (Shoot p2))
    Removal {} -> Right (Leaf e)
    IndividualSample {} -> Right (Leaf e)
    PopulationSample {..} ->
      if nullPeople popSampPeople
        then Left "The last event is a PopulationSample with no people sampled"
        else Right (Leaf e)
    Extinction -> Left "Extinction event encountered"
    StoppingTime -> Left "Stopping time encountered"
    _ -> Right (Leaf e)
maybeEpidemicTree (e:es) =
  case e of
    Infection _ p1 p2 ->
      let infectorEvents = derivedFrom p1 es
          infecteeEvents = derivedFrom p2 es
       in do leftTree <-
               if null infectorEvents
                 then Right (Shoot p1)
                 else maybeEpidemicTree infectorEvents
             rightTree <-
               if null infecteeEvents
                 then Right (Shoot p2)
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
eventsInRTree node =
  case node of
    RBranch e lt rt ->
      List.insert e (List.sort $ eventsInRTree lt ++ eventsInRTree rt)
    RLeaf e -> [e]

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
