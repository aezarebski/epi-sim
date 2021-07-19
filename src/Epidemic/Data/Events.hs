{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Epidemic.Data.Events
  ( EpidemicEvent(Infection, Removal, IndividualSample,
              PopulationSample, StoppingTime, Extinction)
  , eventPopDelta
  , popSampPeople
  , popSampSeq
  , popSampTime
  , indSampPerson
  , indSampSeq
  , indSampTime
  , EpidemicTree(Branch, Leaf, Shoot)
  , maybeEpidemicTree
  , isExtinctionOrStopping
  , isIndividualSample
  , derivedFrom
  ) where

import qualified Data.Aeson                as Json
import           Epidemic.Data.Population
import           Epidemic.Data.Parameter (Probability, noScheduledEvent)
import           Epidemic.Data.Time       (AbsoluteTime (..), TimeStamp (..))
import           GHC.Generics

-- | Events that can occur in an epidemic with their absolute time.
data EpidemicEvent
  = Infection AbsoluteTime Person Person -- ^ absolute time; infector; infectee
  | Removal AbsoluteTime Person
  | IndividualSample
      { indSampTime   :: AbsoluteTime
      , indSampPerson :: Person
      , indSampSeq    :: Bool
      }
  | PopulationSample
      { popSampTime   :: AbsoluteTime
      , popSampPeople :: People
      , popSampSeq    :: Bool
      }
  | Extinction AbsoluteTime -- ^ epidemic went extinct
  | StoppingTime AbsoluteTime -- ^ the simulation reached the stopping time
  deriving (Show, Generic, Eq)

instance Json.FromJSON EpidemicEvent

instance Json.ToJSON EpidemicEvent

instance TimeStamp EpidemicEvent where
  absTime ee =
    case ee of
      Infection absT _ _    -> absT
      Removal absT _        -> absT
      IndividualSample {..} -> indSampTime
      PopulationSample {..} -> popSampTime
      StoppingTime absT     -> absT
      Extinction absT       -> absT

-- | Predicate for the event being an individual sample event.
isIndividualSample :: EpidemicEvent -> Bool
isIndividualSample ee =
  case ee of
    IndividualSample {} -> True
    _                   -> False

-- | Predicate for whether an @EpidemicEvent@ is one of the terminal events of
-- extinction or the stopping time having been reached.
isExtinctionOrStopping :: EpidemicEvent -> Bool
isExtinctionOrStopping e =
  case e of
    Extinction {}   -> True
    StoppingTime {} -> True
    _               -> False

-- | Epidemic Events are ordered based on which occurred first. Since
-- 'Extinction' and 'StoppingTime' events are there as placeholders they are
-- placed as the end of the order.
instance Ord EpidemicEvent where
  e1 <= e2 = absTime e1 <= absTime e2

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
    Extinction {} -> derivedFromPeople people es
    StoppingTime {} -> derivedFromPeople people es

-- | The whole transmission tree including the unobserved leaves. Lineages that
-- are still extant are modelled as /shoots/ and contain a 'Person' as their
-- data rather than an event.
data EpidemicTree
  = Branch EpidemicEvent EpidemicTree EpidemicTree
  | Leaf EpidemicEvent
  | Shoot Person
  deriving (Show, Eq)

-- | If possible return an 'EpidemicTree' describing the /sorted/ list of
-- 'EpidemicEvent'.
maybeEpidemicTree ::
     [EpidemicEvent] -- ^ ordered epidemic events
  -> Either String EpidemicTree
maybeEpidemicTree [] =
  Left "There are no EpidemicEvent values to construct a tree with."
maybeEpidemicTree [e] =
  case e of
    Infection _ p1 p2 -> Right (Branch e (Shoot p1) (Shoot p2))
    Removal {} -> Right (Leaf e)
    IndividualSample {} -> Right (Leaf e)
    PopulationSample {..} ->
      if nullPeople popSampPeople
        then Left "The last event is a PopulationSample with no people sampled"
        else Right (Leaf e)
    Extinction {} ->
      Left "Extinction event encountered. It should have been removed"
    StoppingTime {} ->
      Left "Stopping time encountered. It should have been removed"
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
    Removal {} -> Right (Leaf e)
    IndividualSample {} -> Right (Leaf e)
    PopulationSample {..} ->
      if nullPeople popSampPeople
        then maybeEpidemicTree es
        else Right (Leaf e)
    Extinction {} ->
      Left "Extinction event encountered. It should have been removed"
    StoppingTime {} ->
      Left "Stopping time encountered. It should have been removed"

-- | The number of people added or removed in an event. In the case of an
-- extinction event the number of people removed is arbitrarily set to zero
-- because this information is available from the prior event in the sequence.
eventPopDelta :: EpidemicEvent -> Integer
eventPopDelta e =
  case e of
    Infection {}          -> 1
    Removal {}            -> -1
    IndividualSample {}   -> -1
    PopulationSample {..} -> fromIntegral $ numPeople popSampPeople
    StoppingTime {}       -> 0
    Extinction {}         -> 0
