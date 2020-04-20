{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
module Epidemic where

import Data.Word
import Control.Monad
import qualified Data.Vector as V
import qualified Data.ByteString as B
import Data.ByteString.Internal (c2w)
import Data.Csv
import Data.List (nub)
import GHC.Generics (Generic)

type Time = Double

type Identifier = Integer

type Rate = Double

type Probability = Double

newtype Person =
  Person Identifier
  deriving (Show, Generic, Eq)

instance ToField Person where
  toField (Person n) = toField n

instance FromField Person where
  parseField f = Person <$> (parseField f :: Parser Identifier)

newtype People =
  People (V.Vector Person)
  deriving (Show, Eq)

instance ToField People where
  toField (People persons) =
    B.intercalate ":" $ V.toList $ V.map toField persons

instance FromField People where
  parseField f =
    (People . V.fromList) <$> (mapM parseField $ B.split (c2w ':') f)

-- | Predicate for wther there are any people
nullPeople :: People -> Bool
nullPeople (People persons) = V.null persons

-- | The number of people
numPeople :: People -> Int
numPeople (People persons) = V.length persons

-- | Add a person to a group of people
addPerson :: Person -> People -> People
addPerson person (People persons) = People $ V.cons person persons

-- | Remove a person from a group of people
removePerson :: Person -> People -> People
removePerson person (People persons) = People $ V.filter (/= person) persons

data Event
  = InfectionEvent Time Person Person -- infection time, infector, infectee
  | RemovalEvent Time Person
  | SamplingEvent Time Person
  | CatastropheEvent Time People
  | OccurrenceEvent Time Person
  | DisasterEvent Time People
  deriving (Show, Generic, Eq)

instance ToRecord Event where
  toRecord e =
    case e of
      (InfectionEvent time person1 person2) ->
        record ["infection", toField time, toField person1, toField person2]
      (RemovalEvent time person) ->
        record ["removal", toField time, toField person, "NA"]
      (SamplingEvent time person) ->
        record ["sampling", toField time, toField person, "NA"]
      (CatastropheEvent time people) ->
        record ["catastrophe", toField time, toField people, "NA"]
      (OccurrenceEvent time person) ->
        record ["occurrence", toField time, toField person, "NA"]
      (DisasterEvent time people) ->
        record ["disaster", toField time, toField people, "NA"]

et :: B.ByteString -> Record -> Bool
et bs r = (==bs) . head $ V.toList r

instance FromRecord Event where
  parseRecord r
    | et "infection" r =
      InfectionEvent <$> (r .! 1) <*> (Person <$> (r .! 2)) <*>
      (Person <$> (r .! 3))
    | et "removal" r = RemovalEvent <$> (r .! 1) <*> (Person <$> (r .! 2))
    | et "sampling" r = SamplingEvent <$> (r .! 1) <*> (Person <$> (r .! 2))
    | et "catastrophe" r = CatastropheEvent <$> (r .! 1) <*> (r .! 2)
    | et "occurrence" r = OccurrenceEvent <$> (r .! 1) <*> (Person <$> (r .! 2))
    | et "disaster" r = DisasterEvent <$> (r .! 1) <*> (r .! 2)
    | otherwise = undefined

eventTime :: Event -> Time
eventTime e = case e of
  InfectionEvent time _ _ -> time
  RemovalEvent time _ -> time
  SamplingEvent time _ -> time
  CatastropheEvent time _ -> time
  OccurrenceEvent time _ -> time
  DisasterEvent time _ -> time

-- | The first scheduled event after a given time.
firstScheduled :: Time                 -- ^ The given time
               -> [(Time,Probability)] -- ^ The information about all scheduled events
               -> Maybe (Time,Probability)
firstScheduled _ [] = Nothing
firstScheduled currTime (sched@(schedTime, _):scheduledEvents)
  | schedTime == currTime = Just sched
  | schedTime < currTime = firstScheduled currTime scheduledEvents
  | schedTime > currTime =
    let maybeFirstSched = firstScheduled currTime scheduledEvents
     in case maybeFirstSched of
          Nothing -> Just sched
          (Just (schedTime', _)) ->
            if schedTime < schedTime'
              then Just sched
              else maybeFirstSched

-- | Predicate for whether there is a scheduled event during an interval.
noScheduledEvent :: Time                 -- ^ Start time for interval
                 -> Time                 -- ^ End time for interval
                 -> [(Time,Probability)] -- ^ Information about all scheduled events
                 -> Bool
noScheduledEvent _ _ [] = True
noScheduledEvent a b ((shedTime, _):scheduledEvents) =
  not (a < shedTime && shedTime <= b) && noScheduledEvent a b scheduledEvents

instance Ord Event where
  e1 <= e2 = eventTime e1 <= eventTime e2

personsInEvent :: Event -> [Person]
personsInEvent e = case e of
  (InfectionEvent _ p1 p2) -> [p1,p2]
  (RemovalEvent _ p) -> [p]
  (SamplingEvent _ p) -> [p]
  (CatastropheEvent _ (People persons)) -> V.toList persons
  (OccurrenceEvent _ p) -> [p]
  (DisasterEvent _ (People persons)) -> V.toList persons

peopleInEvents :: [Event] -> People
peopleInEvents events =
  People . V.fromList . nub . concat $ map personsInEvent events


-- | Predicate for whether the first person infected the second in the given event
infected :: Person -- ^ Potential infector
         -> Person -- ^ Potential infectee
         -> Event  -- ^ Given event
         -> Bool
infected p1 p2 e =
  case e of
    (InfectionEvent _ infector infectee) -> infector == p1 && infectee == p2
    _ -> False


-- | The people infected by a particular person in a list of events.
infectedBy :: Person  -- ^ Potential infector
           -> [Event] -- ^ Events
           -> People
infectedBy person events =
  case events of
    [] -> People V.empty
    (InfectionEvent _ infector infectee :es) ->
      if infector == person
        then addPerson infectee $ infectedBy person es
        else infectedBy person es
    (_:es) -> infectedBy person es


-- | Predicate for whether a person or one of their descendents satisfies a
-- predicate
hasDescendentWhich :: [Event]
                   -> (Person -> Bool)
                   -> Person
                   -> Bool
hasDescendentWhich events predicate person =
  predicate person ||
  any (hasDescendentWhich events predicate) (V.toList descendents)
  where
    (People descendents) = infectedBy person events

hasSampledDescendent :: [Event] -> Person -> Bool
hasSampledDescendent events = hasDescendentWhich events (wasSampled events)

isInfection :: Event -> Bool
isInfection e =
  case e of
    InfectionEvent {} -> True
    _ -> False

isSampling :: Event -> Bool
isSampling e = case e of
  SamplingEvent {} -> True
  CatastropheEvent {} -> True
  _ -> False

-- | Predicate for whether a person was sampled in the given events
wasSampled :: [Event] -- ^ The given events
           -> Person  -- ^ The person of interest
           -> Bool
wasSampled events person =
  case events of
    (SamplingEvent _ sampledPerson:es) ->
      sampledPerson == person || wasSampled es person
    (CatastropheEvent _ (People sampledPeople):es) ->
      person `V.elem` sampledPeople || wasSampled es person
    (_:es) -> wasSampled es person
    [] -> False

-- | Return the sampling event of a person who was sampled.
samplingEvent :: [Event] -> Person -> Event
samplingEvent events person =
  case events of
    (se@(SamplingEvent _ sampledPerson):remainingEvents) ->
      if sampledPerson == person
        then se
        else samplingEvent remainingEvents person
    (se@(CatastropheEvent _ (People sampledPeople)):remainingEvents) ->
      if person `V.elem` sampledPeople
        then se
        else samplingEvent remainingEvents person
    _:remainingEvents -> samplingEvent remainingEvents person
    _ -> error "person does not appear to have been sampled."


isOccurrence :: Event -> Bool
isOccurrence e = case e of
  OccurrenceEvent {} -> True
  _ -> False

isDisaster :: Event -> Bool
isDisaster e = case e of
  DisasterEvent {} -> True
  _ -> False

class ModelParameters a where
  rNaught :: a -> Double
  eventRate :: a -> Double

class Population a where
  susceptiblePeople :: a -> Maybe People
  infectiousPeople :: a -> Maybe People
  removedPeople :: a -> Maybe People
  isInfected :: a -> Bool


data TransmissionTree
  = TTUnresolved Person
  | TTDeath People Event
  | TTBirth Person Event (TransmissionTree, TransmissionTree)
  deriving (Show)

-- | A transmission tree of all the events starting from a given person
transmissionTree :: [Event] -> Person -> TransmissionTree
transmissionTree (e@(InfectionEvent _ p1 p2):es) person
  | p1 == person = TTBirth person e (transmissionTree es p1,transmissionTree es p2)
  | null es = TTUnresolved person
  | otherwise = transmissionTree es person
transmissionTree (e@(RemovalEvent _ p1):es) person
  | p1 == person = TTDeath (peopleInEvents [e]) e
  | otherwise = transmissionTree es person
transmissionTree (e@(SamplingEvent _ p1):es) person
  | p1 == person = TTDeath (peopleInEvents [e]) e
  | otherwise = transmissionTree es person
transmissionTree (e@(CatastropheEvent _ (People people)):es) person
  | person `V.elem` people = TTDeath (People people) e
  | otherwise = transmissionTree es person
transmissionTree (e@(OccurrenceEvent _ p1):es) person
  | p1 == person = TTDeath (peopleInEvents [e]) e
  | otherwise = transmissionTree es person
transmissionTree (e@(DisasterEvent _ (People people)):es) person
  | person `V.elem` people = TTDeath (People people) e
  | otherwise = transmissionTree es person
transmissionTree [] person = TTUnresolved person

-- | A predicate for whether there is a sampled leaf in the transmission tree
hasSampledLeaf :: TransmissionTree -> Bool
hasSampledLeaf t = case t of
  (TTUnresolved _) -> False
  (TTDeath _ (SamplingEvent _ _)) -> True
  (TTDeath _ (CatastropheEvent _ _)) -> True
  (TTDeath _ _) -> False
  (TTBirth _ _ (t1,t2)) -> hasSampledLeaf t1 || hasSampledLeaf t2

data SampleTree
  = STBirth Event (SampleTree,SampleTree)
  | STDeath Event
  deriving (Show)

-- | A transmission tree with all non-sampling leaves removed
sampleTree :: TransmissionTree -> SampleTree
sampleTree transTree = case transTree of
  (TTBirth _ e@InfectionEvent {} (t1,t2))
    | hasSampledLeaf t1 && hasSampledLeaf t2 -> STBirth e (sampleTree t1,sampleTree t2)
    | hasSampledLeaf t1 -> sampleTree t1
    | hasSampledLeaf t2 -> sampleTree t2
  (TTDeath _ e@(SamplingEvent _ _)) -> STDeath e
  (TTDeath _ e@(CatastropheEvent _ _)) -> STDeath e
  _ -> error "ill-formed transmission tree"

-- | Recurse through the tree and extract all birth and death events.
sampleTreeEvents' :: SampleTree -> [Event]
sampleTreeEvents' sTree =
  case sTree of
    (STDeath e) -> [e]
    (STBirth e (s1, s2)) -> e : sampleTreeEvents s1 ++ sampleTreeEvents s2

-- | The unique events in a sample tree.
sampleTreeEvents :: SampleTree -> [Event]
sampleTreeEvents = nub . sampleTreeEvents'
