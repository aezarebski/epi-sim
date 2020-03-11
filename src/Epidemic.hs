{-# LANGUAGE DeriveGeneric #-}

module Epidemic where

import qualified Data.Vector as V
import Data.List (nub)
import GHC.Generics (Generic)

type Time = Double

type Identifier = Integer

type Rate = Double

newtype Person =
  Person Identifier
  deriving (Show, Generic,Eq)

newtype People = People (V.Vector Person) deriving (Show,Eq)

-- | Predicate for wther there are any people
nullPeople :: People -> Bool
nullPeople (People persons) = V.null persons

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
  | OccurrenceEvent Time Person
  deriving (Show, Generic, Eq)

eventTime :: Event -> Time
eventTime e = case e of
  InfectionEvent time _ _ -> time
  RemovalEvent time _ -> time
  SamplingEvent time _ -> time
  OccurrenceEvent time _ -> time

instance Ord Event where
  e1 <= e2 = eventTime e1 <= eventTime e2

personsInEvent :: Event -> [Person]
personsInEvent e = case e of
  (InfectionEvent _ p1 p2) -> [p1,p2]
  (RemovalEvent _ p) -> [p]
  (SamplingEvent _ p) -> [p]
  (OccurrenceEvent _ p) -> [p]

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
    ((InfectionEvent _ infector infectee):es) ->
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
  _ -> False

-- | Predicate for whether a person was sampled in the given events
wasSampled :: [Event]
           -> Person
           -> Bool
wasSampled events person = case events of
  ((SamplingEvent _ sampledPerson):es) -> sampledPerson == person || wasSampled es person
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
    _:remainingEvents -> samplingEvent remainingEvents person
    _ -> error "person does not appear to have been sampled."


isOccurrence :: Event -> Bool
isOccurrence e = case e of
  OccurrenceEvent {} -> True
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
  | TTDeath Person Event
  | TTBirth Person Event (TransmissionTree, TransmissionTree)
  deriving (Show)

-- | A transmission tree of all the events starting from a given person
transmissionTree :: [Event] -> Person -> TransmissionTree
transmissionTree (e@(InfectionEvent _ p1 p2):es) person
  | p1 == person = TTBirth person e (transmissionTree es p1,transmissionTree es p2)
  | null es = TTUnresolved person
  | otherwise = transmissionTree es person
transmissionTree (e@(RemovalEvent _ p1):es) person
  | p1 == person = TTDeath p1 e
  | otherwise = transmissionTree es person
transmissionTree (e@(SamplingEvent _ p1):es) person
  | p1 == person = TTDeath p1 e
  | otherwise = transmissionTree es person
transmissionTree (e@(OccurrenceEvent _ p1):es) person
  | p1 == person = TTDeath p1 e
  | otherwise = transmissionTree es person
transmissionTree [] person = TTUnresolved person

-- | A predicate for whether there is a sampled leaf in the transmission tree
hasSampledLeaf :: TransmissionTree -> Bool
hasSampledLeaf t = case t of
  (TTUnresolved _) -> False
  (TTDeath _ (SamplingEvent _ _)) -> True
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
  _ -> error "ill-formed transmission tree"

sampleTreeEvents :: SampleTree -> [Event]
sampleTreeEvents sTree = case sTree of
  (STDeath e) -> [e]
  (STBirth e (s1,s2)) -> e:sampleTreeEvents s1 ++ sampleTreeEvents s2
