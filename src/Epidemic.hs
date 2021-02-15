{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
module Epidemic where

import Control.Monad
import qualified Data.ByteString as B
import Data.ByteString.Internal (c2w)
import Data.Csv
import Data.List (nub)
import Data.Maybe (fromJust, isJust, isNothing)
import qualified Data.Vector as V
import Data.Word
import Epidemic.Types.Events
import Epidemic.Types.Parameter
import Epidemic.Types.Population
import Epidemic.Types.Simulation
  ( SimulationConfiguration(..)
  , SimulationRandEvent(..)
  , SimulationState(..)
  )
import GHC.Generics (Generic)
import System.Random.MWC

-- | The number of people added or removed in an event.
eventPopDelta :: EpidemicEvent -> Integer
eventPopDelta e =
  case e of
    Infection {} -> 1
    Removal _ _ -> -1
    Sampling _ _ -> -1
    Catastrophe _ people -> fromIntegral $ numPeople people
    Occurrence _ _ -> -1
    Disaster _ people -> fromIntegral $ numPeople people
    Extinction {} -> undefined
    StoppingTime {} -> undefined

-- | The first scheduled event after a given time.
firstScheduled ::
     AbsoluteTime -- ^ The given time
  -> Timed Probability -- ^ The information about all scheduled events
  -> Maybe (AbsoluteTime, Probability)
firstScheduled time timedProb = do
  time' <- nextTime timedProb time
  prob' <- diracDeltaValue timedProb time'
  return (time', prob')

-- | Predicate for whether there is a scheduled event during an interval.
noScheduledEvent ::
     AbsoluteTime -- ^ Start time for interval
  -> AbsoluteTime -- ^ End time for interval
  -> Timed Probability -- ^ Information about all scheduled events
  -> Bool
noScheduledEvent _ _ (Timed []) = True
noScheduledEvent a b (Timed ((shedTime, _):scheduledEvents)) =
  not (a < shedTime && shedTime <= b) &&
  noScheduledEvent a b (Timed scheduledEvents)

personsInEvent :: EpidemicEvent -> [Person]
personsInEvent e =
  case e of
    (Infection _ p1 p2) -> [p1, p2]
    (Removal _ p) -> [p]
    (Sampling _ p) -> [p]
    (Catastrophe _ (People persons)) -> V.toList persons
    (Occurrence _ p) -> [p]
    (Disaster _ (People persons)) -> V.toList persons
    Extinction {} -> undefined
    StoppingTime {} -> undefined

peopleInEvents :: [EpidemicEvent] -> People
peopleInEvents events =
  People . V.fromList . nub . concat $ map personsInEvent events

-- | Predicate for whether the first person infected the second in the given event
infected ::
     Person -- ^ Potential infector
  -> Person -- ^ Potential infectee
  -> EpidemicEvent -- ^ Given event
  -> Bool
infected p1 p2 e =
  case e of
    (Infection _ infector infectee) -> infector == p1 && infectee == p2
    _ -> False

-- | The people infected by a particular person in a list of events.
infectedBy ::
     Person -- ^ Potential infector
  -> [EpidemicEvent] -- ^ Events
  -> People
infectedBy person events =
  case events of
    [] -> People V.empty
    (Infection _ infector infectee:es) ->
      if infector == person
        then addPerson infectee $ infectedBy person es
        else infectedBy person es
    (_:es) -> infectedBy person es

-- | Predicate for whether a person or one of their descendents satisfies a
-- predicate
hasDescendentWhich :: [EpidemicEvent] -> (Person -> Bool) -> Person -> Bool
hasDescendentWhich events predicate person =
  predicate person ||
  any (hasDescendentWhich events predicate) (V.toList descendents)
  where
    (People descendents) = infectedBy person events

hasSampledDescendent :: [EpidemicEvent] -> Person -> Bool
hasSampledDescendent events = hasDescendentWhich events (wasSampled events)

-- | Predicate for whether a person was sampled in the given events
wasSampled ::
     [EpidemicEvent] -- ^ The given events
  -> Person -- ^ The person of interest
  -> Bool
wasSampled events person =
  case events of
    (Sampling _ sampledPerson:es) ->
      sampledPerson == person || wasSampled es person
    (Catastrophe _ (People sampledPeople):es) ->
      person `V.elem` sampledPeople || wasSampled es person
    (_:es) -> wasSampled es person
    [] -> False

-- | Return the sampling event of a person who was sampled.
samplingEvent :: [EpidemicEvent] -> Person -> EpidemicEvent
samplingEvent events person =
  case events of
    (se@(Sampling _ sampledPerson):remainingEvents) ->
      if sampledPerson == person
        then se
        else samplingEvent remainingEvents person
    (se@(Catastrophe _ (People sampledPeople)):remainingEvents) ->
      if person `V.elem` sampledPeople
        then se
        else samplingEvent remainingEvents person
    _:remainingEvents -> samplingEvent remainingEvents person
    _ -> error "person does not appear to have been sampled."

data TransmissionTree
  = TTUnresolved Person
  | TTDeath People EpidemicEvent
  | TTBirth Person EpidemicEvent (TransmissionTree, TransmissionTree)
  deriving (Show)

-- | A transmission tree of all the events starting from a given person
transmissionTree :: [EpidemicEvent] -> Person -> TransmissionTree
transmissionTree (e@(Infection _ p1 p2):es) person
  | p1 == person = TTBirth person e (transmissionTree es p1,transmissionTree es p2)
  | null es = TTUnresolved person
  | otherwise = transmissionTree es person
transmissionTree (e@(Removal _ p1):es) person
  | p1 == person = TTDeath (peopleInEvents [e]) e
  | otherwise = transmissionTree es person
transmissionTree (e@(Sampling _ p1):es) person
  | p1 == person = TTDeath (peopleInEvents [e]) e
  | otherwise = transmissionTree es person
transmissionTree (e@(Catastrophe _ (People people)):es) person
  | person `V.elem` people = TTDeath (People people) e
  | otherwise = transmissionTree es person
transmissionTree (e@(Occurrence _ p1):es) person
  | p1 == person = TTDeath (peopleInEvents [e]) e
  | otherwise = transmissionTree es person
transmissionTree (e@(Disaster _ (People people)):es) person
  | person `V.elem` people = TTDeath (People people) e
  | otherwise = transmissionTree es person
transmissionTree [] person = TTUnresolved person
transmissionTree ((Extinction _):_) _ = undefined
transmissionTree ((StoppingTime _):_) _ = undefined

-- | A predicate for whether there is a sampled leaf in the transmission tree
hasSampledLeaf :: TransmissionTree -> Bool
hasSampledLeaf t = case t of
  (TTUnresolved _) -> False
  (TTDeath _ (Sampling _ _)) -> True
  (TTDeath _ (Catastrophe _ _)) -> True
  (TTDeath _ _) -> False
  (TTBirth _ _ (t1,t2)) -> hasSampledLeaf t1 || hasSampledLeaf t2

data SampleTree
  = STBirth EpidemicEvent (SampleTree,SampleTree)
  | STDeath EpidemicEvent
  deriving (Show)

-- | A transmission tree with all non-sampling leaves removed
sampleTree :: TransmissionTree -> SampleTree
sampleTree transTree = case transTree of
  (TTBirth _ e@Infection {} (t1,t2))
    | hasSampledLeaf t1 && hasSampledLeaf t2 -> STBirth e (sampleTree t1,sampleTree t2)
    | hasSampledLeaf t1 -> sampleTree t1
    | hasSampledLeaf t2 -> sampleTree t2
  (TTDeath _ e@(Sampling _ _)) -> STDeath e
  (TTDeath _ e@(Catastrophe _ _)) -> STDeath e
  _ -> error "ill-formed transmission tree"

-- | Recurse through the tree and extract all birth and death events.
sampleTreeEvents' :: SampleTree -> [EpidemicEvent]
sampleTreeEvents' sTree =
  case sTree of
    (STDeath e) -> [e]
    (STBirth e (s1, s2)) -> e : sampleTreeEvents s1 ++ sampleTreeEvents s2

-- | The unique events in a sample tree.
sampleTreeEvents :: SampleTree -> [EpidemicEvent]
sampleTreeEvents = nub . sampleTreeEvents'

-- | Run the simulation and return a @SimulationState@ which holds the history
-- of the simulation.
allEvents ::
     (ModelParameters a b, Population b)
  => SimulationRandEvent a b
  -> a
  -> AbsoluteTime
  -> Maybe (b -> Bool) -- ^ predicate for a valid population
  -> SimulationState b
  -> GenIO
  -> IO (SimulationState b)
allEvents _ _ _ _ TerminatedSimulation _ = return TerminatedSimulation
allEvents simRandEvent@(SimulationRandEvent randEvent) modelParams maxTime maybePopPredicate currState@(SimulationState (currTime, currEvents, currPop, currId)) gen =
  if isNothing maybePopPredicate ||
     (isJust maybePopPredicate && fromJust maybePopPredicate currPop)
    then if isInfected currPop
           then do
             (newTime, event, newPop, newId) <-
               randEvent modelParams currTime currPop currId gen
             if newTime < maxTime
               then allEvents
                      simRandEvent
                      modelParams
                      maxTime
                      maybePopPredicate
                      (SimulationState
                         (newTime, event : currEvents, newPop, newId))
                      gen
               else return currState
           else return currState
    else return TerminatedSimulation
