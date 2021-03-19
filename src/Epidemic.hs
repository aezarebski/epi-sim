{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

module Epidemic where

import Control.Monad
import qualified Data.ByteString as B
import Data.ByteString.Internal (c2w)
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
import Epidemic.Types.Time
  ( AbsoluteTime(..)
  , Timed(..)
  , diracDeltaValue
  , nextTime
  )
import GHC.Generics (Generic)
import System.Random.MWC

-- | The number of people added or removed in an event.
eventPopDelta :: EpidemicEvent -> Integer
eventPopDelta e =
  case e of
    Infection {} -> 1
    Removal {} -> -1
    IndividualSample {} -> -1
    PopulationSample {..} -> fromIntegral $ numPeople popSampPeople
    StoppingTime {} -> 0
    Extinction {} -> 0 -- ^ already represented in previous event.

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

-- | A list of the people involved in an 'EpidemicEvent'.
personsInEvent :: EpidemicEvent -> [Person]
personsInEvent e =
  case e of
    Infection _ p1 p2 -> [p1, p2]
    Removal _ p -> [p]
    (IndividualSample {..}) -> [indSampPerson]
    (PopulationSample {..}) -> V.toList personVec
      where (People personVec) = popSampPeople
    Extinction {} -> []
    StoppingTime {} -> []

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
allEvents simRandEvent@(SimulationRandEvent randEvent) modelParams maxTime maybePopPredicate (SimulationState (currTime, currEvents, currPop, currId)) gen =
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
               else return $
                    SimulationState
                      ( maxTime
                      , (StoppingTime maxTime) : currEvents
                      , currPop
                      , currId)
           else return $
                SimulationState
                  ( currTime
                  , (Extinction currTime) : currEvents
                  , currPop
                  , currId)
    else return TerminatedSimulation
