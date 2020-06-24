{-# LANGUAGE RecordWildCards #-}

module Epidemic.BirthDeathSamplingCatastropheOccurrence
  ( configuration
  , allEvents
  , observedEvents
  ) where

import Epidemic.Types.Population
import Epidemic.Types.Events
import Epidemic.Types.Parameter
import Data.Maybe (fromJust)
import qualified Data.Vector as V
import qualified Data.Vector.Generic as G
import System.Random.MWC
import System.Random.MWC.Distributions (categorical, exponential, bernoulli)

import Epidemic
import Epidemic.Utility


data BDSCOParameters
  -- | birth rate, death rate, sampling rate, catastrophe probability and occurrence rate.
  = BDSCOParameters Rate Rate Rate (Timed Probability) Rate

instance ModelParameters BDSCOParameters where
  rNaught (BDSCOParameters birthRate deathRate samplingRate _ occurrenceRate) _ =
    Just $ birthRate / (deathRate + samplingRate + occurrenceRate)
  eventRate (BDSCOParameters birthRate deathRate samplingRate _ occurrenceRate) _ =
    Just $ birthRate + deathRate + samplingRate + occurrenceRate
  birthProb (BDSCOParameters birthRate deathRate samplingRate _ occurrenceRate) _ =
    Just $ birthRate / (birthRate + deathRate + samplingRate + occurrenceRate)

newtype BDSCOPopulation =
  BDSCOPopulation People
  deriving (Show)

instance Population BDSCOPopulation where
  susceptiblePeople _ = Nothing
  infectiousPeople (BDSCOPopulation people) = Just people
  removedPeople _ = Nothing
  isInfected (BDSCOPopulation (People people)) = not $ V.null people

-- | Configuration of a birth-death-sampling-occurrence simulation
configuration :: Time                                       -- ^ Duration of the simulation
              -> (Rate,Rate,Rate,[(Time,Probability)],Rate) -- ^ Birth, Death, Sampling, Catastrophe probability and Occurrence rates
              -> Maybe (SimulationConfiguration BDSCOParameters BDSCOPopulation)
configuration maxTime (birthRate, deathRate, samplingRate, catastropheProb, occurrenceRate) = do
  catastropheTimedProb <- asTimed catastropheProb
  let bdscoParams =
        BDSCOParameters
          birthRate
          deathRate
          samplingRate
          catastropheTimedProb
          occurrenceRate
      (seedPerson, newId) = newPerson initialIdentifier
      bdscoPop = BDSCOPopulation (People $ V.singleton seedPerson)
   in Just $ SimulationConfiguration bdscoParams bdscoPop newId maxTime

-- | Return a random event from the BDSCO-process given the current state of the process.
randomBdscoEvent ::
     BDSCOParameters  -- ^ Parameters of the process
  -> Time             -- ^ The current time within the process
  -> BDSCOPopulation  -- ^ The current state of the populaion
  -> Integer       -- ^ The current state of the identifier generator
  -> GenIO            -- ^ The current state of the PRNG
  -> IO (Time, EpidemicEvent, BDSCOPopulation, Integer)
randomBdscoEvent params@(BDSCOParameters br dr sr catastInfo occr) currTime currPop@(BDSCOPopulation (People people)) currId gen =
  let netEventRate = fromJust $ eventRate params currTime
      eventWeights = V.fromList [br, dr, sr, occr]
   in
    do delay <- exponential (fromIntegral (V.length people) * netEventRate) gen
       nextEventTime <- pure $ currTime + delay
       if noScheduledEvent currTime nextEventTime catastInfo
         then do eventIx <- categorical eventWeights gen
                 (selectedPerson, unselectedPeople) <- randomPerson people gen
                 return $ case eventIx of
                   0 -> let (birthedPerson, newId) = newPerson currId
                            event = Infection nextEventTime selectedPerson birthedPerson
                     in ( nextEventTime
                        , event
                        , BDSCOPopulation (People $ V.cons birthedPerson people)
                        , newId)
                   1 -> (nextEventTime, Removal nextEventTime selectedPerson, BDSCOPopulation (People unselectedPeople), currId)
                   2 -> (nextEventTime, Sampling nextEventTime selectedPerson, BDSCOPopulation (People unselectedPeople), currId)
                   3 -> (nextEventTime, Occurrence nextEventTime selectedPerson, BDSCOPopulation (People unselectedPeople), currId)
                   _ -> error "no birth, death, sampling, occurrence event selected."
         else let (Just (catastTime,catastProb)) = firstScheduled currTime catastInfo
               in do (catastEvent,postCatastPop) <- randomCatastropheEvent (catastTime,catastProb) currPop gen
                     return (catastTime,catastEvent,postCatastPop,currId)


-- | Return a randomly sampled Catastrophe event
randomCatastropheEvent :: (Time,Probability) -- ^ Time and probability of sampling in the catastrophe
                       -> BDSCOPopulation    -- ^ The state of the population prior to the catastrophe
                       -> GenIO
                       -> IO (EpidemicEvent,BDSCOPopulation)
randomCatastropheEvent (catastTime, rhoProb) (BDSCOPopulation (People currPeople)) gen = do
  rhoBernoullis <- G.replicateM (V.length currPeople) (bernoulli rhoProb gen)
  let filterZip predicate a b = fst . V.unzip . V.filter predicate $ V.zip a b
      sampledPeople = filterZip snd currPeople rhoBernoullis
      unsampledPeople = filterZip (not . snd) currPeople rhoBernoullis
   in return
        ( Catastrophe catastTime (People sampledPeople)
        , BDSCOPopulation (People unsampledPeople))

allEvents ::
     BDSCOParameters
  -> Time
  -> (Time, [EpidemicEvent], BDSCOPopulation, Integer)
  -> GenIO
  -> IO (Time, [EpidemicEvent], BDSCOPopulation, Integer)
allEvents rates maxTime currState@(currTime, currEvents, currPop, currId) gen =
  if isInfected currPop
    then do
      (newTime, event, newPop, newId) <-
        randomBdscoEvent rates currTime currPop currId gen
      if newTime < maxTime
        then allEvents
               rates
               maxTime
               (newTime, event : currEvents, newPop, newId)
               gen
        else return currState
    else return currState


-- | Just the observable events from a list of all the events in a simulation.
observedEvents :: [EpidemicEvent] -- ^ All of the simulation events
                    -> [EpidemicEvent]
observedEvents events = sort $ occurrenceEvents ++ sampleTreeEvents''
  where
    occurrenceEvents = filter isOccurrence events
    sampleTreeEvents'' =
      sampleTreeEvents . sampleTree $ transmissionTree events (Person 1)
