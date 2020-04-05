{-# LANGUAGE RecordWildCards #-}

module Epidemic.BirthDeathSamplingCatastropheOccurrence
  ( simulation
  , configuration
  , allEvents
  , observedEvents
  ) where

import Data.List (minimumBy)
import qualified Data.Vector as V
import qualified Data.Vector.Generic as G
import System.Random.MWC
import System.Random.MWC.Distributions (categorical, exponential, bernoulli)

import Epidemic
import Epidemic.Utility


data BDSCOParameters
  -- | birth rate, death rate, sampling rate, catastrophe probability and occurrence rate.
  = BDSCOParameters Rate Rate Rate [(Time,Probability)] Rate

instance ModelParameters BDSCOParameters where
  rNaught (BDSCOParameters birthRate deathRate samplingRate _ occurrenceRate) =
    birthRate / (deathRate + samplingRate + occurrenceRate)
  eventRate (BDSCOParameters birthRate deathRate samplingRate _ occurrenceRate) =
    birthRate + deathRate + samplingRate + occurrenceRate

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
            -> SimulationConfiguration BDSCOParameters BDSCOPopulation
configuration maxTime (birthRate, deathRate, samplingRate, catastropheProb, occurrenceRate) =
  let bdscoParams =
        BDSCOParameters
          birthRate
          deathRate
          samplingRate
          catastropheProb
          occurrenceRate
      (seedPerson, newId) = newPerson initialIdentifier
      bdscoPop = BDSCOPopulation (People $ V.singleton seedPerson)
   in SimulationConfiguration bdscoParams bdscoPop newId maxTime

-- | Return a random event from the BDSCO-process given the current state of the process.
randomBdscoEvent ::
     BDSCOParameters  -- ^ Parameters of the process
  -> Time             -- ^ The current time within the process
  -> BDSCOPopulation  -- ^ The current state of the populaion
  -> Identifier       -- ^ The current state of the identifier generator
  -> GenIO            -- ^ The current state of the PRNG
  -> IO (Time, Event, BDSCOPopulation, Identifier)
randomBdscoEvent params@(BDSCOParameters br dr sr catastInfo occr) currTime currPop@(BDSCOPopulation (People currPeople)) currId gen =
  let netEventRate = eventRate params
      eventWeights = V.fromList [br, dr, sr, occr]
   in
    do delay <- exponential (fromIntegral (V.length currPeople) * netEventRate) gen
       nextTime <- pure $ currTime + delay
       if noScheduledEvent currTime nextTime catastInfo
         then do eventIx <- categorical eventWeights gen
                 (selectedPerson, unselectedPeople) <- randomPerson currPeople gen
                 return $ case eventIx of
                   0 -> let (birthedPerson, newId) = newPerson currId
                            event = InfectionEvent nextTime selectedPerson birthedPerson
                     in ( nextTime
                        , event
                        , BDSCOPopulation (People $ V.cons birthedPerson currPeople)
                        , newId)
                   1 -> (nextTime, RemovalEvent nextTime selectedPerson, BDSCOPopulation (People unselectedPeople), currId)
                   2 -> (nextTime, SamplingEvent nextTime selectedPerson, BDSCOPopulation (People unselectedPeople), currId)
                   3 -> (nextTime, OccurrenceEvent nextTime selectedPerson, BDSCOPopulation (People unselectedPeople), currId)
                   _ -> error "no birth, death, sampling, occurrence event selected."
         else let (Just (catastTime,catastProb)) = firstScheduled currTime catastInfo
               in do (catastEvent,postCatastPop) <- randomCatastropheEvent (catastTime,catastProb) currPop gen
                     return (catastTime,catastEvent,postCatastPop,currId)


-- | Return a randomly sampled Catastrophe event
randomCatastropheEvent :: (Time,Probability) -- ^ Time and probability of sampling in the catastrophe
                       -> BDSCOPopulation    -- ^ The state of the population prior to the catastrophe
                       -> GenIO
                       -> IO (Event,BDSCOPopulation)
randomCatastropheEvent (catastTime, rhoProb) (BDSCOPopulation (People currPeople)) gen = do
  rhoBernoullis <- G.replicateM (V.length currPeople) (bernoulli rhoProb gen)
  let filterZip predicate a b = fst . V.unzip . V.filter predicate $ V.zip a b
      sampledPeople = filterZip snd currPeople rhoBernoullis
      unsampledPeople = filterZip (not . snd) currPeople rhoBernoullis
   in return
        ( CatastropheEvent catastTime (People sampledPeople)
        , BDSCOPopulation (People unsampledPeople))

allEvents ::
     BDSCOParameters
  -> Time
  -> (Time, [Event], BDSCOPopulation, Identifier)
  -> GenIO
  -> IO (Time, [Event], BDSCOPopulation, Identifier)
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

-- | Run a simulation described by a configuration object.
simulation :: SimulationConfiguration BDSCOParameters BDSCOPopulation
                                       -> IO [Event]
simulation SimulationConfiguration {..} = do
  gen <- System.Random.MWC.create :: IO GenIO
  (_, events, _, _) <-
    allEvents rates timeLimit (0, [], population, newIdentifier) gen
  return $ sort events

-- | Just the observable events from a list of all the events in a simulation.
observedEvents :: [Event] -- ^ All of the simulation events
                    -> [Event]
observedEvents events = sort $ occurrenceEvents ++ sampleTreeEvents''
  where
    occurrenceEvents = filter isOccurrence events
    sampleTreeEvents'' =
      sampleTreeEvents . sampleTree $ transmissionTree events (Person 1)
