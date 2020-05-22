{-# LANGUAGE RecordWildCards #-}

module Epidemic.BDSCOD
  ( configuration
  , allEvents
  , observedEvents
  ) where

import Epidemic.Types
import Data.Maybe (fromJust)
import qualified Data.Vector as V
import qualified Data.Vector.Generic as G
import System.Random.MWC
import System.Random.MWC.Distributions (bernoulli, categorical, exponential)

import Epidemic
import Epidemic.Utility


data BDSCODParameters
  -- | birth rate, death rate, sampling rate, catastrophe specification, occurrence rate and disaster specification
  = BDSCODParameters Rate Rate Rate (Timed Probability) Rate (Timed Probability)

instance ModelParameters BDSCODParameters where
  rNaught (BDSCODParameters birthRate deathRate samplingRate _ occurrenceRate _) _ =
    Just $ birthRate / (deathRate + samplingRate + occurrenceRate)
  eventRate (BDSCODParameters birthRate deathRate samplingRate _ occurrenceRate _) _ =
    Just $ birthRate + deathRate + samplingRate + occurrenceRate
  birthProb (BDSCODParameters birthRate deathRate samplingRate _ occurrenceRate _) _ =
    Just $ birthRate / (birthRate + deathRate + samplingRate + occurrenceRate)

newtype BDSCODPopulation =
  BDSCODPopulation People
  deriving (Show)

instance Population BDSCODPopulation where
  susceptiblePeople _ = Nothing
  infectiousPeople (BDSCODPopulation people) = Just people
  removedPeople _ = Nothing
  isInfected (BDSCODPopulation (People people)) = not $ V.null people

-- | Configuration of a birth-death-sampling-occurrence simulation
configuration :: Time                                                            -- ^ Duration of the simulation
              -> (Rate,Rate,Rate,[(Time,Probability)],Rate,[(Time,Probability)]) -- ^ Birth, Death, Sampling, Catastrophe probability and Occurrence rates
              -> Maybe (SimulationConfiguration BDSCODParameters BDSCODPopulation)
configuration maxTime (birthRate, deathRate, samplingRate, catastropheSpec, occurrenceRate, disasterSpec) =
  do catastropheSpec' <- asTimed catastropheSpec
     disasterSpec' <- asTimed disasterSpec
     let bdscodParams =
           BDSCODParameters
           birthRate
           deathRate
           samplingRate
           catastropheSpec'
           occurrenceRate
           disasterSpec'
         (seedPerson, newId) = newPerson initialIdentifier
         bdscodPop = BDSCODPopulation (People $ V.singleton seedPerson)
       in return $ SimulationConfiguration bdscodParams bdscodPop newId maxTime

-- | Return a random event from the BDSCOD-process given the current state of the process.
randomEvent :: BDSCODParameters  -- ^ Parameters of the process
            -> Time              -- ^ The current time within the process
            -> BDSCODPopulation  -- ^ The current state of the populaion
            -> Identifier        -- ^ The current state of the identifier generator
            -> GenIO             -- ^ The current state of the PRNG
            -> IO (Time, Event, BDSCODPopulation, Identifier)
randomEvent params@(BDSCODParameters br dr sr catastInfo occr disastInfo) currTime currPop@(BDSCODPopulation (People currPeople)) currId gen =
  let netEventRate = fromJust $ eventRate params currTime
      eventWeights = V.fromList [br, dr, sr, occr]
   in do delay <- exponential (fromIntegral (V.length currPeople) * netEventRate) gen
         nextTime <- pure $ currTime + delay
         if noScheduledEvent currTime nextTime (joinTimed catastInfo disastInfo)
           then do eventIx <- categorical eventWeights gen
                   (selectedPerson, unselectedPeople) <- randomPerson currPeople gen
                   return $ case eventIx of
                     0 -> let (birthedPerson, newId) = newPerson currId
                              event = InfectionEvent nextTime selectedPerson birthedPerson
                       in ( nextTime
                          , event
                          , BDSCODPopulation (People $ V.cons birthedPerson currPeople)
                          , newId)
                     1 -> (nextTime, RemovalEvent nextTime selectedPerson, BDSCODPopulation (People unselectedPeople), currId)
                     2 -> (nextTime, SamplingEvent nextTime selectedPerson, BDSCODPopulation (People unselectedPeople), currId)
                     3 -> (nextTime, OccurrenceEvent nextTime selectedPerson, BDSCODPopulation (People unselectedPeople), currId)
                     _ -> error "no birth, death, sampling, occurrence event selected."

           else if noScheduledEvent currTime nextTime catastInfo
                  then let (Just (disastTime,disastProb)) = firstScheduled currTime disastInfo
                        in do (disastEvent,postDisastPop) <- randomDisasterEvent (disastTime,disastProb) currPop gen
                              return (disastTime,disastEvent,postDisastPop,currId)
                else if noScheduledEvent currTime nextTime disastInfo
                        then let (Just (catastTime,catastProb)) = firstScheduled currTime catastInfo
                              in do (catastEvent,postCatastPop) <- randomCatastropheEvent (catastTime,catastProb) currPop gen
                                    return (catastTime,catastEvent,postCatastPop,currId)
                     else let (Just (catastTime,catastProb)) = firstScheduled currTime catastInfo
                              (Just (disastTime,disastProb)) = firstScheduled currTime disastInfo
                           in do (scheduledEvent,postEventPop) <- if catastTime < disastTime then
                                                                    randomCatastropheEvent (catastTime,catastProb) currPop gen else
                                                                    randomDisasterEvent (disastTime,disastProb) currPop gen
                                 return (min catastTime disastTime,scheduledEvent,postEventPop,currId)


-- | Return a randomly sampled Catastrophe event
randomCatastropheEvent :: (Time,Probability) -- ^ Time and probability of sampling in the catastrophe
                       -> BDSCODPopulation    -- ^ The state of the population prior to the catastrophe
                       -> GenIO
                       -> IO (Event,BDSCODPopulation)
randomCatastropheEvent (catastTime, rhoProb) (BDSCODPopulation (People currPeople)) gen = do
  rhoBernoullis <- G.replicateM (V.length currPeople) (bernoulli rhoProb gen)
  let filterZip predicate a b = fst . V.unzip . V.filter predicate $ V.zip a b
      sampledPeople = filterZip snd currPeople rhoBernoullis
      unsampledPeople = filterZip (not . snd) currPeople rhoBernoullis
   in return
        ( CatastropheEvent catastTime (People sampledPeople)
        , BDSCODPopulation (People unsampledPeople))

-- | Return a randomly sampled Disaster event
randomDisasterEvent :: (Time,Probability) -- ^ Time and probability of sampling in the disaster
                    -> BDSCODPopulation    -- ^ The state of the population prior to the disaster
                    -> GenIO
                    -> IO (Event,BDSCODPopulation)
randomDisasterEvent (disastTime, nuProb) (BDSCODPopulation (People currPeople)) gen = do
  nuBernoullis <- G.replicateM (V.length currPeople) (bernoulli nuProb gen)
  let filterZip predicate a b = fst . V.unzip . V.filter predicate $ V.zip a b
      sampledPeople = filterZip snd currPeople nuBernoullis
      unsampledPeople = filterZip (not . snd) currPeople nuBernoullis
   in return
        ( DisasterEvent disastTime (People sampledPeople)
        , BDSCODPopulation (People unsampledPeople))

allEvents ::
     BDSCODParameters
  -> Time
  -> (Time, [Event], BDSCODPopulation, Identifier)
  -> GenIO
  -> IO (Time, [Event], BDSCODPopulation, Identifier)
allEvents rates maxTime currState@(currTime, currEvents, currPop, currId) gen =
  if isInfected currPop
    then do
      (newTime, event, newPop, newId) <-
        randomEvent rates currTime currPop currId gen
      if newTime < maxTime
        then allEvents
               rates
               maxTime
               (newTime, event : currEvents, newPop, newId)
               gen
        else return currState
    else return currState


-- | Just the observable events from a list of all the events in a simulation.
observedEvents :: [Event] -- ^ All of the simulation events
               -> [Event]
observedEvents [] = []
observedEvents events = sort $ occurrenceEvents ++ disasterEvents ++ sampleTreeEvents''
  where
    occurrenceEvents = filter isOccurrence events
    disasterEvents = filter isDisaster events
    sampleTreeEvents'' =
      sampleTreeEvents . sampleTree $ transmissionTree events (Person 1)
