{-# LANGUAGE RecordWildCards #-}

module Epidemic.BirthDeathSamplingOccurrence
  ( configuration
  , allEvents
  , observedEvents
  ) where

import Epidemic.Types
import Data.Maybe (fromJust)
import qualified Data.Vector as V
import System.Random.MWC
import System.Random.MWC.Distributions (categorical, exponential)

import Epidemic
import Epidemic.Utility

data BDSORates =
  BDSORates Rate Rate Rate Rate

instance ModelParameters BDSORates where
  rNaught (BDSORates birthRate deathRate samplingRate occurrenceRate) _ =
    Just $ birthRate / (deathRate + samplingRate + occurrenceRate)
  eventRate (BDSORates birthRate deathRate samplingRate occurrenceRate) _ =
    Just $ birthRate + deathRate + samplingRate + occurrenceRate
  birthProb (BDSORates birthRate deathRate samplingRate occurrenceRate) _ =
    Just $ birthRate / (birthRate + deathRate + samplingRate + occurrenceRate)

newtype BDSOPopulation =
  BDSOPopulation People
  deriving (Show)

instance Population BDSOPopulation where
  susceptiblePeople _ = Nothing
  infectiousPeople (BDSOPopulation people) = Just people
  removedPeople _ = Nothing
  isInfected (BDSOPopulation (People people)) = not $ V.null people

birthDeathSamplingOccurrenceRates :: Rate -> Rate -> Rate -> Rate -> BDSORates
birthDeathSamplingOccurrenceRates = BDSORates -- birthRate deathRate samplingRate occurrenceRate

-- | Configuration of a birth-death-sampling-occurrence simulation
configuration :: Time                  -- ^ Duration of the simulation
              -> (Rate,Rate,Rate,Rate) -- ^ Birth, Death, Sampling and Occurrence rates
              -> SimulationConfiguration BDSORates BDSOPopulation
configuration maxTime (birthRate, deathRate, samplingRate, occurrenceRate) =
  let bdsoRates =
        birthDeathSamplingOccurrenceRates
          birthRate
          deathRate
          samplingRate
          occurrenceRate
      (seedPerson, newId) = newPerson initialIdentifier
      bdsoPop = BDSOPopulation (People $ V.singleton seedPerson)
   in SimulationConfiguration bdsoRates bdsoPop newId maxTime

randomBirthDeathSamplingOccurrenceEvent ::
     BDSORates
  -> Time
  -> BDSOPopulation
  -> Identifier
  -> GenIO
  -> IO (Time, Event, BDSOPopulation, Identifier)
randomBirthDeathSamplingOccurrenceEvent rates@(BDSORates br dr sr ocr) currTime (BDSOPopulation (People currPeople)) currId gen =
  let netEventRate = fromJust $ eventRate rates currTime
      eventWeights = V.fromList [br,dr,sr,ocr]
   in
    do delay <- exponential (fromIntegral (V.length currPeople) * netEventRate) gen
       eventIx <- categorical eventWeights gen
       (selectedPerson, unselectedPeople) <- randomPerson currPeople gen
       return $ case eventIx of
         0 -> let newTime = currTime + delay
                  (birthedPerson, newId) = newPerson currId
                  event = InfectionEvent newTime selectedPerson birthedPerson
              in ( newTime
                 , event
                 , BDSOPopulation (People $ V.cons birthedPerson currPeople)
                 , newId)
         1 -> let newTime = currTime + delay
                  event = RemovalEvent newTime selectedPerson
              in (newTime, event, BDSOPopulation (People unselectedPeople), currId)
         2 -> let newTime = currTime + delay
                  event = SamplingEvent newTime selectedPerson
              in (newTime, event, BDSOPopulation (People unselectedPeople), currId)
         3 -> let newTime = currTime + delay
                  event = OccurrenceEvent newTime selectedPerson
              in (newTime, event, BDSOPopulation (People unselectedPeople), currId)
         _ -> error "no birth-death-sampling-occurrence event selected."

allEvents ::
     BDSORates
  -> Time
  -> (Time, [Event], BDSOPopulation, Identifier)
  -> GenIO
  -> IO (Time, [Event], BDSOPopulation, Identifier)
allEvents rates maxTime currState@(currTime, currEvents, currPop, currId) gen =
  if isInfected currPop
    then do
      (newTime, event, newPop, newId) <-
        randomBirthDeathSamplingOccurrenceEvent rates currTime currPop currId gen
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
observedEvents events =
  sort $ occurrenceEvents ++ sampleTreeEvents''
  where
    occurrenceEvents = filter isOccurrence events
    sampleTreeEvents'' =
      sampleTreeEvents . sampleTree $ transmissionTree events (Person 1)
