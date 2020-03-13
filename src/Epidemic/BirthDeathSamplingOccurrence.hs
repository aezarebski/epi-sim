{-# LANGUAGE RecordWildCards #-}

module Epidemic.BirthDeathSamplingOccurrence
  ( birthDeathSamplingOccurrenceSimulation
  , birthDeathSamplingOccurrenceConfig
  , birthDeathSamplingOccurrenceObservedEvents
  ) where

import qualified Data.Vector as V
import System.Random.MWC
import System.Random.MWC.Distributions (categorical, exponential)

import Epidemic
import Epidemic.Utility

data BDSORates =
  BDSORates Rate Rate Rate Rate

instance ModelParameters BDSORates where
  rNaught (BDSORates birthRate deathRate samplingRate occurrenceRate) =
    birthRate / (deathRate + samplingRate + occurrenceRate)
  eventRate (BDSORates birthRate deathRate samplingRate occurrenceRate) =
    birthRate + deathRate + samplingRate + occurrenceRate

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
birthDeathSamplingOccurrenceConfig :: Time                  -- ^ Duration of the simulation
                                   -> (Rate,Rate,Rate,Rate) -- ^ Birth, Death, Sampling and Occurrence rates
                                   -> SimulationConfiguration BDSORates BDSOPopulation
birthDeathSamplingOccurrenceConfig maxTime (birthRate, deathRate, samplingRate, occurrenceRate) =
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
randomBirthDeathSamplingOccurrenceEvent rates@(BDSORates br dr sr or) currTime (BDSOPopulation (People currPeople)) currId gen =
  let netEventRate = eventRate rates
      eventWeights = V.fromList [br,dr,sr,or]
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

birthDeathSamplingOccurrenceEvents ::
     BDSORates
  -> Time
  -> (Time, [Event], BDSOPopulation, Identifier)
  -> GenIO
  -> IO (Time, [Event], BDSOPopulation, Identifier)
birthDeathSamplingOccurrenceEvents rates maxTime currState@(currTime, currEvents, currPop, currId) gen =
  if isInfected currPop
    then do
      (newTime, event, newPop, newId) <-
        randomBirthDeathSamplingOccurrenceEvent rates currTime currPop currId gen
      if newTime < maxTime
        then birthDeathSamplingOccurrenceEvents
               rates
               maxTime
               (newTime, event : currEvents, newPop, newId)
               gen
        else return currState
    else return currState

-- | Run a simulation described by a configuration object.
birthDeathSamplingOccurrenceSimulation :: SimulationConfiguration BDSORates BDSOPopulation
                                       -> IO [Event]
birthDeathSamplingOccurrenceSimulation SimulationConfiguration {..} = do
  gen <- System.Random.MWC.create :: IO GenIO
  (_, events, _, _) <-
    birthDeathSamplingOccurrenceEvents rates timeLimit (0, [], population, newIdentifier) gen
  return $ sort events

-- | Just the observable events from a list of all the events in a simulation.
birthDeathSamplingOccurrenceObservedEvents :: [Event] -- ^ All of the simulation events
                                           -> [Event]
birthDeathSamplingOccurrenceObservedEvents events =
  sort $ occurrenceEvents ++ sampleTreeEvents'
  where
    occurrenceEvents = filter isOccurrence events
    sampleTreeEvents' =
      sampleTreeEvents . sampleTree $ transmissionTree events (Person 1)
