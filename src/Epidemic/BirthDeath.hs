{-# LANGUAGE RecordWildCards #-}

module Epidemic.BirthDeath where

import qualified Data.Vector as V
import System.Random.MWC
import System.Random.MWC.Distributions (bernoulli, exponential)

import Epidemic
import Epidemic.Utility

data BDRates =
  BDRates Rate Rate

instance ModelParameters BDRates where
  rNaught (BDRates birthRate deathRate) = birthRate / deathRate
  eventRate (BDRates birthRate deathRate) = birthRate + deathRate

newtype BDPopulation =
  BDPopulation People
  deriving (Show)

instance Population BDPopulation where
  susceptiblePeople _ = Nothing
  infectiousPeople (BDPopulation people) = Just people
  removedPeople _ = Nothing
  isInfected (BDPopulation (People people)) = not $ V.null people

birthDeathRates :: Rate -> Rate -> BDRates
birthDeathRates = BDRates -- birthRate deathRate

-- | Configuration of a birth-death simulation.
birthDeathConfig :: Time         -- ^ Duration of the simulation
                 -> (Rate, Rate) -- ^ Birth and Death rates
                 -> SimulationConfiguration BDRates BDPopulation
birthDeathConfig maxTime (birthRate, deathRate) =
  let bdRates = birthDeathRates birthRate deathRate
      (seedPerson, newId) = newPerson initialIdentifier
      bdPop = BDPopulation (People $ V.singleton seedPerson)
   in SimulationConfiguration bdRates bdPop newId maxTime

randomBirthDeathEvent ::
     BDRates
  -> Time
  -> BDPopulation
  -> Identifier
  -> GenIO
  -> IO (Time, Event, BDPopulation, Identifier)
randomBirthDeathEvent (BDRates br dr) currTime (BDPopulation (People currPeople)) currId gen = do
  delay <- exponential (fromIntegral (V.length currPeople) * (br + dr)) gen
  isBirth <- bernoulli (br / (br + dr)) gen
  (selectedPerson, unselectedPeople) <- randomPerson currPeople gen
  return $
    if isBirth
      then let newTime = currTime + delay
               (birthedPerson, newId) = newPerson currId
               event = InfectionEvent newTime selectedPerson birthedPerson
            in ( newTime
               , event
               , BDPopulation (People $ V.cons birthedPerson currPeople)
               , newId)
      else let newTime = currTime + delay
               event = RemovalEvent newTime selectedPerson
            in (newTime, event, BDPopulation (People unselectedPeople), currId)

birthDeathEvents ::
     BDRates
  -> Time
  -> (Time, [Event], BDPopulation, Identifier)
  -> GenIO
  -> IO (Time, [Event], BDPopulation, Identifier)
birthDeathEvents rates maxTime currState@(currTime, currEvents, currPop, currId) gen =
  if isInfected currPop
    then do
      (newTime, event, newPop, newId) <-
        randomBirthDeathEvent rates currTime currPop currId gen
      if newTime < maxTime
        then birthDeathEvents
               rates
               maxTime
               (newTime, event : currEvents, newPop, newId)
               gen
        else return currState
    else return currState

birthDeathSimulation :: SimulationConfiguration BDRates BDPopulation
                     -> IO [Event]
birthDeathSimulation SimulationConfiguration {..} = do
  gen <- System.Random.MWC.create :: IO GenIO
  (_, events, _, _) <-
    birthDeathEvents rates timeLimit (0, [], population, newIdentifier) gen
  return $ sort events
