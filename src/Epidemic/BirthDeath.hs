{-# LANGUAGE RecordWildCards #-}

module Epidemic.BirthDeath
  ( configuration
  , allEvents
  ) where

import Epidemic.Types.Parameter
import Epidemic.Types.Population
import Epidemic.Types.Events
import qualified Data.Vector as V
import System.Random.MWC
import System.Random.MWC.Distributions (bernoulli, exponential)

import Epidemic
import Epidemic.Utility

data BDRates =
  BDRates Rate Rate

instance ModelParameters BDRates where
  rNaught (BDRates birthRate deathRate) _ = Just $ birthRate / deathRate
  eventRate (BDRates birthRate deathRate) _ = Just $ birthRate + deathRate
  birthProb (BDRates birthRate deathRate) _ = Just $ birthRate / (birthRate + deathRate)

newtype BDPopulation =
  BDPopulation People
  deriving (Show)

instance Population BDPopulation where
  susceptiblePeople _ = Nothing
  infectiousPeople (BDPopulation people) = Just people
  removedPeople _ = Nothing
  isInfected (BDPopulation (People people)) = not $ V.null people

-- | Return a BD-process parameters object
birthDeathRates :: Rate -- ^ birth rate
                -> Rate -- ^ death rate
                -> Maybe BDRates
birthDeathRates birthRate deathRate
  | birthRate >= 0 && deathRate >= 0 = Just $ BDRates birthRate deathRate
  | otherwise = Nothing

-- | Configuration of a birth-death simulation.
configuration :: AbsoluteTime         -- ^ Duration of the simulation
                 -> (Rate, Rate) -- ^ Birth and Death rates
                 -> Maybe (SimulationConfiguration BDRates BDPopulation)
configuration maxTime (birthRate, deathRate) =
  let (seedPerson, newId) = newPerson initialIdentifier
      bdPop = BDPopulation (People $ V.singleton seedPerson)
   in do maybeBDRates <- birthDeathRates birthRate deathRate
         if maxTime > AbsoluteTime 0 then Just (SimulationConfiguration maybeBDRates bdPop newId maxTime) else Nothing

randomBirthDeathEvent ::
     BDRates
  -> AbsoluteTime
  -> BDPopulation
  -> Integer
  -> GenIO
  -> IO (AbsoluteTime, EpidemicEvent, BDPopulation, Integer)
randomBirthDeathEvent (BDRates br dr) currTime (BDPopulation currPeople) currId gen = do
  delay <- exponential (fromIntegral (numPeople currPeople) * (br + dr)) gen
  let newTime = timeAfterDelta currTime (TimeDelta delay)
  isBirth <- bernoulli (br / (br + dr)) gen
  (selectedPerson, unselectedPeople) <- randomPerson currPeople gen
  return $
    if isBirth
    then let (birthedPerson, newId) = newPerson currId
             event = Infection newTime selectedPerson birthedPerson
         in ( newTime
            , event
            , BDPopulation (addPerson birthedPerson currPeople)
            , newId)
    else let event = Removal newTime selectedPerson
         in (newTime, event, BDPopulation unselectedPeople, currId)

allEvents ::
     BDRates
  -> AbsoluteTime
  -> (AbsoluteTime, [EpidemicEvent], BDPopulation, Integer)
  -> GenIO
  -> IO (AbsoluteTime, [EpidemicEvent], BDPopulation, Integer)
allEvents rates maxTime currState@(currTime, currEvents, currPop, currId) gen =
  if isInfected currPop
    then do
      (newTime, event, newPop, newId) <-
        randomBirthDeathEvent rates currTime currPop currId gen
      if newTime < maxTime
        then allEvents
               rates
               maxTime
               (newTime, event : currEvents, newPop, newId)
               gen
        else return currState
    else return currState

