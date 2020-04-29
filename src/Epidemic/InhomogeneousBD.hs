{-# LANGUAGE RecordWildCards #-}

module Epidemic.InhomogeneousBD
  ( configuration
  , allEvents
  ) where

import qualified Data.Vector as V
import System.Random.MWC
import System.Random.MWC.Distributions (bernoulli, exponential)

import Epidemic
import Epidemic.Utility

data BDRates =
  BDRates Rate Rate

instance ModelParameters BDRates where
  rNaught (BDRates birthRate deathRate) _ = birthRate / deathRate
  eventRate (BDRates birthRate deathRate) _ = birthRate + deathRate

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
configuration :: Time         -- ^ Duration of the simulation
                 -> (Rate, Rate) -- ^ Birth and Death rates
                 -> Maybe (SimulationConfiguration BDRates BDPopulation)
configuration maxTime (birthRate, deathRate) =
  let (seedPerson, newId) = newPerson initialIdentifier
      bdPop = BDPopulation (People $ V.singleton seedPerson)
   in do maybeBDRates <- birthDeathRates birthRate deathRate
         if maxTime > 0 then Just (SimulationConfiguration maybeBDRates bdPop newId maxTime) else Nothing

-- | A random event and the state afterwards
randomEvent ::
     InhomBDRates -- ^ model parameters
  -> Time         -- ^ the current time
  -> InhomBDPop   -- ^ the population
  -> Identifier   -- ^ current identifier
  -> GenIO        -- ^ PRNG
  -> IO (Time, Event, InhomBDPop, Identifier)
randomEvent inhomRates@(InhomBDRates brts dr) currTime (InhomBDPop currPeople) currId gen = do
  delay <- inhomExponential (fromIntegral $ numPeople currPeople) inhomRates gen
  isBirth <- bernoulli (birthProb inhomRates (currTime + delay)) gen
  (selectedPerson, unselectedPeople) <- randomPerson currPeople gen
  return $
    if isBirth
      then let newTime = currTime + delay
               (birthedPerson, newId) = newPerson currId
               event = InfectionEvent newTime selectedPerson birthedPerson
            in ( newTime
               , event
               , InhomBDPop (People $ addPerson birthedPerson currPeople)
               , newId)
      else let newTime = currTime + delay
               event = RemovalEvent newTime selectedPerson
            in (newTime, event, BDPopulation (People unselectedPeople), currId)

-- | The state of the simulation at the time of the last event prior to the
-- stopping time.
allEvents ::
     InhomBDRates                            -- ^ model parameters
  -> Time                                    -- ^ stopping time
  -> (Time, [Event], InhomBDPop, Identifier) -- ^ simulation state
  -> GenIO                                   -- ^ PRNG
  -> IO (Time, [Event], InhomBDPop, Identifier)
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

