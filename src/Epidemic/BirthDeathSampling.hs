{-# LANGUAGE RecordWildCards #-}

module Epidemic.BirthDeathSampling
  ( configuration
  , allEvents
  ) where

import Data.Maybe (fromJust)
import qualified Data.Vector as V
import System.Random.MWC
import System.Random.MWC.Distributions (categorical, exponential)

import Epidemic
import Epidemic.Utility

data BDSRates =
  BDSRates Rate Rate Rate

instance ModelParameters BDSRates where
  rNaught (BDSRates birthRate deathRate samplingRate) _ =
    Just $ birthRate / (deathRate + samplingRate)
  eventRate (BDSRates birthRate deathRate samplingRate) _ =
    Just $ birthRate + deathRate + samplingRate
  birthProb (BDSRates birthRate deathRate samplingRate) _ =
    Just $ birthRate / (birthRate + deathRate + samplingRate)

newtype BDSPopulation =
  BDSPopulation People
  deriving (Show)

instance Population BDSPopulation where
  susceptiblePeople _ = Nothing
  infectiousPeople (BDSPopulation people) = Just people
  removedPeople _ = Nothing
  isInfected (BDSPopulation (People people)) = not $ V.null people

birthDeathSamplingRates :: Rate -> Rate -> Rate -> BDSRates
birthDeathSamplingRates = BDSRates -- birthRate deathRate samplingRate

-- | Configuration of a birth-death-sampling simulation.
configuration :: Time             -- ^ Duration of the simulation
              -> (Rate,Rate,Rate) -- ^ Birth, Death and Sampling rates
              -> SimulationConfiguration BDSRates BDSPopulation
configuration maxTime (birthRate, deathRate, samplingRate) =
  let bdsRates = birthDeathSamplingRates birthRate deathRate samplingRate
      (seedPerson, newId) = newPerson initialIdentifier
      bdsPop = BDSPopulation (People $ V.singleton seedPerson)
   in SimulationConfiguration bdsRates bdsPop newId maxTime

randomBirthDeathSamplingEvent ::
     BDSRates
  -> Time
  -> BDSPopulation
  -> Identifier
  -> GenIO
  -> IO (Time, Event, BDSPopulation, Identifier)
randomBirthDeathSamplingEvent rates@(BDSRates br dr sr) currTime (BDSPopulation (People currPeople)) currId gen =
  let netEventRate = fromJust $ eventRate rates currTime 
      eventWeights = V.fromList [br,dr,sr]
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
                 , BDSPopulation (People $ V.cons birthedPerson currPeople)
                 , newId)
         1 -> let newTime = currTime + delay
                  event = RemovalEvent newTime selectedPerson
              in (newTime, event, BDSPopulation (People unselectedPeople), currId)
         2 -> let newTime = currTime + delay
                  event = SamplingEvent newTime selectedPerson
              in (newTime, event, BDSPopulation (People unselectedPeople), currId)
         _ -> error "no birth-death-sampling event selected."

allEvents ::
     BDSRates
  -> Time
  -> (Time, [Event], BDSPopulation, Identifier)
  -> GenIO
  -> IO (Time, [Event], BDSPopulation, Identifier)
allEvents rates maxTime currState@(currTime, currEvents, currPop, currId) gen =
  if isInfected currPop
    then do
      (newTime, event, newPop, newId) <-
        randomBirthDeathSamplingEvent rates currTime currPop currId gen
      if newTime < maxTime
        then allEvents
               rates
               maxTime
               (newTime, event : currEvents, newPop, newId)
               gen
        else return currState
    else return currState

