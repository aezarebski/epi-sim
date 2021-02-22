{-# LANGUAGE MultiParamTypeClasses #-}

module Epidemic.Model.BirthDeathSampling
  ( configuration
  , randomEvent
  , BDSRates(..)
  , BDSPopulation(..)
  ) where

import Epidemic.Types.Time
  ( AbsoluteTime(..)
  , Timed(..)
  , TimeDelta(..)
  , allTimes
  , asTimed
  , diracDeltaValue
  , nextTime
  , cadlagValue
  , timeAfterDelta
  )
import Data.Maybe (fromJust)
import qualified Data.Vector as V
import Epidemic
import Epidemic.Types.Events
import Epidemic.Types.Parameter
import Epidemic.Types.Population
import Epidemic.Types.Simulation
  ( SimulationConfiguration(..)
  , SimulationRandEvent(..)
  )
import Epidemic.Utility
import System.Random.MWC
import System.Random.MWC.Distributions (categorical, exponential)

data BDSRates =
  BDSRates Rate Rate Rate

newtype BDSPopulation =
  BDSPopulation People
  deriving (Show)

instance ModelParameters BDSRates BDSPopulation where
  rNaught _ (BDSRates birthRate deathRate samplingRate) _ =
    Just $ birthRate / (deathRate + samplingRate)
  eventRate _ (BDSRates birthRate deathRate samplingRate) _ =
    Just $ birthRate + deathRate + samplingRate
  birthProb _ (BDSRates birthRate deathRate samplingRate) _ =
    Just $ birthRate / (birthRate + deathRate + samplingRate)

instance Population BDSPopulation where
  susceptiblePeople _ = Nothing
  infectiousPeople (BDSPopulation people) = Just people
  removedPeople _ = Nothing
  isInfected (BDSPopulation (People people)) = not $ V.null people

birthDeathSamplingRates :: Rate -> Rate -> Rate -> BDSRates
birthDeathSamplingRates = BDSRates -- birthRate deathRate samplingRate

-- | Configuration of a birth-death-sampling simulation.
configuration ::
     TimeDelta -- ^ Duration of the simulation
  -> (Rate, Rate, Rate) -- ^ Birth, Death and Sampling rates
  -> SimulationConfiguration BDSRates BDSPopulation
configuration maxTime (birthRate, deathRate, samplingRate) =
  let bdsRates = birthDeathSamplingRates birthRate deathRate samplingRate
      (seedPerson, newId) = newPerson initialIdentifier
      bdsPop = BDSPopulation (People $ V.singleton seedPerson)
   in SimulationConfiguration bdsRates bdsPop newId (AbsoluteTime 0) maxTime Nothing

randomEvent :: SimulationRandEvent BDSRates BDSPopulation
randomEvent = SimulationRandEvent randomBirthDeathSamplingEvent

randomBirthDeathSamplingEvent ::
     BDSRates
  -> AbsoluteTime
  -> BDSPopulation
  -> Identifier
  -> GenIO
  -> IO (AbsoluteTime, EpidemicEvent, BDSPopulation, Identifier)
randomBirthDeathSamplingEvent bdsRates@(BDSRates br dr sr) currTime pop@(BDSPopulation currPeople) currId gen =
  let individualEventRate = fromJust $ eventRate pop bdsRates currTime
      eventWeights = V.fromList [br, dr, sr]
   in do delay <-
           exponential
             (fromIntegral (numPeople currPeople) * individualEventRate)
             gen
         let newEventTime = timeAfterDelta currTime (TimeDelta delay)
         (selectedPerson, unselectedPeople) <- randomPerson currPeople gen
         eventIx <- categorical eventWeights gen
         case eventIx of
           0 ->
             let (birthedPerson, newId) = newPerson currId
              in return
                   ( newEventTime
                   , Infection newEventTime selectedPerson birthedPerson
                   , BDSPopulation (addPerson birthedPerson currPeople)
                   , newId)
           1 ->
             return
               ( newEventTime
               , Removal newEventTime selectedPerson
               , BDSPopulation unselectedPeople
               , currId)
           2 ->
             return
               ( newEventTime
               , Sampling newEventTime selectedPerson
               , BDSPopulation unselectedPeople
               , currId)
           _ -> error "no birth-death-sampling event selected."
