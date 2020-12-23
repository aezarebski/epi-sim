module Epidemic.BirthDeathSampling
  ( configuration
  , allEvents
  ) where

import Data.Maybe (fromJust, isJust, isNothing)
import qualified Data.Vector as V
import Epidemic.Types.Events
import Epidemic.Types.Parameter
import Epidemic.Types.Population
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
configuration ::
     TimeDelta -- ^ Duration of the simulation
  -> (Rate, Rate, Rate) -- ^ Birth, Death and Sampling rates
  -> SimulationConfiguration BDSRates BDSPopulation
configuration maxTime (birthRate, deathRate, samplingRate) =
  let bdsRates = birthDeathSamplingRates birthRate deathRate samplingRate
      (seedPerson, newId) = newPerson initialIdentifier
      bdsPop = BDSPopulation (People $ V.singleton seedPerson)
   in SimulationConfiguration bdsRates bdsPop newId (AbsoluteTime 0) maxTime Nothing

randomBirthDeathSamplingEvent ::
     BDSRates
  -> AbsoluteTime
  -> BDSPopulation
  -> Identifier
  -> GenIO
  -> IO (AbsoluteTime, EpidemicEvent, BDSPopulation, Identifier)
randomBirthDeathSamplingEvent bdsRates@(BDSRates br dr sr) currTime (BDSPopulation currPeople) currId gen =
  let individualEventRate = fromJust $ eventRate bdsRates currTime
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

allEvents ::
     BDSRates
  -> AbsoluteTime
  -> Maybe (BDSPopulation -> Bool) -- ^ predicate for a valid population
  -> SimulationState BDSPopulation
  -> GenIO
  -> IO (SimulationState BDSPopulation)
allEvents _ _ _ TerminatedSimulation _ = return TerminatedSimulation
allEvents bdsRates maxTime maybePopPredicate currState@(SimulationState (currTime, currEvents, currPop, currId)) gen =
  if isNothing maybePopPredicate || (isJust maybePopPredicate && fromJust maybePopPredicate currPop)
  then
    if isInfected currPop
    then do
      (newTime, event, newPop, newId) <-
        randomBirthDeathSamplingEvent bdsRates currTime currPop currId gen
      if newTime < maxTime
        then allEvents
               bdsRates
               maxTime
               maybePopPredicate
               (SimulationState (newTime, event : currEvents, newPop, newId))
               gen
        else return currState
    else return currState
  else return TerminatedSimulation
