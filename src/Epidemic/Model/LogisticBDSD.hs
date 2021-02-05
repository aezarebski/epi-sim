{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}

module Epidemic.Model.LogisticBDSD
  ( configuration
  , observedEvents
  , randomEvent
  , LogisticBDSDParameters(..)
  , LogisticBDSDPopulation(..)
  ) where

import qualified Data.Vector as V
import Epidemic.Types.Events (EpidemicEvent(..))
import Epidemic.Types.Parameter
  ( AbsoluteTime(..)
  , ModelParameters(..)
  , Probability
  , Rate
  , TimeDelta
  , Timed(..)
  , asTimed
  )
import Epidemic.Types.Population (Identifier(..), People(..), Population(..), nullPeople, numPeople)
import Epidemic.Types.Simulation
  ( SimulationConfiguration(..)
  , SimulationRandEvent(..)
  , SimulationState(..)
  )
import Epidemic.Utility (initialIdentifier, maybeToRight, newPerson)
import System.Random.MWC (GenIO)

data LogisticBDSDParameters =
  LogisticBDSDParameters
    { paramsBirthRate :: Rate
    , paramsCapacity :: Int
    , paramsDeathRate :: Rate
    , paramsSamplingRate :: Rate
    , paramsDisasters :: Timed Probability
    }
  deriving (Show)

newtype LogisticBDSDPopulation =
  LogisticBDSDPopulation People
  deriving (Show)

-- | The per lineage birth rate accounting for the population size.
logisticBirthRate :: LogisticBDSDParameters -> LogisticBDSDPopulation -> Rate
logisticBirthRate LogisticBDSDParameters {..} (LogisticBDSDPopulation pop) =
  let propCapacity = fromIntegral (numPeople pop) / fromIntegral paramsCapacity
   in paramsBirthRate * (1.0 - propCapacity)

instance ModelParameters LogisticBDSDParameters LogisticBDSDPopulation where
  rNaught _ _ _ = Nothing
  eventRate (LogisticBDSDPopulation pop) LogisticBDSDParameters {..} _ =
    let propCapcity = fromIntegral (numPeople pop) / fromIntegral paramsCapacity
        br = paramsBirthRate * (1.0 - propCapcity)
     in Just $ br + paramsDeathRate + paramsSamplingRate
  birthProb lpop lparam@LogisticBDSDParameters {..} absTime = do
    er <- eventRate lpop lparam absTime
    Just $ br / er
    where
      br = logisticBirthRate lparam lpop

instance Population LogisticBDSDPopulation where
  susceptiblePeople _ = Nothing
  infectiousPeople (LogisticBDSDPopulation people) = Just people
  removedPeople _ = Nothing
  isInfected (LogisticBDSDPopulation people) = not $ nullPeople people


-- | Create an simulation configuration or return an error message if this is
-- not possible.
configuration ::
     TimeDelta
  -> (Rate, Int, Rate, Rate, [(AbsoluteTime, Probability)])
  -> Either String (SimulationConfiguration LogisticBDSDParameters LogisticBDSDPopulation)
configuration simDuration (birthRate, capacity, deathRate, samplingRate, disasterSpec)
  | minimum [birthRate, deathRate, samplingRate] < 0 =
    Left "negative rate provided"
  | capacity < 1 = Left "insufficient population capacity"
  | otherwise = do
    disasterTP <-
      maybeToRight
        "could not construct timed probability"
        (asTimed disasterSpec)
    let logBDSDParams =
          LogisticBDSDParameters
            birthRate
            capacity
            deathRate
            samplingRate
            disasterTP
        (seedPerson, newId) = newPerson initialIdentifier
        logBDSDPop = LogisticBDSDPopulation (People $ V.singleton seedPerson)
     in return $
        SimulationConfiguration
          logBDSDParams
          logBDSDPop
          newId
          (AbsoluteTime 0)
          simDuration
          Nothing

-- | Defines how a single random event is simulated in this model.
randomEvent :: SimulationRandEvent LogisticBDSDParameters LogisticBDSDPopulation
randomEvent = SimulationRandEvent randEvent'

randEvent' ::
     LogisticBDSDParameters
  -> AbsoluteTime
  -> LogisticBDSDPopulation
  -> Identifier
  -> GenIO
  -> IO (AbsoluteTime, EpidemicEvent, LogisticBDSDPopulation, Identifier)
randEvent' = undefined


-- | From a full list of epidemic events that occurred in a simulation return
-- just the ones that were observed. For the logistic-BDSD this is those that
-- are part of the reconstructed tree and those that occurred in disasters. If
-- this is not possible return an error message in a string.
observedEvents :: [EpidemicEvent] -> Either String [EpidemicEvent]
observedEvents = undefined
