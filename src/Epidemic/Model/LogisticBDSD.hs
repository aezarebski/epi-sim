module Epidemic.Model.LogisticBDSD
  ( configuration
  , allEvents
  , observedEvents
  , LogisticBDSDParameters(..)
  , LogisticBDSDPopulation(..)
  ) where

import qualified Data.Vector as V
import Epidemic.Types.Events (EpidemicEvent(..))
import Epidemic.Types.Parameter
  ( AbsoluteTime(..)
  , Probability
  , Rate
  , TimeDelta
  , Timed(..)
  , asTimed
  )
import Epidemic.Types.Population (People(..))
import Epidemic.Types.Simulation (SimulationConfiguration(..),SimulationState(..))
import Epidemic.Utility
  ( initialIdentifier
  , maybeToRight
  , newPerson
  )
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

-- | This acts as a coordinator of the whole simulation.
--
-- TODO Abstract this as it repeated across several modules
--
allEvents ::
     LogisticBDSDParameters
  -> AbsoluteTime
  -> Maybe (LogisticBDSDPopulation -> Bool)
  -> SimulationState LogisticBDSDPopulation
  -> GenIO
  -> IO (SimulationState LogisticBDSDPopulation)
allEvents = undefined

-- | From a full list of epidemic events that occurred in a simulation return
-- just the ones that were observed. For the logistic-BDSD this is those that
-- are part of the reconstructed tree and those that occurred in disasters. If
-- this is not possible return an error message in a string.
observedEvents :: [EpidemicEvent] -> Either String [EpidemicEvent]
observedEvents = undefined
