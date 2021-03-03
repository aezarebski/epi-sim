{-# LANGUAGE GADTs #-}

module Epidemic.Types.Simulation
  ( SimulationConfiguration(..)
  , SimulationState(..)
  , SimulationRandEvent(..)
  ) where

import Epidemic.Types.Events
import Epidemic.Types.Parameter
import Epidemic.Types.Population
import Epidemic.Types.Time (AbsoluteTime(..), TimeDelta(..), timeDelta)
import System.Random.MWC

data SimulationConfiguration r p =
  SimulationConfiguration
    { -- | The event rates
      scRates :: r
      -- | The population
    , scPopulation :: p
      -- | A new identifier
    , scNewIdentifier :: Identifier
      -- | The absolute time at which the simulation starts
    , scStartTime :: AbsoluteTime
      -- | The duration of the simulation until it stops
    , scSimDuration :: TimeDelta
      -- | The simulation terminates if this predicate is not satisfied
    , scValidPopulation :: Maybe (p -> Bool)
    }

-- | Either there is a valid simulation state which contains a sequence of
-- epidemic events of there is a terminated simulation which indicates that
-- the simulation has been rejected.
data SimulationState b
  = SimulationState (AbsoluteTime, [EpidemicEvent], b, Identifier)
  | TerminatedSimulation
  deriving (Eq, Show)

data SimulationRandEvent a b where
  SimulationRandEvent
    :: (ModelParameters a b, Population b)
    => (a
    -> AbsoluteTime
    -> b
    -> Identifier
    -> GenIO
    -> IO (AbsoluteTime, EpidemicEvent, b, Identifier))
    -> SimulationRandEvent a b
