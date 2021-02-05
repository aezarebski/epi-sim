module Epidemic.Types.Simulation (SimulationConfiguration(..),SimulationState(..)) where


import Epidemic.Types.Events
import Epidemic.Types.Parameter
import Epidemic.Types.Population

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

data SimulationState b
  = SimulationState (AbsoluteTime, [EpidemicEvent], b, Identifier)
  | TerminatedSimulation
  deriving (Eq, Show)
