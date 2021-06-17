{-# LANGUAGE MultiParamTypeClasses #-}

module Epidemic.Types.Parameter where

import Data.Vector (Vector)
import Epidemic.Types.Population (Population(..))
import Epidemic.Types.Time (AbsoluteTime(..))

-- | Class of types that can be considered parameterisations of a epidemic
-- model.
class (Population p) => ModelParameters a p where

  -- | The basic reproduction number.
  --
  -- __NOTE__ that this is not the /effective/ reproduction number the
  -- population is included in case there is structure other than immunity that
  -- needs to be accounted for.
  rNaught :: p -> a -> AbsoluteTime -> Maybe Double

  -- | The total event rate at a particular point in time.
  eventRate :: p -> a -> AbsoluteTime -> Maybe Rate

  -- | The probability that an event will result in an infection.
  birthProb :: p -> a -> AbsoluteTime -> Maybe Probability

  -- | The __unnormalised__ distribution across the possible events.
  eventWeights :: p -> a -> AbsoluteTime -> Maybe (Vector Double)

type Rate = Double

type Probability = Double
