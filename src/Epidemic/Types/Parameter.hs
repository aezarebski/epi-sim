{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Epidemic.Types.Parameter where

import Epidemic.Types.Population (Population(..))
import Epidemic.Types.Time (AbsoluteTime(..))

-- | Class of types that can be considered parameterisations of a epidemic
-- model.
class (Population p) => ModelParameters a p where
  rNaught :: p -> a -> AbsoluteTime -> Maybe Double
  eventRate :: p -> a -> AbsoluteTime -> Maybe Rate
  birthProb :: p -> a -> AbsoluteTime -> Maybe Probability

type Rate = Double

type Probability = Double
