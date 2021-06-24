{-# LANGUAGE MultiParamTypeClasses #-}

-- |
-- Module: Epidemic.Types.Parameter
-- Copyright: (c) 2021 Alexander E. Zarebski
-- License: MIT
--
-- Maintainer: Alexander E. Zarebski <aezarebski@gmail.com>
-- Stability: unstable
-- Portability: ghc
--
-- This module defines some types and functions for working with parameters of models.
--

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

-- | The rate at which an event occurs
type Rate = Double

-- | A probability
type Probability = Double
