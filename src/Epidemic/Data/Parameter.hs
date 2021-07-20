{-# LANGUAGE MultiParamTypeClasses #-}

-- |
-- Module: Epidemic.Data.Parameter
-- Copyright: (c) 2021 Alexander E. Zarebski
-- License: MIT
--
-- Maintainer: Alexander E. Zarebski <aezarebski@gmail.com>
-- Stability: unstable
-- Portability: ghc
--
-- This module defines some types and functions for working with parameters of models.
--

module Epidemic.Data.Parameter (ModelParameters (..), Rate, Probability, firstScheduled, noScheduledEvent) where

import           Data.Vector              (Vector)
import           Epidemic.Data.Population (Population (..))
import           Epidemic.Data.Time       (AbsoluteTime (..), Timed (..),
                                           diracDeltaValue, nextTime)

-- | Class of types that can be considered parameterisations of a epidemic
-- model.
class (Population p) => ModelParameters a p where

  -- | The basic reproduction number.
  --
  -- __NOTE__ that this is /not/ the effective reproduction number the
  -- population is included in case there is structure other than immunity that
  -- needs to be accounted for.
  rNaught :: p -> a -> AbsoluteTime -> Maybe Double

  -- | The per capita event rate at a particular point in time.
  perCapitaEventRate :: p -> a -> AbsoluteTime -> Maybe Rate

  -- | The probability that an event will result in an infection.
  birthProb :: p -> a -> AbsoluteTime -> Maybe Probability

  -- | The __unnormalised__ distribution across the possible events.
  eventWeights :: p -> a -> AbsoluteTime -> Maybe (Vector Double)

-- | The rate at which an event occurs
type Rate = Double

-- | A probability
type Probability = Double

-- | Predicate for whether there is a scheduled event during an interval. NOTE
-- that this does not consider events that happen at the start of the interval
-- as occurring between the times.
--
-- >>> tA = AbsoluteTime 1.0
-- >>> tB = AbsoluteTime 2.0
-- >>> noScheduledEvent tA tB <$> asTimed [(AbsoluteTime 1.5, 0.5)]
-- Just False
-- >>> noScheduledEvent tA tB <$> asTimed [(AbsoluteTime 2.5, 0.5)]
-- Just True
-- >>> noScheduledEvent tA tB <$> asTimed [(tA, 0.5)]
-- Just True
--
noScheduledEvent ::
     AbsoluteTime -- ^ Start time for interval
  -> AbsoluteTime -- ^ End time for interval
  -> Timed Probability -- ^ Information about all scheduled events
  -> Bool
noScheduledEvent _ _ (Timed []) = True
noScheduledEvent a b (Timed ((shedTime, _):scheduledEvents)) =
  not (a < shedTime && shedTime <= b) &&
  noScheduledEvent a b (Timed scheduledEvents)

-- | The first scheduled event after a given time.
firstScheduled ::
     AbsoluteTime -- ^ The given time
  -> Timed Probability -- ^ The information about all scheduled events
  -> Maybe (AbsoluteTime, Probability)
firstScheduled time timedProb = do
  time' <- nextTime timedProb time
  prob' <- diracDeltaValue timedProb time'
  return (time', prob')
