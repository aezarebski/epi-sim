{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE RecordWildCards #-}

-- |
-- Module: Epidemic.Data.Time
-- Copyright: (c) 2021 Alexander E. Zarebski
-- License: MIT
--
-- Maintainer: Alexander E. Zarebski <aezarebski@gmail.com>
-- Stability: unstable
-- Portability: ghc
--
-- This module provides some types and functions for working with times. For
-- absolute times there is the 'AbsoluteTime' type and for differences between
-- times there is the 'TimeDelta' type. For quantities that vary across time the
-- 'Timed' type is a way to represent piecewise constant functions and there are
-- several helper functions to query these objects.

module Epidemic.Data.Time
  ( AbsoluteTime(..)
  , TimeDelta(..)
  , TimeInterval(..)
  , Timed(..)
  , TimeStamp(..)
  , allTimes
  , allValues
  , asConsecutiveIntervals1
  , asTimed
  , cadlagValue
  , diracDeltaValue
  , hasTime
  , inInterval
  , maybeNextTimed
  , nextTime
  , timeAfterDelta
  , timeDelta
  , timeInterval1
  , timeInterval2
  ) where

import qualified Data.Aeson   as Json
import qualified Data.List    as List
import qualified Data.Maybe   as Maybe
import           GHC.Generics

-- | Absolute time.
newtype AbsoluteTime =
  AbsoluteTime Double
  deriving (Generic, Eq, Show, Ord)

instance Json.FromJSON AbsoluteTime

instance Json.ToJSON AbsoluteTime

-- | A type that has an absolute time associated with it and can be treated as
-- having a temporal ordering.
--
-- > a = AbsoluteTime 1
-- > b = AbsoluteTime 2
-- > a `isBefore` b
--
class TimeStamp a where
  absTime :: a -> AbsoluteTime

  isAfter :: a -> a -> Bool
  isAfter x y = absTime x > absTime y

  isBefore :: a -> a -> Bool
  isBefore x y = absTime x < absTime y

instance TimeStamp AbsoluteTime where
  absTime = id

-- | Duration of time between two absolute times.
newtype TimeDelta =
  TimeDelta Double
  deriving (Generic, Eq, Show, Ord)

instance Json.FromJSON TimeDelta

instance Json.ToJSON TimeDelta

-- | An interval of time
data TimeInterval =
  TimeInterval
    { timeIntEndPoints :: (AbsoluteTime, AbsoluteTime)
    , timeIntDuration  :: TimeDelta
    }
  deriving (Generic, Eq, Show)

instance Json.FromJSON TimeInterval

instance Json.ToJSON TimeInterval

-- | The duration of time between two absolute times
--
-- >>> timeDelta (AbsoluteTime 1) (AbsoluteTime 2.5)
-- TimeDelta 1.5
--
timeDelta ::
     AbsoluteTime -- ^ start
  -> AbsoluteTime -- ^ finish
  -> TimeDelta
timeDelta (AbsoluteTime t0) (AbsoluteTime t1) = TimeDelta (t1 - t0)

-- | The time after a given delay
--
-- >>> timeAfterDelta (AbsoluteTime 1) (TimeDelta 2.5)
-- AbsoluteTime 3.5
--
timeAfterDelta :: AbsoluteTime -> TimeDelta -> AbsoluteTime
timeAfterDelta (AbsoluteTime t0) (TimeDelta d) = AbsoluteTime (t0 + d)

-- | Construct a 'TimeInterval' from the end points.
timeInterval1 :: AbsoluteTime -> AbsoluteTime -> TimeInterval
timeInterval1 start end = TimeInterval (start, end) (timeDelta start end)

-- | Construct a 'TimeInterval' from the start time and the duration.
timeInterval2 :: AbsoluteTime -> TimeDelta -> TimeInterval
timeInterval2 start duration =
  TimeInterval (start, timeAfterDelta start duration) duration

-- | Check if an 'AbsoluteTime' sits within a 'TimeInterval'.
inInterval :: TimeStamp a => TimeInterval -> a -> Bool
inInterval TimeInterval {..} x =
  let (start, end) = timeIntEndPoints
      absT = absTime x
   in start <= absT && absT <= end

-- | Construct a list of consecutive intervals divided by the given absolute
-- times.
asConsecutiveIntervals1 :: [AbsoluteTime] -> [TimeInterval]
asConsecutiveIntervals1 absTimes =
  zipWith timeInterval1 (init absTimes) (tail absTimes)

-- | Type containing values at times. The times are increasing as required by
-- @asTimed@.
newtype Timed a =
  Timed [(AbsoluteTime, a)]
  deriving (Generic, Eq, Show)

instance Json.FromJSON a => Json.FromJSON (Timed a)

instance Json.ToJSON a => Json.ToJSON (Timed a)

instance Semigroup (Timed a) where
  (Timed x) <> (Timed y) = Timed $ List.sortOn fst (x ++ y)

-- | Construct a timed numeric value from a /sorted/ list of values and their
-- times.
asTimed ::
     Num a
  => [(AbsoluteTime, a)] -- ^ list of ascending times and values
  -> Maybe (Timed a)
asTimed tas
  | strictlyInc $ map fst tas = return . Timed $ tas <> pointAtInf
  | otherwise = Nothing
  where pointAtInf = [(AbsoluteTime (1 / 0), -1)]
        strictlyInc xs =
          case xs of
            (a:b:c) -> a < b && strictlyInc (b:c)
            _ -> True

-- | Evaluate the timed object treating it as a cadlag function
cadlagValue :: Timed a -> AbsoluteTime -> Maybe a
cadlagValue (Timed txs) = cadlagValue' txs

cadlagValue' :: [(AbsoluteTime, a)] -> AbsoluteTime -> Maybe a
cadlagValue' [] _ = Nothing
cadlagValue' ((t, x):txs) q =
  if q < t
    then Nothing
    else let nextCLV = cadlagValue' txs q
          in if Maybe.isNothing nextCLV
               then Just x
               else nextCLV

-- | Evaluate the timed object treating it as a direct delta function
diracDeltaValue :: Timed a -> AbsoluteTime -> Maybe a
diracDeltaValue (Timed txs) = diracDeltaValue' txs

diracDeltaValue' :: [(AbsoluteTime, a)] -> AbsoluteTime -> Maybe a
diracDeltaValue' txs q =
  case txs of
    ((t, x):txs') ->
      if t == q
        then Just x
        else diracDeltaValue' txs' q
    [] -> Nothing

-- | Check if there exists a pair with a particular time index.
hasTime :: Timed a -> AbsoluteTime -> Bool
hasTime tx absT = elem absT $ allTimes tx

-- | Return the value of the next time if possible or an exact match if it
-- exists.
nextTime :: Timed a -> AbsoluteTime -> Maybe AbsoluteTime
nextTime (Timed txs) = nextTime' txs

nextTime' :: [(AbsoluteTime, a)] -> AbsoluteTime -> Maybe AbsoluteTime
nextTime' txs q =
  case txs of
    ((t, _):txs') ->
      if q < t
        then Just t
        else nextTime' txs' q
    [] -> Nothing

-- | Return a list of the (finite) absolute times that the step function changes
-- value.
--
-- >>> let demoMaybeTimed = asTimed [(AbsoluteTime 1,2),(AbsoluteTime 1.5,1)]
-- >>> liftM allTimes demoMaybeTimed
-- Just [AbsoluteTime 1.0,AbsoluteTime 1.5]
--
allTimes :: Timed a -> [AbsoluteTime]
allTimes (Timed txs) = [t | (t, _) <- txs, not $ isInfiniteAbsoluteTime t]

-- | The values that the timed variable takes. NOTE that it is safe to use
-- 'fromJust' here because 'allTimes' only returns times for which there is a
-- cadlag value anyway.
--
-- >>> (Just tx) = asTimed [(AbsoluteTime 1,2),(AbsoluteTime 1.5,1)]
-- >>> allValues tx
-- [2,1]
--
allValues :: Timed a -> [a]
allValues (Timed txs) = [x | (t, x) <- txs, not $ isInfiniteAbsoluteTime t]

-- | Predicate for an infinite absolute time
isInfiniteAbsoluteTime :: AbsoluteTime -> Bool
isInfiniteAbsoluteTime (AbsoluteTime t) = isInfinite t

-- | Look at both of the timed objects and, if possible, return the time that
-- the first one changes along with the value it changes to.
--
-- >>> (Just tA) = asTimed [(AbsoluteTime 1, (1.1 :: Double)), (AbsoluteTime 3, 2.3)]
-- >>> (Just tB) = asTimed [(AbsoluteTime 2, (1 :: Int))]
-- >>> maybeNextTimed tA tB (AbsoluteTime 0.5)
-- Just (AbsoluteTime 1.0,Left 1.1)
-- >>> maybeNextTimed tA tB (AbsoluteTime 1.5)
-- Just (AbsoluteTime 2.0,Right 1)
-- >>> maybeNextTimed tA tB (AbsoluteTime 3.5)
-- Nothing
--
maybeNextTimed :: Timed a
               -> Timed b
               -> AbsoluteTime
               -> Maybe (AbsoluteTime, Either a b)
maybeNextTimed timedA timedB absT =
  let f = flip nextTime absT
      g1 timed at = do -- two functions are needed for the different types.
        v <- diracDeltaValue timed at
        if isInfiniteAbsoluteTime at
          then Nothing
          else Just (at, Left v)
      g2 timed at = do
        v <- diracDeltaValue timed at
        if isInfiniteAbsoluteTime at
          then Nothing
          else Just (at, Right v)
   in case (f timedA, f timedB) of
        (Just tA, Just tB) ->
          if tA < tB
            then g1 timedA tA
            else g2 timedB tB
        (Just tA, Nothing) -> g1 timedA tA
        (Nothing, Just tB) -> g2 timedB tB
        (Nothing, Nothing) -> Nothing
