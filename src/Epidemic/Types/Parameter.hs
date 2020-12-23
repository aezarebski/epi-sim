{-# LANGUAGE DeriveGeneric #-}

module Epidemic.Types.Parameter where

import qualified Data.Aeson as Json
import qualified Data.Csv as Csv
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import GHC.Generics

-- | Absolute time.
newtype AbsoluteTime =
  AbsoluteTime Double
  deriving (Generic, Eq, Show, Ord)

instance Json.FromJSON AbsoluteTime

instance Json.ToJSON AbsoluteTime

instance Csv.ToField AbsoluteTime where
  toField (AbsoluteTime at) = Csv.toField at

instance Csv.FromField AbsoluteTime where
  parseField s = AbsoluteTime <$> Csv.parseField s

-- | Predicate for an infinite absolute time
isInfiniteAbsoluteTime :: AbsoluteTime -> Bool
isInfiniteAbsoluteTime (AbsoluteTime t) = isInfinite t

-- | Duration of time between two absolute times.
newtype TimeDelta =
  TimeDelta Double
  deriving (Generic, Eq, Show, Ord)

instance Json.FromJSON TimeDelta

instance Json.ToJSON TimeDelta

instance Csv.ToField TimeDelta where
  toField (TimeDelta td) = Csv.toField td

-- | The duration of time between two absolute times
--
-- >>> timeDelta (AbsoluteTime 1) (AbsoluteTime 2.5)
-- TimeDelta 1.5
--
timeDelta :: AbsoluteTime -- ^ start
          -> AbsoluteTime -- ^ finish
          -> TimeDelta
timeDelta (AbsoluteTime t0) (AbsoluteTime t1) = TimeDelta (t1 - t0)

-- | The time after a given delay
--
-- >>> timeAfterDelta (AbsoluteTime 1) (TimeDelta 2.5)
-- AbsoluteTime 3.5
--
timeAfterDelta :: AbsoluteTime
               -> TimeDelta
               -> AbsoluteTime
timeAfterDelta (AbsoluteTime t0) (TimeDelta d) = AbsoluteTime (t0 + d)

-- | Type containing values at times. The times are increasing as required by
-- @asTimed@.
newtype Timed a =
  Timed [(AbsoluteTime, a)]
  deriving (Generic, Eq, Show)

instance Json.FromJSON a => Json.FromJSON (Timed a)

instance Json.ToJSON a => Json.ToJSON (Timed a)

instance Semigroup (Timed a) where
  (Timed x) <> (Timed y) = Timed $ List.sortOn fst (x ++ y)

type Rate = Double

type Probability = Double

-- | Construct a timed list if possible.
asTimed ::
     Num a
  => [(AbsoluteTime, a)] -- ^ list of ascending times and values
  -> Maybe (Timed a)
asTimed tas =
  if isAscending $ map fst tas
    then Just (Timed $ tas ++ [(AbsoluteTime (1 / 0), -1)])
    else Nothing

-- | Predicate to check if a list of orderable objects is in ascending order.
isAscending :: Ord a => [a] -> Bool
isAscending xs =
  case xs of
    [] -> True
    [_] -> True
    (x:y:xs') -> x <= y && isAscending (y : xs')

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
hasTime (Timed txs) = hasTime' txs

hasTime' :: [(AbsoluteTime, a)] -> AbsoluteTime -> Bool
hasTime' txs q =
  case txs of
    ((t, _):txs') -> t == q || hasTime' txs' q
    [] -> False

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
allTimes (Timed txs) = [t | (t,_) <- txs, not $ isInfiniteAbsoluteTime t]
