{-# LANGUAGE DeriveGeneric #-}

module Epidemic.Types.Parameter where

import qualified Data.Aeson as Json
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import GHC.Generics

type Time = Double

-- | Type containing values at times. The times are increasing as required by
-- @asTimed@.
newtype Timed a =
  Timed [(Time, a)]
  deriving (Generic, Eq, Show)

instance Json.FromJSON a => Json.FromJSON (Timed a)

instance Json.ToJSON a => Json.ToJSON (Timed a)

instance Semigroup (Timed a) where
  (Timed x) <> (Timed y) = Timed $ List.sortOn fst (x ++ y)

type Rate = Double

type Probability = Double

-- | Construct a timed list if possible.
asTimed :: Num a
        => [(Time,a)] -- ^ list of ascending times and values
        -> Maybe (Timed a)
asTimed tas = if isAscending $ map fst tas then Just (Timed $ tas ++ [(1e100,-1)]) else Nothing

-- | Predicate to check if a list of orderable objects is in ascending order.
isAscending :: Ord a => [a] -> Bool
isAscending xs = case xs of
  [] -> True
  [_] -> True
  (x:y:xs') -> x <= y && isAscending (y:xs')

-- | Evaluate the timed object treating it as a cadlag function
cadlagValue :: Timed a -> Time -> Maybe a
cadlagValue (Timed txs) = cadlagValue' txs


cadlagValue' :: [(Time,a)] -> Time -> Maybe a
cadlagValue' [] _ = Nothing
cadlagValue' ((t, x):txs) q =
  if q < t
    then Nothing
    else let nextCLV = cadlagValue' txs q
          in if Maybe.isNothing nextCLV
               then Just x
               else nextCLV


-- | Evaluate the timed object treating it as a direct delta function
diracDeltaValue :: Timed a -> Time -> Maybe a
diracDeltaValue (Timed txs) = diracDeltaValue' txs

diracDeltaValue' :: [(Time,a)] -> Time -> Maybe a
diracDeltaValue' txs q = case txs of
  ((t,x):txs') -> if t == q then Just x else diracDeltaValue' txs' q
  [] -> Nothing

-- | Check if there exists a pair with a particular time index.
hasTime :: Timed a -> Time -> Bool
hasTime (Timed txs) = hasTime' txs

hasTime' :: [(Time,a)] -> Time -> Bool
hasTime' txs q = case txs of
  ((t,_):txs') -> t == q || hasTime' txs' q
  [] -> False

-- | Return the value of the next time if possible or an exact match if it
-- exists.
nextTime :: Timed a -> Time -> Maybe Time
nextTime (Timed txs) = nextTime' txs

nextTime' :: [(Time,a)] -> Time -> Maybe Time
nextTime' txs q = case txs of
  ((t,_):txs') -> if q < t then Just t else nextTime' txs' q
  [] -> Nothing
