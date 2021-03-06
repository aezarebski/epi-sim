{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Epidemic.Utility where

import Control.Applicative
import Control.Monad (liftM)
import Control.Monad.Primitive (PrimMonad, PrimState)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as Char8
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Data.Vector as V
import Epidemic
import Epidemic.Types.Events
import Epidemic.Types.Parameter
import Epidemic.Types.Population
import Epidemic.Types.Simulation
import Epidemic.Types.Time
  ( AbsoluteTime(..)
  , Timed(..)
  , TimeDelta(..)
  , diracDeltaValue
  , nextTime
  , cadlagValue
  , timeAfterDelta
  )
import GHC.Generics (Generic)
import System.Random.MWC
import System.Random.MWC.Distributions (exponential)


initialIdentifier :: Identifier
initialIdentifier = Identifier 1

newPerson :: Identifier -> (Person, Identifier)
newPerson idntty@(Identifier idInt) = (Person idntty, Identifier (idInt + 1))

selectElem :: V.Vector a -> Int -> (a, V.Vector a)
selectElem v n
  | n == 0 = (V.head v, V.tail v)
  | otherwise =
    let (foo, bar) = V.splitAt n v
     in (V.head bar, foo V.++ (V.tail bar))

randomPerson :: People -> GenIO -> IO (Person, People)
randomPerson people@(People persons) gen = do
  u <- uniform gen
  let personIx = floor (u * (fromIntegral $ numPeople people :: Double))
      (person, remPeople) = selectElem persons personIx
   in return (person, People remPeople)

type NName = Maybe String

type NLength = Maybe Double

data NBranch =
  NBranch NSubtree NLength
  deriving (Eq)

instance Show NBranch where
  show (NBranch st (Just l)) = show st ++ ":" ++ show l
  show (NBranch st Nothing) = show st

data NBranchSet =
  NBranchSet [NBranch]
  deriving (Eq)

instance Show NBranchSet where
  show (NBranchSet bs) = "(" ++ (List.intercalate "," (map show bs)) ++ ")"

data NSubtree
  = NLeaf NName
  | NInternal NBranchSet
  deriving (Eq)

instance Show NSubtree where
  show (NLeaf (Just n)) = n
  show (NLeaf Nothing) = ""
  show (NInternal bs) = show bs

data NTree =
  NTree [NBranch]
  deriving (Eq)

instance Show NTree where
  show (NTree bs) = show (NBranchSet bs) ++ ";"

-- | Example run
--   > (Success foo) = parseString newickTree mempty "((foo:1.1,bar:1.2):1.3,baz:1.4);"
--   > (Success bar) = parseString newickTree mempty $ show foo
--   > foo == bar
--   True
sort :: Ord a => [a] -> [a]
sort = List.sort

count' :: (a -> Bool) -> [a] -> Int
count' p = go 0
  where
    go n [] = n
    go n (x:xs)
      | p x = go (n + 1) xs
      | otherwise = go n xs

-- | Run a simulation described by a configuration object with the provided
-- PRNG.
simulationWithGenIO ::
     (ModelParameters a b, Population b)
  => SimulationConfiguration a b
  -> (a -> AbsoluteTime -> Maybe (b -> Bool) -> SimulationState b -> GenIO -> IO (SimulationState b))
  -> GenIO
  -> IO [EpidemicEvent]
simulationWithGenIO config@SimulationConfiguration {..} allEventsFunc gen =
  if scRequireCherry
    then do
      simulation' config allEventsFunc gen
    else do
      SimulationState (_, events, _, _) <-
        allEventsFunc
          scRates
          (timeAfterDelta scStartTime scSimDuration)
          scValidPopulation
          (SimulationState (AbsoluteTime 0, [], scPopulation, scNewIdentifier))
          gen
      return $ sort events

-- | Run a simulation described by a configuration object using the fixed PRNG
-- that is hardcoded in the @mwc-random@ package.
simulation ::
     (ModelParameters a b, Population b)
  => SimulationConfiguration a b
  -> (a -> AbsoluteTime -> Maybe (b -> Bool) -> SimulationState b -> GenIO -> IO (SimulationState b))
  -> IO [EpidemicEvent]
simulation config allEventsFunc = do
  gen <- System.Random.MWC.create :: IO GenIO
  simulationWithGenIO config allEventsFunc gen

-- | Predicate for whether an epidemic event will appear as a leaf in the
-- reconstructed tree.
isReconTreeLeaf :: EpidemicEvent -> Bool
isReconTreeLeaf e =
  case e of
    IndividualSample {..} -> indSampSeq
    PopulationSample {..} -> popSampSeq
    _ -> False

-- | Simulation conditioned upon there being at least two sequenced samples.
-- NOTE This function is deprecated and will be removed in future versions.
simulation' ::
     (ModelParameters a b, Population b)
  => SimulationConfiguration a b
  -> (a -> AbsoluteTime -> Maybe (b -> Bool) -> SimulationState b -> GenIO -> IO (SimulationState b))
  -> GenIO
  -> IO [EpidemicEvent]
simulation' config@SimulationConfiguration {..} allEventsFunc gen = do
  SimulationState (_, events, _, _) <-
    allEventsFunc
      scRates
      (timeAfterDelta scStartTime scSimDuration)
      scValidPopulation
      (SimulationState (AbsoluteTime 0, [], scPopulation, scNewIdentifier))
      gen
  if count' isReconTreeLeaf events >= 2
    then return $ sort events
    else simulation' config allEventsFunc gen

-- | Run a simulation described by a configuration object but using a random
-- seed generated by the system rather than a seed
simulationWithSystemRandom ::
     (ModelParameters a b, Population b)
  => SimulationConfiguration a b
  -> (a -> AbsoluteTime -> Maybe (b -> Bool) -> SimulationState b -> GenIO -> IO (SimulationState b))
  -> IO [EpidemicEvent]
simulationWithSystemRandom config@SimulationConfiguration {..} allEventsFunc = do
  SimulationState (_, events, _, _) <-
    withSystemRandom $ \g ->
      allEventsFunc
        scRates
        (timeAfterDelta scStartTime scSimDuration)
        scValidPopulation
        (SimulationState (AbsoluteTime 0, [], scPopulation, scNewIdentifier))
        g
  if scRequireCherry
    then (if count' isReconTreeLeaf events >= 2
            then return $ sort events
            else simulationWithSystemRandom config allEventsFunc)
    else return $ sort events

-- | The number of lineages at the end of a simulation.
finalSize ::
     [EpidemicEvent] -- ^ The events from the simulation
  -> Integer
finalSize = foldl (\x y -> x + eventPopDelta y) 1

-- | Generate exponentially distributed random variates with inhomogeneous rate
-- starting from a particular point in time.
--
-- Assuming the @stepFunc@ is the intensity of arrivals and @t0@ is the start
-- time this returns @t1@ the time of the next arrival.
inhomExponential ::
     PrimMonad m
  => Timed Double -- ^ Step function
  -> AbsoluteTime -- ^ Start time
  -> Gen (PrimState m) -- ^ Generator
  -> m (Maybe AbsoluteTime)
inhomExponential stepFunc t0 = randInhomExp t0 stepFunc

-- | Generate exponentially distributed random variates with inhomogeneous rate.
--
-- __TODO__ The algorithm used here generates more variates than are needed. It
-- would be nice to use a more efficient implementation.
--
randInhomExp ::
     PrimMonad m
  => AbsoluteTime -- ^ Timer
  -> Timed Double -- ^ Step function
  -> Gen (PrimState m) -- ^ Generator.
  -> m (Maybe AbsoluteTime)
randInhomExp crrT stepFunc gen =
  let crrR = cadlagValue stepFunc crrT
      nxtT = nextTime stepFunc crrT
   in if Maybe.isJust crrR && Maybe.isJust nxtT
        then do
          crrD <- exponential (Maybe.fromJust crrR) gen
          let propT = timeAfterDelta crrT (TimeDelta crrD)
          if propT < Maybe.fromJust nxtT
            then return $ Just propT
            else randInhomExp (Maybe.fromJust nxtT) stepFunc gen
        else return Nothing

-- | Helper function for converting between the Maybe monad and the Either
-- monad.
maybeToRight :: a -> Maybe b -> Either a b
maybeToRight a maybeB =
  case maybeB of
    (Just b) -> Right b
    Nothing -> Left a
