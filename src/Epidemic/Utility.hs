{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Epidemic.Utility ( initialIdentifier
                        , inhomExponential
                        , randomPerson
                        , maybeToRight
                        , newPerson
                        , isReconTreeLeaf
                        , simulationWithSystem
                        , simulationWithFixedSeed
                        , simulationWithGenIO
                        ) where

import           Control.Monad.Primitive         (PrimMonad, PrimState)
import qualified Data.List                       as List
import qualified Data.Maybe                      as Maybe
import qualified Data.Vector                     as V
import           Epidemic
import           Epidemic.Types.Events
import           Epidemic.Types.Parameter
import           Epidemic.Types.Population
import           Epidemic.Types.Simulation
import           Epidemic.Types.Time             (AbsoluteTime (..),
                                                  TimeDelta (..), Timed (..),
                                                  cadlagValue, nextTime,
                                                  timeAfterDelta)
import           System.Random.MWC
import           System.Random.MWC.Distributions (exponential)


initialIdentifier :: Identifier
initialIdentifier = Identifier 1

-- | A new person constructed from the given identifier and a new identifier.
newPerson :: Identifier -> (Person, Identifier)
newPerson idntty@(Identifier idInt) = (Person idntty, Identifier (idInt + 1))

-- | An element of a vector and the vector with that element removed.
selectElem :: V.Vector a -> Int -> (a, V.Vector a)
selectElem v n
  | n == 0 = (V.head v, V.tail v)
  | otherwise =
    let (foo, bar) = V.splitAt n v
     in (V.head bar, foo V.++ (V.tail bar))

-- | A random person and the remaining group of people after they have been
-- sampled with removal.
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
  show (NBranch st Nothing)  = show st

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
  show (NLeaf Nothing)  = ""
  show (NInternal bs)   = show bs

data NTree =
  NTree [NBranch]
  deriving (Eq)

instance Show NTree where
  show (NTree bs) = show (NBranchSet bs) ++ ";"

-- | The number of elements of the list that map to @True@ under the predicate.
count' :: (a -> Bool) -> [a] -> Int
count' p xs = sum [if p x then 1 else 0 | x <- xs]

-- | Run a simulation described by a configuration object and the model's
-- @allEvents@ style function (see the example in
-- "Epidemic.Model.InhomogeneousBDSCOD") using the provided PRNG.
simulationWithGenIO ::
     (ModelParameters a b, Population b)
  => SimulationConfiguration a b c
  -> (a -> AbsoluteTime -> Maybe (TerminationHandler b c) -> SimulationState b c -> GenIO -> IO (SimulationState b c))
  -> GenIO
  -> IO [EpidemicEvent]
simulationWithGenIO config@SimulationConfiguration {..} allEventsFunc gen =
  if scRequireCherry
    then
      simulationAtLeastCherry config allEventsFunc gen
    else do
      SimulationState (_, events, _, _) <-
        allEventsFunc
          scRates
          (timeAfterDelta scStartTime scSimDuration)
          scTerminationHandler
          (SimulationState (scStartTime, [], scPopulation, scNewIdentifier))
          gen
      return $ List.sort events

-- | Run a simulation using a fixed PRNG random seed.
simulationWithFixedSeed ::
     (ModelParameters a b, Population b)
  => SimulationConfiguration a b c
  -> (a -> AbsoluteTime -> Maybe (TerminationHandler b c) -> SimulationState b c -> GenIO -> IO (SimulationState b c))
  -> IO [EpidemicEvent]
simulationWithFixedSeed config allEventsFunc = do
  gen <- genIOFromFixed
  simulationWithGenIO config allEventsFunc gen

-- | Simulation conditioned upon there being at least two sequenced samples.
simulationAtLeastCherry ::
     (ModelParameters a b, Population b)
  => SimulationConfiguration a b c
  -> (a -> AbsoluteTime -> Maybe (TerminationHandler b c) -> SimulationState b c -> GenIO -> IO (SimulationState b c))
  -> GenIO
  -> IO [EpidemicEvent]
simulationAtLeastCherry config@SimulationConfiguration {..} allEventsFunc gen = do
  SimulationState (_, events, _, _) <-
    allEventsFunc
      scRates
      (timeAfterDelta scStartTime scSimDuration)
      scTerminationHandler
      (SimulationState (scStartTime, [], scPopulation, scNewIdentifier))
      gen
  if count' isReconTreeLeaf events >= 2
    then return $ List.sort events
    else simulationAtLeastCherry config allEventsFunc gen

-- | Run a simulation described by a configuration object but using a random
-- seed generated by the system rather than a seed
simulationWithSystem ::
     (ModelParameters a b, Population b)
  => SimulationConfiguration a b c
  -> (a -> AbsoluteTime -> Maybe (TerminationHandler b c) -> SimulationState b c -> GenIO -> IO (SimulationState b c))
  -> IO [EpidemicEvent]
simulationWithSystem config@SimulationConfiguration {..} allEventsFunc = do
  SimulationState (_, events, _, _) <-
    withSystemRandom $ \g ->
      allEventsFunc
        scRates
        (timeAfterDelta scStartTime scSimDuration)
        scTerminationHandler
        (SimulationState (scStartTime, [], scPopulation, scNewIdentifier))
        g
  if scRequireCherry
    then (if count' isReconTreeLeaf events >= 2
            then return $ List.sort events
            else simulationWithSystem config allEventsFunc)
    else return $ List.sort events

-- | Predicate for whether an epidemic event will appear as a leaf in the
-- reconstructed tree. For scheduled sequenced samples this will only return
-- true if there was at least one lineage observed.
isReconTreeLeaf :: EpidemicEvent -> Bool
isReconTreeLeaf e =
  case e of
    IndividualSample {..} -> indSampSeq
    PopulationSample {..} -> popSampSeq && not (nullPeople popSampPeople)
    _                     -> False

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
    Nothing  -> Left a
