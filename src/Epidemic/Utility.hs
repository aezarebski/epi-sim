{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Epidemic.Utility ( allEvents
                        , inhomExponential
                        , randomPerson
                        , maybeToRight
                        , infectedBy
                        , isReconTreeLeaf
                        , simulationWithSystem
                        , simulationWithFixedSeed
                        , simulationWithGenIO
                        ) where

import           Control.Monad.Primitive         (PrimMonad, PrimState)
import qualified Data.List                       as List
import qualified Data.Maybe                      as Maybe
import qualified Data.Vector                     as V
import           Epidemic.Data.Events
import           Epidemic.Data.Parameter
import           Epidemic.Data.Population
import           Epidemic.Data.Simulation
import           Epidemic.Data.Time             (AbsoluteTime (..),
                                                  TimeDelta (..), Timed (..),
                                                  cadlagValue, nextTime,
                                                  timeAfterDelta)
import           System.Random.MWC
import           System.Random.MWC.Distributions (exponential)
import qualified Data.Set as Set

-- | A random person and the remaining group of people after they have been
-- sampled with removal.
randomPerson :: People -> GenIO -> IO (Person, People)
randomPerson people@(People persons) gen = do
  randIx <- uniformR (0, numPeople people - 1) gen
  return (Set.elemAt randIx persons, People $ Set.deleteAt randIx persons)

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
countTrues :: (a -> Bool) -> [a] -> Int
countTrues p xs = sum [if p x then 1 else 0 | x <- xs]

-- | Run a simulation described by a configuration object and the model's
-- @allEvents@ style function (see the example in
-- "Epidemic.Model.InhomogeneousBDSCOD") using the provided PRNG.
simulationWithGenIO ::
     (ModelParameters a b, Population b)
  => SimulationConfiguration a b c
  -> (a -> AbsoluteTime -> Maybe (TerminationHandler b c) -> SimulationState b c -> GenIO -> IO (SimulationState b c))
  -> GenIO
  -> IO (Either (Maybe c) [EpidemicEvent])
simulationWithGenIO config@SimulationConfiguration {..} allEventsFunc gen =
  if scRequireCherry
    then
      simulationAtLeastCherry config allEventsFunc gen
    else do
      simState <-
        allEventsFunc
          scRates
          (timeAfterDelta scStartTime scSimDuration)
          scTerminationHandler
          (SimulationState (scStartTime, [], scPopulation, scNewIdentifier))
          gen
      return $ case simState of
                 SimulationState (_, events, _, _) -> Right $ List.sort events
                 TerminatedSimulation maybeSummary -> Left maybeSummary

-- | Run a simulation using a fixed PRNG random seed.
simulationWithFixedSeed ::
     (ModelParameters a b, Population b)
  => SimulationConfiguration a b c
  -> (a -> AbsoluteTime -> Maybe (TerminationHandler b c) -> SimulationState b c -> GenIO -> IO (SimulationState b c))
  -> IO (Either (Maybe c) [EpidemicEvent])
simulationWithFixedSeed config allEventsFunc = do
  gen <- genIOFromFixed
  simulationWithGenIO config allEventsFunc gen

-- | Simulation conditioned upon there being at least two sequenced samples.
simulationAtLeastCherry ::
     (ModelParameters a b, Population b)
  => SimulationConfiguration a b c
  -> (a -> AbsoluteTime -> Maybe (TerminationHandler b c) -> SimulationState b c -> GenIO -> IO (SimulationState b c))
  -> GenIO
  -> IO (Either (Maybe c) [EpidemicEvent])
simulationAtLeastCherry config@SimulationConfiguration {..} allEventsFunc gen = do
  simState <-
    allEventsFunc
      scRates
      (timeAfterDelta scStartTime scSimDuration)
      scTerminationHandler
      (SimulationState (scStartTime, [], scPopulation, scNewIdentifier))
      gen
  case simState of
    SimulationState (_, events, _, _) -> 
      if countTrues isReconTreeLeaf events >= 2
      then return $ Right $ List.sort events
      else simulationAtLeastCherry config allEventsFunc gen
    TerminatedSimulation maybeSummary -> return $ Left maybeSummary

-- | Run a simulation described by a configuration object but using a random
-- seed generated by the system rather than a seed
simulationWithSystem ::
     (ModelParameters a b, Population b)
  => SimulationConfiguration a b c
  -> (a -> AbsoluteTime -> Maybe (TerminationHandler b c) -> SimulationState b c -> GenIO -> IO (SimulationState b c))
  -> IO (Either (Maybe c) [EpidemicEvent])
simulationWithSystem config@SimulationConfiguration {..} allEventsFunc = do
  simState <-
    withSystemRandom $ \g ->
      allEventsFunc
        scRates
        (timeAfterDelta scStartTime scSimDuration)
        scTerminationHandler
        (SimulationState (scStartTime, [], scPopulation, scNewIdentifier))
        g
  case simState of
    SimulationState (_, events, _, _) ->
      if scRequireCherry
      then (if countTrues isReconTreeLeaf events >= 2
             then return $ Right $ List.sort events
             else simulationWithSystem config allEventsFunc)
      else return $ Right $ List.sort events
    TerminatedSimulation maybeSummary -> return $ Left maybeSummary

-- | Predicate for whether an epidemic event will appear as a leaf in the
-- reconstructed tree. For scheduled sequenced samples this will only return
-- true if there was at least one lineage observed.
isReconTreeLeaf :: EpidemicEvent -> Bool
isReconTreeLeaf e =
  case e of
    IndividualSample {..} -> indSampSeq
    PopulationSample {..} -> popSampSeq && not (nullPeople popSampPeople)
    _                     -> False

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

-- | The people infected by a particular person in a list of events.
infectedBy ::
     Person -- ^ Potential infector
  -> [EpidemicEvent] -- ^ Events
  -> People
infectedBy person events =
  case events of
    [] -> People Set.empty
    (Infection _ infector infectee:es) ->
      if infector == person
        then addPerson infectee $ infectedBy person es
        else infectedBy person es
    (_:es) -> infectedBy person es


-- | Run the simulation until the specified stopping time and return a
-- @SimulationState@ which holds the history of the simulation.
allEvents ::
     (ModelParameters a b, Population b)
  => SimulationRandEvent a b
  -> a
  -> AbsoluteTime -- ^ time at which to stop the simulation
  -> Maybe (TerminationHandler b c)
  -> SimulationState b c -- ^ the initial/current state of the simulation
  -> GenIO
  -> IO (SimulationState b c)
allEvents _ _ _ _ ts@(TerminatedSimulation _) _ = return ts
allEvents (SimulationRandEvent randEvent) modelParams maxTime maybeTermHandler (SimulationState (currTime, currEvents, currPop, currId)) gen =
  let isNotTerminated = case maybeTermHandler of
        Nothing                                   -> const True
        Just (TerminationHandler hasTerminated _) -> not . hasTerminated
  in if isNotTerminated currPop
     then if isInfected currPop
           then do
             (newTime, event, newPop, newId) <-
               randEvent modelParams currTime currPop currId gen
             if newTime < maxTime
               then allEvents
                      (SimulationRandEvent randEvent)
                      modelParams
                      maxTime
                      maybeTermHandler
                      (SimulationState
                         (newTime, event : currEvents, newPop, newId))
                      gen
               else return $
                    SimulationState
                      ( maxTime
                      , StoppingTime maxTime : currEvents
                      , currPop
                      , currId)
           else return $
                SimulationState
                  ( currTime
                  , Extinction currTime : currEvents
                  , currPop
                  , currId)
     else return . TerminatedSimulation $ do TerminationHandler _ termSummary <- maybeTermHandler
                                             return $ termSummary currEvents
