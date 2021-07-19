{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}

-- |
-- Module: Epidemic.Model.InhomogeneousBDSCOD
-- Copyright: (c) 2021 Alexander E. Zarebski
-- License: MIT
--
-- Maintainer: Alexander E. Zarebski <aezarebski@gmail.com>
-- Stability: unstable
-- Portability: ghc
--
-- This module defines a birth-death model with continuous time sampling and
-- scheduled sampling and rates that are piece-wise constant in time.
--
-- __Example:__ we will run a simulation for one unit of time and require that
-- there be at least two sequenced samples.
--
-- >>> simDuration = TimeDelta 1.0
-- >>> atLeastTwoSequences = True
--
-- The rates can change through time so we need to specify the times at which
-- they change. In this example the birth rate starts at 1.0 and then drops down
-- to 0.5. The other rates stay at their initial values.
--
-- >>> birthRateSpec = [(AbsoluteTime 0.0, 1.0), (AbsoluteTime 0.5, 0.5)]
-- >>> deathRateSpec = [(AbsoluteTime 0.0, 0.2)]
-- >>> sampRateSpec = [(AbsoluteTime 0.0, 0.1)]
-- >>> occRateSpec = [(AbsoluteTime 0.0, 0.1)]
--
-- There are a couple of scheduled samples with probabilities specified for
-- them, ie there will be a scheduled sample at time 0.9 where each lineage is
-- removed and sequenced individually with probability 0.1 and at times 0.5 and
-- 0.75 there is a scheduled sample where individuals are removed but /not/
-- sequenced with probabilities 0.4 and 0.5 respectively.
--
-- >>> seqSched = [(AbsoluteTime 0.9, 0.1)]
-- >>> unseqSched = [(AbsoluteTime 0.5, 0.4), (AbsoluteTime 0.75, 0.5)]
--
-- This is enough to define a 'SimulationConfiguration'. We will ignore the
-- possibility of using a termination handler for this example.
--
-- >>> ratesAndProbs = (birthRateSpec,deathRateSpec,sampRateSpec,seqSched,occRateSpec,unseqSched)
-- >>> (Just simConfig) = configuration simDuration atLeastTwoSequences Nothing ratesAndProbs
--
-- Then we can use this to generated a list of epidemic events in the simulation
--
-- >>> myEpidemicEvents = simulationWithSystem simConfig (allEvents randomEvent)
--
-- and from this we can extract the observations
--
-- >>> myObservedEvents = do
-- >>>   simState <- myEpidemicEvents
-- >>>   case simState of
-- >>>     Right es -> return $ observedEvents es
-- >>>     Left _ -> return $ Left "simulation terminated early"
--

module Epidemic.Model.InhomogeneousBDSCOD
  ( configuration
  , randomEvent
  , InhomBDSCODRates(..)
  , InhomBDSCODPop(..)
  , getNumRemovedByDeath
  , getNumRemovedBySampling
  , getNumRemovedByCatastrophe
  , getNumRemovedByOccurrence
  , getNumRemovedByDisaster
  ) where

import           Data.List                       as List
import           Data.Maybe                      (fromJust)
import qualified Data.Vector                     as V
import qualified Data.Vector.Generic             as G
import           Epidemic.Data.Events           (EpidemicEvent (..))
import           Epidemic.Data.Parameter
import           Epidemic.Data.Population
import           Epidemic.Data.Simulation       (SimulationConfiguration (..),
                                                  SimulationRandEvent (..),
                                                  TerminationHandler (..))
import           Epidemic.Data.Time             (AbsoluteTime (..),
                                                  TimeDelta (..), Timed (..),
                                                  allTimes, asTimed,
                                                  cadlagValue, maybeNextTimed)
import           Epidemic.Utility
import           System.Random.MWC
import           System.Random.MWC.Distributions (bernoulli, categorical)
import Control.Monad (replicateM)
import qualified Data.Set as Set
import Data.Coerce (coerce)

data InhomBDSCODRates =
  InhomBDSCODRates
    { irBirthRate       :: Timed Rate
    , irDeathRate       :: Timed Rate
    , irSamplingRate    :: Timed Rate
    , irCatastropheSpec :: Timed Probability
    , irOccurrenceRate  :: Timed Rate
    , irDisasterSpec    :: Timed Probability
    }
  deriving (Show, Eq)

-- | The population in which the epidemic occurs. This includes information
-- about the number of people that have previously been infected and
-- subsequently removed.
data InhomBDSCODPop =
  InhomBDSCODPop
  { ipInfectedPeople          :: People
  , ipNumRemovedByDeath       :: Int
  , ipNumRemovedBySampling    :: Int
  , ipNumRemovedByCatastrophe :: Int
  , ipNumRemovedByOccurrence  :: Int
  , ipNumRemovedByDisaster    :: Int
  } deriving (Show)

getNumRemovedByDeath :: InhomBDSCODPop -> Int
getNumRemovedByDeath = ipNumRemovedByDeath

getNumRemovedBySampling :: InhomBDSCODPop -> Int
getNumRemovedBySampling = ipNumRemovedBySampling

getNumRemovedByCatastrophe :: InhomBDSCODPop -> Int
getNumRemovedByCatastrophe = ipNumRemovedByCatastrophe

getNumRemovedByOccurrence :: InhomBDSCODPop -> Int
getNumRemovedByOccurrence = ipNumRemovedByOccurrence

getNumRemovedByDisaster :: InhomBDSCODPop -> Int
getNumRemovedByDisaster = ipNumRemovedByDisaster

instance ModelParameters InhomBDSCODRates InhomBDSCODPop where
  rNaught _ InhomBDSCODRates {..} time =
    do
      birthRate <- cadlagValue irBirthRate time
      deathRate <- cadlagValue irDeathRate time
      sampleRate <- cadlagValue irSamplingRate time
      occurrenceRate <- cadlagValue irOccurrenceRate time
      Just $ birthRate / (deathRate + sampleRate + occurrenceRate)
  eventRate _ InhomBDSCODRates {..} time =
    do
      birthRate <- cadlagValue irBirthRate time
      deathRate <- cadlagValue irDeathRate time
      sampleRate <- cadlagValue irSamplingRate time
      occurrenceRate <- cadlagValue irOccurrenceRate time
      Just $ birthRate + deathRate + sampleRate + occurrenceRate
  birthProb p inhomRates@InhomBDSCODRates {..} time =
    do
      birthRate <- cadlagValue irBirthRate time
      totalEventRate <- eventRate p inhomRates time
      Just $ birthRate / totalEventRate
  eventWeights _ InhomBDSCODRates{..} time =
    do
      birthRate <- cadlagValue irBirthRate time
      deathRate <- cadlagValue irDeathRate time
      sampleRate <- cadlagValue irSamplingRate time
      occurrenceRate <- cadlagValue irOccurrenceRate time
      Just $ V.fromList [birthRate, deathRate, sampleRate, occurrenceRate]

instance Population InhomBDSCODPop where
  susceptiblePeople _ = Nothing
  infectiousPeople = pure . ipInfectedPeople
  removedPeople _ = Nothing
  isInfected = not . nullPeople . ipInfectedPeople

-- | Configuration for the simulation of the inhomogeneous rates BDSCOD process.
configuration ::
     TimeDelta -- ^ Duration of the simulation after starting at time 0.
  -> Bool -- ^ condition upon at least two sequenced samples.
  -> Maybe (InhomBDSCODPop -> Bool, [EpidemicEvent] -> s) -- ^ values for termination handling.
  -> ( [(AbsoluteTime, Rate)]
     , [(AbsoluteTime, Rate)]
     , [(AbsoluteTime, Rate)]
     , [(AbsoluteTime, Probability)]
     , [(AbsoluteTime, Rate)]
     , [(AbsoluteTime, Probability)])
  -> Maybe (SimulationConfiguration InhomBDSCODRates InhomBDSCODPop s)
configuration maxTime atLeastCherry maybeTHFuncs (tBirthRate, tDeathRate, tSampleRate, cSpec, tOccurrenceRate, dSpec) =
  let (seedPerson, newId) = newPerson initialIdentifier
      bdscodPop = InhomBDSCODPop { ipInfectedPeople = asPeople [seedPerson]
                                 , ipNumRemovedByDeath = 0
                                 , ipNumRemovedBySampling = 0
                                 , ipNumRemovedByCatastrophe = 0
                                 , ipNumRemovedByOccurrence = 0
                                 , ipNumRemovedByDisaster = 0 }
   in do timedBirthRate <- asTimed tBirthRate
         timedDeathRate <- asTimed tDeathRate
         timedSamplingRate <- asTimed tSampleRate
         catastropheSpec <- asTimed cSpec
         timedOccurrenceRate <- asTimed tOccurrenceRate
         disasterSpec <- asTimed dSpec
         let irVal =
               InhomBDSCODRates
                 timedBirthRate
                 timedDeathRate
                 timedSamplingRate
                 catastropheSpec
                 timedOccurrenceRate
                 disasterSpec
             termHandler = do (f1, f2) <- maybeTHFuncs
                              return $ TerminationHandler f1 f2
         if maxTime > TimeDelta 0
           then Just
                  (SimulationConfiguration
                     irVal
                     bdscodPop
                     newId
                     (AbsoluteTime 0)
                     maxTime
                     termHandler
                     atLeastCherry)
           else Nothing

-- | A random event and the state afterwards
randomEvent :: SimulationRandEvent InhomBDSCODRates InhomBDSCODPop
randomEvent = SimulationRandEvent randomEvent'

randomEvent' ::
     InhomBDSCODRates
  -> AbsoluteTime -- ^ the current time
  -> InhomBDSCODPop -- ^ the population
  -> Identifier -- ^ current identifier
  -> GenIO -- ^ PRNG
  -> IO (AbsoluteTime, EpidemicEvent, InhomBDSCODPop, Identifier)
randomEvent' inhomRates@InhomBDSCODRates {..} currTime currPop currId gen =
  let (Just people) = infectiousPeople currPop
      popSize = fromIntegral $ numPeople people :: Double
      weightVecFunc = eventWeights currPop inhomRates
      -- we need a new step function to account for the population size.
      (Just stepFunction) =
        asTimed
          [ (t, popSize * fromJust (eventRate currPop inhomRates t))
          | t <- List.sort $ concatMap allTimes [irBirthRate, irDeathRate, irSamplingRate, irOccurrenceRate]
          ]
   in do (Just newEventTime) <- inhomExponential stepFunction currTime gen
         if noScheduledEvent currTime newEventTime (irCatastropheSpec <> irDisasterSpec)
           then do
             eventIx <- categorical (fromJust $ weightVecFunc newEventTime) gen
             (selectedPerson, unselectedPeople) <- randomPerson people gen
             return $
               case eventIx of
                 0 ->
                   ( newEventTime
                   , Infection newEventTime selectedPerson birthedPerson
                   , currPop { ipInfectedPeople = addPerson birthedPerson people}
                   , newId)
                   where (birthedPerson, newId) = newPerson currId
                 1 -> let currNumDeaths = ipNumRemovedByDeath currPop
                      in ( newEventTime
                         , Removal newEventTime selectedPerson
                         , currPop { ipInfectedPeople = unselectedPeople
                                   , ipNumRemovedByDeath = currNumDeaths + 1 }
                         , currId )
                 2 -> let currNumSampled = ipNumRemovedBySampling currPop
                      in ( newEventTime
                         , IndividualSample newEventTime selectedPerson True True
                         , currPop { ipInfectedPeople = unselectedPeople
                                   , ipNumRemovedBySampling = currNumSampled + 1 }
                         , currId)
                 3 -> let currNumOccurrence = ipNumRemovedByOccurrence currPop
                      in ( newEventTime
                         , IndividualSample newEventTime selectedPerson False True
                         , currPop { ipInfectedPeople = unselectedPeople
                                   , ipNumRemovedByOccurrence = currNumOccurrence + 1}
                         , currId)
                 _ -> error "no birth, death, sampling, or occurrence event selected."
           else case maybeNextTimed irCatastropheSpec irDisasterSpec currTime of
                  Just (disastTime, Right disastProb) ->
                    do (disastEvent, postDisastPop) <-
                         randomDisasterEvent
                         (disastTime, disastProb)
                         currPop
                         gen
                       return (disastTime, disastEvent, postDisastPop, currId)
                  Just (catastTime, Left catastProb) ->
                    do (catastEvent, postCatastPop) <-
                         randomCatastropheEvent
                         (catastTime, catastProb)
                         currPop
                         gen
                       return (catastTime, catastEvent, postCatastPop, currId)
                  Nothing -> error "Missing a next scheduled event when there should be one."

-- | Return a randomly sampled Catastrophe event and the population after that
-- event has occurred.
randomCatastropheEvent ::
     (AbsoluteTime, Probability) -- ^ Time and probability of sampling in the catastrophe
  -> InhomBDSCODPop -- ^ The state of the population prior to the catastrophe
  -> GenIO
  -> IO (EpidemicEvent, InhomBDSCODPop)
randomCatastropheEvent (catastTime, rhoProb) currPop gen =
  let (Just currPeople) = infectiousPeople currPop
  in do rhoBernoullis <- replicateM (numPeople currPeople) (bernoulli rhoProb gen)
        let setFilterZip pred a b = Set.fromList [x | p@(x, _) <- zip a b, pred p]
            currPersons = Set.toList $ coerce currPeople
            sampledPeople = People $ setFilterZip snd currPersons rhoBernoullis
            unsampledPeople = People $ setFilterZip (not . snd) currPersons rhoBernoullis
            currNumCatastrophe = ipNumRemovedByCatastrophe currPop
         in return ( PopulationSample catastTime sampledPeople True
                   , currPop { ipInfectedPeople = unsampledPeople
                             , ipNumRemovedByCatastrophe = currNumCatastrophe + numPeople sampledPeople })

-- | Return a randomly sampled Disaster event and the population after that
-- event has occurred.
randomDisasterEvent ::
     (AbsoluteTime, Probability) -- ^ Time and probability of sampling in the disaster
  -> InhomBDSCODPop -- ^ The state of the population prior to the disaster
  -> GenIO
  -> IO (EpidemicEvent, InhomBDSCODPop)
randomDisasterEvent (disastTime, nuProb) currPop gen = do
  let (Just currPeople) = infectiousPeople currPop
      currPersons = Set.toList $ coerce currPeople
  nuBernoullis <- replicateM (numPeople currPeople) (bernoulli nuProb gen)
  let setFilterZip pred a b = Set.fromList [x | p@(x, _) <- zip a b, pred p]
      sampledPeople = People $ setFilterZip snd currPersons nuBernoullis
      unsampledPeople = People $ setFilterZip (not . snd) currPersons nuBernoullis
      currNumDisaster = ipNumRemovedByDisaster currPop
   in return ( PopulationSample disastTime sampledPeople False
             , currPop { ipInfectedPeople = unsampledPeople
                       , ipNumRemovedByDisaster = currNumDisaster + numPeople sampledPeople })
