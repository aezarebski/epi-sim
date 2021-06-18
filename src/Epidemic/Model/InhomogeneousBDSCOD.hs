{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiParamTypeClasses #-}

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
-- This is enough to define a 'SimulationConfiguration'.
--
-- >>> ratesAndProbs = (birthRateSpec,deathRateSpec,sampRateSpec,seqSched,occRateSpec,unseqSched)
-- >>> (Just simConfig) = configuration simDuration atLeastTwoSequences ratesAndProbs
--
-- Then we can use this to generated a list of epidemic events in the simulation
--
-- >>> myEpidemicEvents = simulation simConfig (allEvents randomEvent)
--
-- and from this we can extract the observations
--
-- >>> myObservedEvents = observedEvents <$> myEpidemicEvents
--

module Epidemic.Model.InhomogeneousBDSCOD
  ( configuration
  , randomEvent
  , InhomBDSCODRates(..)
  , InhomBDSCODPop(..)
  ) where

import Data.Maybe (fromJust)
import qualified Data.Vector as V
import qualified Data.Vector.Generic as G
import Epidemic
import Epidemic.Types.Events (EpidemicEvent(..))
import Epidemic.Types.Parameter
import Epidemic.Types.Population
import Epidemic.Types.Simulation
  ( SimulationConfiguration(..)
  , SimulationRandEvent(..)
  )
import Epidemic.Types.Time
  ( AbsoluteTime(..)
  , TimeDelta(..)
  , Timed(..)
  , allTimes
  , asTimed
  , cadlagValue
  , maybeNextTimed
  )
import Epidemic.Utility
import System.Random.MWC
import System.Random.MWC.Distributions (bernoulli, categorical)

data InhomBDSCODRates =
  InhomBDSCODRates
    { irBirthRate :: Timed Rate
    , irDeathRate :: Timed Rate
    , irSamplingRate :: Timed Rate
    , irCatastropheSpec :: Timed Probability
    , irOccurrenceRate :: Timed Rate
    , irDisasterSpec :: Timed Probability
    }
  deriving (Show, Eq)

data InhomBDSCODPop =
  InhomBDSCODPop People
  deriving (Show)

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
  infectiousPeople (InhomBDSCODPop people) = Just people
  removedPeople _ = Nothing
  isInfected (InhomBDSCODPop people) = not $ nullPeople people

-- | Configuration of a inhomogeneous rates simulation.
configuration ::
     TimeDelta -- ^ Duration of the simulation after starting at time 0.
  -> Bool -- ^ condition upon at least two sequenced samples.
  -> ( [(AbsoluteTime, Rate)]
     , [(AbsoluteTime, Rate)]
     , [(AbsoluteTime, Rate)]
     , [(AbsoluteTime, Probability)]
     , [(AbsoluteTime, Rate)]
     , [(AbsoluteTime, Probability)])
  -> Maybe (SimulationConfiguration InhomBDSCODRates InhomBDSCODPop)
configuration maxTime atLeastCherry (tBirthRate, tDeathRate, tSampleRate, cSpec, tOccurrenceRate, dSpec) =
  let (seedPerson, newId) = newPerson initialIdentifier
      bdscodPop = InhomBDSCODPop $ asPeople [seedPerson]
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
         if maxTime > TimeDelta 0
           then Just
                  (SimulationConfiguration
                     irVal
                     bdscodPop
                     newId
                     (AbsoluteTime 0)
                     maxTime
                     Nothing
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
randomEvent' inhomRates@InhomBDSCODRates {..} currTime currPop@(InhomBDSCODPop people) currId gen =
  let popSize = fromIntegral $ numPeople people :: Double
      weightVecFunc = eventWeights currPop inhomRates
      -- we need a new step function to account for the population size.
      (Just stepFunction) =
        asTimed
          [ (t, popSize * fromJust (eventRate currPop inhomRates t))
          | t <- sort $ concatMap allTimes [irBirthRate, irDeathRate, irSamplingRate, irOccurrenceRate]
          ]
   in do (Just newEventTime) <- inhomExponential stepFunction currTime gen
         if noScheduledEvent currTime newEventTime (irCatastropheSpec <> irDisasterSpec)
           then do
             eventIx <- categorical (fromJust $ weightVecFunc newEventTime) gen -- select from the continuous time samples
             (selectedPerson, unselectedPeople) <- randomPerson people gen
             return $
               case eventIx of
                 0 ->
                   ( newEventTime
                   , Infection newEventTime selectedPerson birthedPerson
                   , InhomBDSCODPop (addPerson birthedPerson people)
                   , newId)
                   where (birthedPerson, newId) = newPerson currId
                 1 ->
                   ( newEventTime
                   , Removal newEventTime selectedPerson
                   , InhomBDSCODPop unselectedPeople
                   , currId)
                 2 ->
                   ( newEventTime
                   , IndividualSample newEventTime selectedPerson True
                   , InhomBDSCODPop unselectedPeople
                   , currId)
                 3 ->
                   ( newEventTime
                   , IndividualSample newEventTime selectedPerson False
                   , InhomBDSCODPop unselectedPeople
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

-- | Return a randomly sampled Catastrophe event
-- TODO Move this into the epidemic module to keep things DRY.
randomCatastropheEvent ::
     (AbsoluteTime, Probability) -- ^ Time and probability of sampling in the catastrophe
  -> InhomBDSCODPop -- ^ The state of the population prior to the catastrophe
  -> GenIO
  -> IO (EpidemicEvent, InhomBDSCODPop)
randomCatastropheEvent (catastTime, rhoProb) (InhomBDSCODPop (People currPeople)) gen = do
  rhoBernoullis <- G.replicateM (V.length currPeople) (bernoulli rhoProb gen)
  let filterZip predicate a b = fst . V.unzip . V.filter predicate $ V.zip a b
      sampledPeople = filterZip snd currPeople rhoBernoullis
      unsampledPeople = filterZip (not . snd) currPeople rhoBernoullis
   in return
        ( PopulationSample catastTime (People sampledPeople) True
        , InhomBDSCODPop (People unsampledPeople))

-- | Return a randomly sampled Disaster event
-- TODO Move this into the epidemic module to keep things DRY.
randomDisasterEvent ::
     (AbsoluteTime, Probability) -- ^ Time and probability of sampling in the disaster
  -> InhomBDSCODPop -- ^ The state of the population prior to the disaster
  -> GenIO
  -> IO (EpidemicEvent, InhomBDSCODPop)
randomDisasterEvent (disastTime, nuProb) (InhomBDSCODPop (People currPeople)) gen = do
  nuBernoullis <- G.replicateM (V.length currPeople) (bernoulli nuProb gen)
  let filterZip predicate a b = fst . V.unzip . V.filter predicate $ V.zip a b
      sampledPeople = filterZip snd currPeople nuBernoullis
      unsampledPeople = filterZip (not . snd) currPeople nuBernoullis
   in return
        ( PopulationSample disastTime (People sampledPeople) False
        , InhomBDSCODPop (People unsampledPeople))
