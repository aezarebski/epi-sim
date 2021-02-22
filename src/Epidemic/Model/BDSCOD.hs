{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Epidemic.Model.BDSCOD
  ( configuration
  , randomEvent
  , BDSCODParameters(..)
  , BDSCODPopulation(..)
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
  , cadlagValue
  , diracDeltaValue
  , nextTime
  , timeAfterDelta
  , allTimes
  , asTimed
  )
import Epidemic.Utility
import System.Random.MWC
import System.Random.MWC.Distributions (bernoulli, categorical, exponential)

data BDSCODParameters
  -- | birth rate, death rate, sampling rate, catastrophe specification, occurrence rate and disaster specification
      =
  BDSCODParameters Rate Rate Rate (Timed Probability) Rate (Timed Probability)

data BDSCODPopulation =
  BDSCODPopulation People
  deriving (Show)

instance ModelParameters BDSCODParameters BDSCODPopulation where
  rNaught _ (BDSCODParameters birthRate deathRate samplingRate _ occurrenceRate _) _ =
    Just $ birthRate / (deathRate + samplingRate + occurrenceRate)
  eventRate _ (BDSCODParameters birthRate deathRate samplingRate _ occurrenceRate _) _ =
    Just $ birthRate + deathRate + samplingRate + occurrenceRate
  birthProb _ (BDSCODParameters birthRate deathRate samplingRate _ occurrenceRate _) _ =
    Just $ birthRate / (birthRate + deathRate + samplingRate + occurrenceRate)

instance Population BDSCODPopulation where
  susceptiblePeople _ = Nothing
  infectiousPeople (BDSCODPopulation people) = Just people
  removedPeople _ = Nothing
  isInfected (BDSCODPopulation (People people)) = not $ V.null people

-- | Configuration of a birth-death-sampling-occurrence simulation
configuration ::
     TimeDelta -- ^ Duration of the simulation
  -> ( Rate
     , Rate
     , Rate
     , [(AbsoluteTime, Probability)]
     , Rate
     , [(AbsoluteTime, Probability)]
      ) -- ^ Birth, Death, Sampling, Catastrophe probability and Occurrence rates
  -> Maybe (SimulationConfiguration BDSCODParameters BDSCODPopulation)
configuration maxTime (birthRate, deathRate, samplingRate, catastropheSpec, occurrenceRate, disasterSpec) = do
  catastropheSpec' <- asTimed catastropheSpec
  disasterSpec' <- asTimed disasterSpec
  let bdscodParams =
        BDSCODParameters
          birthRate
          deathRate
          samplingRate
          catastropheSpec'
          occurrenceRate
          disasterSpec'
      (seedPerson, newId) = newPerson initialIdentifier
      bdscodPop = BDSCODPopulation (People $ V.singleton seedPerson)
   in return $
      SimulationConfiguration
        bdscodParams
        bdscodPop
        newId
        (AbsoluteTime 0)
        maxTime
        Nothing

-- | The way in which random events are generated in this model.
randomEvent :: SimulationRandEvent BDSCODParameters BDSCODPopulation
randomEvent = SimulationRandEvent randomEvent'

-- | Return a random event from the BDSCOD-process given the current state of the process.
randomEvent' ::
     BDSCODParameters -- ^ Parameters of the process
  -> AbsoluteTime -- ^ The current time within the process
  -> BDSCODPopulation -- ^ The current state of the populaion
  -> Identifier -- ^ The current state of the identifier generator
  -> GenIO -- ^ The current state of the PRNG
  -> IO (AbsoluteTime, EpidemicEvent, BDSCODPopulation, Identifier)
randomEvent' params@(BDSCODParameters br dr sr catastInfo occr disastInfo) currTime currPop@(BDSCODPopulation currPeople) currId gen =
  let netEventRate = fromJust $ eventRate currPop params currTime
      eventWeights = V.fromList [br, dr, sr, occr]
   in do delay <-
           exponential (fromIntegral (numPeople currPeople) * netEventRate) gen
         let newEventTime = timeAfterDelta currTime (TimeDelta delay)
         if noScheduledEvent currTime newEventTime (catastInfo <> disastInfo)
           then do
             eventIx <- categorical eventWeights gen
             (selectedPerson, unselectedPeople) <- randomPerson currPeople gen
             return $
               case eventIx of
                 0 ->
                   let (birthedPerson, newId) = newPerson currId
                       infEvent =
                         Infection newEventTime selectedPerson birthedPerson
                    in ( newEventTime
                       , infEvent
                       , BDSCODPopulation (addPerson birthedPerson currPeople)
                       , newId)
                 1 ->
                   ( newEventTime
                   , Removal newEventTime selectedPerson
                   , BDSCODPopulation unselectedPeople
                   , currId)
                 2 ->
                   ( newEventTime
                   , Sampling newEventTime selectedPerson
                   , BDSCODPopulation unselectedPeople
                   , currId)
                 3 ->
                   ( newEventTime
                   , Occurrence newEventTime selectedPerson
                   , BDSCODPopulation unselectedPeople
                   , currId)
                 _ ->
                   error "no birth, death, sampling, occurrence event selected."
           else if noScheduledEvent currTime newEventTime catastInfo
                  then let (Just (disastTime, disastProb)) =
                             firstScheduled currTime disastInfo
                        in do (disastEvent, postDisastPop) <-
                                randomDisasterEvent
                                  (disastTime, disastProb)
                                  currPop
                                  gen
                              return
                                (disastTime, disastEvent, postDisastPop, currId)
                  else if noScheduledEvent currTime newEventTime disastInfo
                         then let (Just (catastTime, catastProb)) =
                                    firstScheduled currTime catastInfo
                               in do (catastEvent, postCatastPop) <-
                                       randomCatastropheEvent
                                         (catastTime, catastProb)
                                         currPop
                                         gen
                                     return
                                       ( catastTime
                                       , catastEvent
                                       , postCatastPop
                                       , currId)
                         else let (Just (catastTime, catastProb)) =
                                    firstScheduled currTime catastInfo
                                  (Just (disastTime, disastProb)) =
                                    firstScheduled currTime disastInfo
                               in do (scheduledEvent, postEventPop) <-
                                       if catastTime < disastTime
                                         then randomCatastropheEvent
                                                (catastTime, catastProb)
                                                currPop
                                                gen
                                         else randomDisasterEvent
                                                (disastTime, disastProb)
                                                currPop
                                                gen
                                     return
                                       ( min catastTime disastTime
                                       , scheduledEvent
                                       , postEventPop
                                       , currId)

-- | Return a randomly sampled Catastrophe event
randomCatastropheEvent ::
     (AbsoluteTime, Probability) -- ^ Time and probability of sampling in the catastrophe
  -> BDSCODPopulation -- ^ The state of the population prior to the catastrophe
  -> GenIO
  -> IO (EpidemicEvent, BDSCODPopulation)
randomCatastropheEvent (catastTime, rhoProb) (BDSCODPopulation (People currPeople)) gen = do
  rhoBernoullis <- G.replicateM (V.length currPeople) (bernoulli rhoProb gen)
  let filterZip predicate a b = fst . V.unzip . V.filter predicate $ V.zip a b
      sampledPeople = filterZip snd currPeople rhoBernoullis
      unsampledPeople = filterZip (not . snd) currPeople rhoBernoullis
   in return
        ( Catastrophe catastTime (People sampledPeople)
        , BDSCODPopulation (People unsampledPeople))

-- | Return a randomly sampled Disaster event
-- TODO Move this into the epidemic module to keep things DRY.
randomDisasterEvent ::
     (AbsoluteTime, Probability) -- ^ Time and probability of sampling in the disaster
  -> BDSCODPopulation -- ^ The state of the population prior to the disaster
  -> GenIO
  -> IO (EpidemicEvent, BDSCODPopulation)
randomDisasterEvent (disastTime, nuProb) (BDSCODPopulation (People currPeople)) gen = do
  nuBernoullis <- G.replicateM (V.length currPeople) (bernoulli nuProb gen)
  let filterZip predicate a b = fst . V.unzip . V.filter predicate $ V.zip a b
      sampledPeople = filterZip snd currPeople nuBernoullis
      unsampledPeople = filterZip (not . snd) currPeople nuBernoullis
   in return
        ( Disaster disastTime (People sampledPeople)
        , BDSCODPopulation (People unsampledPeople))
