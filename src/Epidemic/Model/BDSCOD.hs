{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}

module Epidemic.Model.BDSCOD
  ( configuration
  , randomEvent
  , BDSCODParameters(..)
  , BDSCODPopulation(..)
  ) where

import           Control.Monad                   (replicateM)
import           Data.Coerce                     (coerce)
import qualified Data.Set                        as Set
import qualified Data.Vector                     as V
import qualified Data.Vector.Generic             as G
import           Epidemic
import           Epidemic.Data.Events            (EpidemicEvent (..))
import           Epidemic.Data.Parameter
import           Epidemic.Data.Population
import           Epidemic.Data.Simulation        (SimulationConfiguration (..),
                                                  SimulationRandEvent (..),
                                                  TerminationHandler (..))
import           Epidemic.Data.Time              (AbsoluteTime (..),
                                                  TimeDelta (..), Timed (..),
                                                  asTimed, maybeNextTimed,
                                                  timeAfterDelta)
import           Epidemic.Utility
import           System.Random.MWC
import           System.Random.MWC.Distributions (bernoulli, categorical,
                                                  exponential)

-- | Parameters of the BDSCOD process: birth rate, death rate, sampling rate,
-- catastrophe specification, occurrence rate and disaster specification
data BDSCODParameters =
  BDSCODParameters
  { bdscodBirthRate       :: Rate
  , bdscodDeathRate       :: Rate
  , bdscodSamplingRate    :: Rate
  , bdscodCatastropheSpec :: Timed Probability
  , bdscodOccurrenceRate  :: Rate
  , bdscodDisasterSpec    :: Timed Probability
  } deriving (Show, Eq)

data BDSCODPopulation =
  BDSCODPopulation People
  deriving (Show)

instance ModelParameters BDSCODParameters BDSCODPopulation where
  rNaught _ BDSCODParameters {..} _ =
    return $ bdscodBirthRate / (bdscodDeathRate + bdscodSamplingRate + bdscodOccurrenceRate)
  perCapitaEventRate _ BDSCODParameters {..} _ =
    return $ bdscodBirthRate + bdscodDeathRate + bdscodSamplingRate + bdscodOccurrenceRate
  birthProb pop params@BDSCODParameters {..} absT =
    do
      eR <- perCapitaEventRate pop params absT
      return $ bdscodBirthRate / eR
  eventWeights _ BDSCODParameters {..} _ =
    return $ V.fromList [bdscodBirthRate, bdscodDeathRate, bdscodSamplingRate, bdscodOccurrenceRate]

instance Population BDSCODPopulation where
  susceptiblePeople _ = Nothing
  infectiousPeople (BDSCODPopulation people) = Just people
  removedPeople _ = Nothing
  isInfected (BDSCODPopulation (People people)) = not $ Set.null people

-- | Configuration of a birth-death-sampling-occurrence-disaster simulation
configuration ::
     TimeDelta -- ^ Duration of the simulation
  -> Bool -- ^ condition upon at least two sequenced samples.
  -> Maybe (BDSCODPopulation -> Bool, [EpidemicEvent] -> s) -- ^ values for termination handling.
  -> ( Rate
     , Rate
     , Rate
     , [(AbsoluteTime, Probability)]
     , Rate
     , [(AbsoluteTime, Probability)]) -- ^ Birth, Death, Sampling, Catastrophe probability, Occurrence rates and Disaster probabilities
  -> Maybe (SimulationConfiguration BDSCODParameters BDSCODPopulation s)
configuration maxTime atLeastCherry maybeTHFuncs (birthRate, deathRate, samplingRate, catastropheSpec, occurrenceRate, disasterSpec) = do
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
      bdscodPop = BDSCODPopulation . People . Set.singleton $ seedPerson
      termHandler = do (f1, f2) <- maybeTHFuncs
                       return $ TerminationHandler f1 f2
   in return $
      SimulationConfiguration
        bdscodParams
        bdscodPop
        newId
        (AbsoluteTime 0)
        maxTime
        termHandler
        atLeastCherry

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
randomEvent' params@BDSCODParameters {..} currTime currPop@(BDSCODPopulation currPeople) currId gen =
  let (Just netEventRate) = perCapitaEventRate currPop params currTime
      (Just weightVec) = eventWeights currPop params currTime
   in do delay <-
           exponential (fromIntegral (numPeople currPeople) * netEventRate) gen
         let newEventTime = timeAfterDelta currTime (TimeDelta delay)
         if noScheduledEvent currTime newEventTime (bdscodCatastropheSpec <> bdscodDisasterSpec)
         then do
                eventIx <- categorical weightVec gen
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
                      , IndividualSample newEventTime selectedPerson True True
                      , BDSCODPopulation unselectedPeople
                      , currId)
                    3 ->
                      ( newEventTime
                      , IndividualSample newEventTime selectedPerson False True
                      , BDSCODPopulation unselectedPeople
                      , currId)
                    _ ->
                      error "no birth, death, sampling, occurrence event selected."
         else case maybeNextTimed bdscodCatastropheSpec bdscodDisasterSpec currTime of
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
randomCatastropheEvent ::
     (AbsoluteTime, Probability) -- ^ Time and probability of sampling in the catastrophe
  -> BDSCODPopulation -- ^ The state of the population prior to the catastrophe
  -> GenIO
  -> IO (EpidemicEvent, BDSCODPopulation)
randomCatastropheEvent (catastTime, rhoProb) (BDSCODPopulation currPeople) gen = do
  let peopleFilterZip pred a b = People $ Set.fromList [x | p@(x, _) <- zip a b, pred p]
      currPersons = Set.toList $ coerce currPeople
  rhoBernoullis <- replicateM (numPeople currPeople) (bernoulli rhoProb gen)
  let sampledPeople = peopleFilterZip snd currPersons rhoBernoullis
      unsampledPeople = peopleFilterZip (not . snd) currPersons rhoBernoullis
   in return
        ( PopulationSample catastTime sampledPeople True
        , BDSCODPopulation unsampledPeople)

-- | Return a randomly sampled Disaster event
-- TODO Move this into the epidemic module to keep things DRY.
-- TODO Extract the @peopleFilterZip@ function because it is used several times.
randomDisasterEvent ::
     (AbsoluteTime, Probability) -- ^ Time and probability of sampling in the disaster
  -> BDSCODPopulation -- ^ The state of the population prior to the disaster
  -> GenIO
  -> IO (EpidemicEvent, BDSCODPopulation)
randomDisasterEvent (disastTime, nuProb) (BDSCODPopulation currPeople) gen = do
  let peopleFilterZip pred a b = People $ Set.fromList [x | p@(x, _) <- zip a b, pred p]
      currPersons = Set.toList $ coerce currPeople
  nuBernoullis <- replicateM (numPeople currPeople) (bernoulli nuProb gen)
  let sampledPeople = peopleFilterZip snd currPersons nuBernoullis
      unsampledPeople = peopleFilterZip (not . snd) currPersons nuBernoullis
   in return
        ( PopulationSample disastTime sampledPeople False
        , BDSCODPopulation unsampledPeople)
