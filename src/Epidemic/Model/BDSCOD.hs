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
-- catastrophe specification, occurrence rate and disaster specification. The SO
-- removal probability is the probability that if an individual appears in a
-- sample or an occurrence that they will be removed from the infectious
-- population.
data BDSCODParameters =
  BDSCODParameters
  { bdscodBirthRate       :: Rate
  , bdscodDeathRate       :: Rate
  , bdscodSamplingRate    :: Rate
  , bdscodCatastropheSpec :: Timed Probability
  , bdscodOccurrenceRate  :: Rate
  , bdscodDisasterSpec    :: Timed Probability
  , bdscodSORemovalProb   :: Probability
  } deriving (Show, Eq)

data BDSCODPopulation =
  BDSCODPopulation People
  deriving (Show)

instance ModelParameters BDSCODParameters BDSCODPopulation where
  -- | Since sampling and occurrence only sometimes results in removal we must
  -- account for this.
  rNaught _ BDSCODParameters {..} _ =
    return $ bdscodBirthRate / (bdscodDeathRate + bdscodSORemovalProb * bdscodSamplingRate + bdscodSORemovalProb * bdscodOccurrenceRate)
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

-- | Configuration of a birth-death-sampling-occurrence-disaster simulation,
-- potentially with sampled ancestors.
configuration ::
     AbsoluteTime -- ^ Start time of the simulation
  -> TimeDelta -- ^ Duration of the simulation
  -> Bool -- ^ condition upon at least two sequenced samples.
  -> Maybe (BDSCODPopulation -> Bool, [EpidemicEvent] -> s) -- ^ values for termination handling.
  -> ( Rate
     , Rate
     , Rate
     , [(AbsoluteTime, Probability)]
     , Rate
     , [(AbsoluteTime, Probability)]
     , Probability) -- ^ Birth, Death, Sampling, Catastrophe probability, Occurrence rates, Disaster probabilities and the probability of removal upon individual sampling.
  -> Maybe (SimulationConfiguration BDSCODParameters BDSCODPopulation s)
configuration startTime maxTime atLeastCherry maybeTHFuncs (birthRate, deathRate, samplingRate, catastropheSpec, occurrenceRate, disasterSpec, sampOccRemovalProb) = do
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
          sampOccRemovalProb
      (seedPerson, newId) = newPerson initialIdentifier
      bdscodPop = BDSCODPopulation . People . Set.singleton $ seedPerson
      termHandler = do (f1, f2) <- maybeTHFuncs
                       return $ TerminationHandler f1 f2
   in return $
      SimulationConfiguration
        bdscodParams
        bdscodPop
        newId
        startTime
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
      (Just weightVec) = eventWeights currPop params currTime -- birth, death, sampling, occurrence.
   in do delay <-
           exponential (fromIntegral (numPeople currPeople) * netEventRate) gen
         let newEventTime = timeAfterDelta currTime (TimeDelta delay)
         if noScheduledEvent currTime newEventTime (bdscodCatastropheSpec <> bdscodDisasterSpec)
         then do
                eventIx <- categorical weightVec gen
                (selectedPerson, unselectedPeople) <- randomPerson currPeople gen
                removeIndividual <- bernoulli bdscodSORemovalProb gen
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
                      if removeIndividual
                      then ( newEventTime
                           , IndividualSample newEventTime selectedPerson True True
                           , BDSCODPopulation unselectedPeople
                           , currId)
                      else ( newEventTime
                           , IndividualSample newEventTime selectedPerson True False
                           , BDSCODPopulation currPeople -- population has not changed!
                           , currId)
                    3 ->
                      if removeIndividual
                      then ( newEventTime
                           , IndividualSample newEventTime selectedPerson False True
                           , BDSCODPopulation unselectedPeople
                           , currId)
                      else ( newEventTime
                           , IndividualSample newEventTime selectedPerson False False
                           , BDSCODPopulation currPeople -- population has not changed!
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


selectedPeople :: ((Person, b) -> Bool) -> [Person] -> [b] -> People
selectedPeople predicate persons bools = asPeople [x | p@(x, _) <- zip persons bools, predicate p]

-- | Return a randomly sampled Catastrophe event
randomCatastropheEvent ::
     (AbsoluteTime, Probability) -- ^ Time and probability of sampling in the catastrophe
  -> BDSCODPopulation -- ^ The state of the population prior to the catastrophe
  -> GenIO
  -> IO (EpidemicEvent, BDSCODPopulation)
randomCatastropheEvent (catastTime, rhoProb) (BDSCODPopulation currPeople) gen = do
  let currPersons = Set.toList $ coerce currPeople
  rhoBernoullis <- replicateM (numPeople currPeople) (bernoulli rhoProb gen)
  let sampledPeople = selectedPeople snd currPersons rhoBernoullis
      unsampledPeople = selectedPeople (not . snd) currPersons rhoBernoullis
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
  let currPersons = Set.toList $ coerce currPeople
  nuBernoullis <- replicateM (numPeople currPeople) (bernoulli nuProb gen)
  let sampledPeople = selectedPeople snd currPersons nuBernoullis
      unsampledPeople = selectedPeople (not . snd) currPersons nuBernoullis
   in return
        ( PopulationSample disastTime sampledPeople False
        , BDSCODPopulation unsampledPeople)
