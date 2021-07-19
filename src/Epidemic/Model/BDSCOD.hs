{-# LANGUAGE MultiParamTypeClasses #-}

module Epidemic.Model.BDSCOD
  ( configuration
  , randomEvent
  , BDSCODParameters(..)
  , BDSCODPopulation(..)
  ) where

import qualified Data.Vector as V
import qualified Data.Vector.Generic as G
import Epidemic
import Epidemic.Data.Events (EpidemicEvent(..))
import Epidemic.Data.Parameter
import Epidemic.Data.Population
import Epidemic.Data.Simulation
  ( SimulationConfiguration(..)
  , SimulationRandEvent(..), TerminationHandler(..)
  )
import Epidemic.Data.Time
  ( AbsoluteTime(..)
  , TimeDelta(..)
  , Timed(..)
  , asTimed
  , maybeNextTimed
  , timeAfterDelta
  )
import Epidemic.Utility
import System.Random.MWC
import System.Random.MWC.Distributions (bernoulli, categorical, exponential)

-- | birth rate, death rate, sampling rate, catastrophe specification, occurrence rate and disaster specification
data BDSCODParameters =
  BDSCODParameters Rate Rate Rate (Timed Probability) Rate (Timed Probability)

data BDSCODPopulation =
  BDSCODPopulation People
  deriving (Show)

instance ModelParameters BDSCODParameters BDSCODPopulation where
  rNaught _ (BDSCODParameters br dr sRate _ occRate _) _ =
    Just $ br / (dr + sRate + occRate)
  eventRate _ (BDSCODParameters br dr sRate _ occRate _) _ =
    Just $ br + dr + sRate + occRate
  birthProb _ (BDSCODParameters br dr sRate _ occRate _) _ =
    Just $ br / (br + dr + sRate + occRate)
  eventWeights _ (BDSCODParameters br dr sRate _ occRate _) _ =
    Just $ V.fromList [br, dr, sRate, occRate]

instance Population BDSCODPopulation where
  susceptiblePeople _ = Nothing
  infectiousPeople (BDSCODPopulation people) = Just people
  removedPeople _ = Nothing
  isInfected (BDSCODPopulation (People people)) = not $ V.null people

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
      bdscodPop = BDSCODPopulation (People $ V.singleton seedPerson)
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
randomEvent' params@(BDSCODParameters _ _ _ catastInfo _ disastInfo) currTime currPop@(BDSCODPopulation currPeople) currId gen =
  let (Just netEventRate) = eventRate currPop params currTime
      (Just weightVec) = eventWeights currPop params currTime
   in do delay <-
           exponential (fromIntegral (numPeople currPeople) * netEventRate) gen
         let newEventTime = timeAfterDelta currTime (TimeDelta delay)
         if noScheduledEvent currTime newEventTime (catastInfo <> disastInfo)
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
                      , IndividualSample newEventTime selectedPerson True
                      , BDSCODPopulation unselectedPeople
                      , currId)
                    3 ->
                      ( newEventTime
                      , IndividualSample newEventTime selectedPerson False
                      , BDSCODPopulation unselectedPeople
                      , currId)
                    _ ->
                      error "no birth, death, sampling, occurrence event selected."
         else case maybeNextTimed catastInfo disastInfo currTime of
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
randomCatastropheEvent (catastTime, rhoProb) (BDSCODPopulation (People currPeople)) gen = do
  rhoBernoullis <- G.replicateM (V.length currPeople) (bernoulli rhoProb gen)
  let filterZip predicate a b = fst . V.unzip . V.filter predicate $ V.zip a b
      sampledPeople = filterZip snd currPeople rhoBernoullis
      unsampledPeople = filterZip (not . snd) currPeople rhoBernoullis
   in return
        ( PopulationSample catastTime (People sampledPeople) True
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
        ( PopulationSample disastTime (People sampledPeople) False
        , BDSCODPopulation (People unsampledPeople))
