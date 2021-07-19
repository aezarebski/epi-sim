{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}

module Epidemic.Model.LogisticBDSD
  ( configuration
  , randomEvent
  , LogisticBDSDParameters(..)
  , LogisticBDSDPopulation(..)
  ) where

import Data.Maybe (fromJust)
import qualified Data.Vector as V
import qualified Data.Vector.Generic as G
import Epidemic.Data.Events (EpidemicEvent(..))
import Epidemic.Data.Time
  ( AbsoluteTime(..)
  , Timed(..)
  , TimeDelta(..)
  , asTimed
  , timeAfterDelta
  )
import Epidemic.Data.Parameter
  (  ModelParameters(..)
  , Probability
  , Rate
  , firstScheduled
  , noScheduledEvent
  )
import Epidemic.Data.Population
  ( Identifier(..)
  , People(..)
  , Population(..)
  , addPerson
  , initialIdentifier
  , newPerson
  , nullPeople
  , numPeople
  )
import Epidemic.Data.Simulation
  ( SimulationConfiguration(..)
  , SimulationRandEvent(..), TerminationHandler(..)
  )
import Epidemic.Utility
  ( maybeToRight
  , randomPerson
  )
import System.Random.MWC (GenIO)
import System.Random.MWC.Distributions (bernoulli, categorical, exponential)

-- | The parameters of the logistic-BDSD process. This process allows for
-- infections, removals, sampling and disasters.
data LogisticBDSDParameters =
  LogisticBDSDParameters
    { paramsBirthRate :: Rate
    , paramsCapacity :: Int
    , paramsDeathRate :: Rate
    , paramsSamplingRate :: Rate
    , paramsDisasters :: Timed Probability
    }
  deriving (Show)

newtype LogisticBDSDPopulation =
  LogisticBDSDPopulation People
  deriving (Show)

-- | The per lineage birth rate accounting for the population size.
logisticBirthRate :: LogisticBDSDParameters -> LogisticBDSDPopulation -> Rate
logisticBirthRate LogisticBDSDParameters {..} (LogisticBDSDPopulation pop) =
  let propCapacity = fromIntegral (numPeople pop) / fromIntegral paramsCapacity
   in paramsBirthRate * (1.0 - propCapacity)

instance ModelParameters LogisticBDSDParameters LogisticBDSDPopulation where
  rNaught _ _ _ = Nothing
  eventRate (LogisticBDSDPopulation pop) LogisticBDSDParameters {..} _ =
    let propCapcity = fromIntegral (numPeople pop) / fromIntegral paramsCapacity
        br = paramsBirthRate * (1.0 - propCapcity)
     in Just $ br + paramsDeathRate + paramsSamplingRate
  birthProb lpop lparam absTime = do
    er <- eventRate lpop lparam absTime
    Just $ br / er
    where
      br = logisticBirthRate lparam lpop
  eventWeights currPop params@LogisticBDSDParameters {..} _ =
    let logisticBR = logisticBirthRate params currPop
        in Just $ V.fromList [logisticBR, paramsDeathRate, paramsSamplingRate]

instance Population LogisticBDSDPopulation where
  susceptiblePeople _ = Nothing
  infectiousPeople (LogisticBDSDPopulation people) = Just people
  removedPeople _ = Nothing
  isInfected (LogisticBDSDPopulation people) = not $ nullPeople people

-- | Create an simulation configuration or return an error message if this is
-- not possible.
configuration ::
     TimeDelta
  -> Bool -- ^ condition upon at least two sequenced samples.
  -> Maybe (LogisticBDSDPopulation -> Bool, [EpidemicEvent] -> s) -- ^ values for termination handling.
  -> (Rate, Int, Rate, Rate, [(AbsoluteTime, Probability)])
  -> Either String (SimulationConfiguration LogisticBDSDParameters LogisticBDSDPopulation s)
configuration simDuration atLeastCherry maybeTHFuncs (birthRate, capacity, deathRate, samplingRate, disasterSpec)
  | minimum [birthRate, deathRate, samplingRate] < 0 =
    Left "negative rate provided"
  | capacity < 1 = Left "insufficient population capacity"
  | otherwise = do
    disasterTP <-
      maybeToRight
        "could not construct timed probability"
        (asTimed disasterSpec)
    let logBDSDParams =
          LogisticBDSDParameters
            birthRate
            capacity
            deathRate
            samplingRate
            disasterTP
        (seedPerson, newId) = newPerson initialIdentifier
        logBDSDPop = LogisticBDSDPopulation (People $ V.singleton seedPerson)
        termHandler = do (f1, f2) <- maybeTHFuncs
                         return $ TerminationHandler f1 f2
     in return $
        SimulationConfiguration
          logBDSDParams
          logBDSDPop
          newId
          (AbsoluteTime 0)
          simDuration
          termHandler
          atLeastCherry

-- | Defines how a single random event is simulated in this model.
randomEvent :: SimulationRandEvent LogisticBDSDParameters LogisticBDSDPopulation
randomEvent = SimulationRandEvent randEvent'

randEvent' ::
     LogisticBDSDParameters
  -> AbsoluteTime
  -> LogisticBDSDPopulation
  -> Identifier
  -> GenIO
  -> IO (AbsoluteTime, EpidemicEvent, LogisticBDSDPopulation, Identifier)
randEvent' params@LogisticBDSDParameters {..} currTime currPop@(LogisticBDSDPopulation currPpl) currId gen =
  let netEventRate = (fromJust $ eventRate currPop params currTime)
      popSizeDouble = fromIntegral $ numPeople currPpl
      (Just weightsVec) = eventWeights currPop params currTime
   in do delay <- exponential (netEventRate * popSizeDouble) gen
         let newEventTime = timeAfterDelta currTime (TimeDelta delay)
         if noScheduledEvent currTime newEventTime paramsDisasters
           then do
             eventIx <- categorical weightsVec gen
             (randPerson, otherPeople) <- randomPerson currPpl gen
             return $
               case eventIx of
                 0 ->
                   let (infectedPerson, newId) = newPerson currId
                       infEvent =
                         Infection newEventTime randPerson infectedPerson
                       newPop =
                         LogisticBDSDPopulation
                           (addPerson infectedPerson currPpl)
                    in (newEventTime, infEvent, newPop, newId)
                 1 ->
                   ( newEventTime
                   , Removal newEventTime randPerson
                   , LogisticBDSDPopulation otherPeople
                   , currId)
                 2 ->
                   ( newEventTime
                   , IndividualSample newEventTime randPerson True
                   , LogisticBDSDPopulation otherPeople
                   , currId)
                 _ -> error "do not recognise the type of event index."
           else let (Just dsstr@(dsstrTime, _)) =
                      firstScheduled currTime paramsDisasters
                 in do (schdEvent, postEventPpl) <-
                         randomDisasterEvent dsstr currPop gen
                       return (dsstrTime, schdEvent, postEventPpl, currId)

-- | Return a randomly sampled Disaster event
-- TODO Move this into the epidemic module to keep things DRY.
randomDisasterEvent ::
     (AbsoluteTime, Probability) -- ^ Time and probability of sampling in the disaster
  -> LogisticBDSDPopulation -- ^ The state of the population prior to the disaster
  -> GenIO
  -> IO (EpidemicEvent, LogisticBDSDPopulation)
randomDisasterEvent (dsstrTime, dsstrProb) (LogisticBDSDPopulation (People currPpl)) gen = do
  randBernoullis <- G.replicateM (V.length currPpl) (bernoulli dsstrProb gen)
  let filterZip predicate a b = fst . V.unzip . V.filter predicate $ V.zip a b
      sampledPeople = filterZip snd currPpl randBernoullis
      unsampledPeople = filterZip (not . snd) currPpl randBernoullis
   in return
        ( PopulationSample dsstrTime (People sampledPeople) False
        , LogisticBDSDPopulation (People unsampledPeople))
