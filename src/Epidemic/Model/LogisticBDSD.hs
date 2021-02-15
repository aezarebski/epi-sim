{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}

module Epidemic.Model.LogisticBDSD
  ( configuration
  , observedEvents
  , randomEvent
  , LogisticBDSDParameters(..)
  , LogisticBDSDPopulation(..)
  ) where

import Data.Maybe (fromJust)
import qualified Data.Vector as V
import qualified Data.Vector.Generic as G
import Epidemic (noScheduledEvent, firstScheduled)
import Epidemic.Types.Events (EpidemicEvent(..))
import Epidemic.Types.Parameter
  ( AbsoluteTime(..)
  , ModelParameters(..)
  , Probability
  , Rate
  , TimeDelta(..)
  , Timed(..)
  , asTimed
  , timeAfterDelta
  )
import Epidemic.Types.Population
  ( Identifier(..)
  , People(..)
  , Population(..)
  , addPerson
  , nullPeople
  , numPeople
  )
import Epidemic.Types.Simulation
  ( SimulationConfiguration(..)
  , SimulationRandEvent(..)
  , SimulationState(..)
  )
import Epidemic.Utility
  ( initialIdentifier
  , maybeToRight
  , newPerson
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
  birthProb lpop lparam@LogisticBDSDParameters {..} absTime = do
    er <- eventRate lpop lparam absTime
    Just $ br / er
    where
      br = logisticBirthRate lparam lpop

instance Population LogisticBDSDPopulation where
  susceptiblePeople _ = Nothing
  infectiousPeople (LogisticBDSDPopulation people) = Just people
  removedPeople _ = Nothing
  isInfected (LogisticBDSDPopulation people) = not $ nullPeople people


-- | Create an simulation configuration or return an error message if this is
-- not possible.
configuration ::
     TimeDelta
  -> (Rate, Int, Rate, Rate, [(AbsoluteTime, Probability)])
  -> Either String (SimulationConfiguration LogisticBDSDParameters LogisticBDSDPopulation)
configuration simDuration (birthRate, capacity, deathRate, samplingRate, disasterSpec)
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
     in return $
        SimulationConfiguration
          logBDSDParams
          logBDSDPop
          newId
          (AbsoluteTime 0)
          simDuration
          Nothing

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
randEvent' params@LogisticBDSDParameters{..} currTime currPop@(LogisticBDSDPopulation currPpl) currId gen =
  let netEventRate = (fromJust $ eventRate currPop params currTime)
      popSizeDouble = fromIntegral $ numPeople currPpl
      logisticBR = logisticBirthRate params currPop
      eventWeights = V.fromList [logisticBR, paramsDeathRate, paramsSamplingRate]
      in do delay <- exponential (netEventRate * popSizeDouble) gen
            let newEventTime = timeAfterDelta currTime (TimeDelta delay)
            if noScheduledEvent currTime newEventTime paramsDisasters
              then do eventIx <- categorical eventWeights gen
                      (randPerson, otherPeople) <- randomPerson currPpl gen
                      return $ case eventIx of
                        0 -> let (infectedPerson, newId) = newPerson currId
                                 infEvent = Infection newEventTime randPerson infectedPerson
                                 newPop = LogisticBDSDPopulation (addPerson infectedPerson currPpl)
                                 in ( newEventTime
                                    , infEvent
                                    , newPop
                                    , newId)
                        1 -> (newEventTime, Removal newEventTime randPerson, LogisticBDSDPopulation otherPeople, currId)
                        2 -> (newEventTime, Sampling newEventTime randPerson, LogisticBDSDPopulation otherPeople, currId)
                        _ -> error "do not recognise the type of event index."
              else let (Just dsstr@(dsstrTime, _)) = firstScheduled currTime paramsDisasters
                   in do (schdEvent,postEventPpl) <- randomDisasterEvent dsstr currPop gen
                         return (dsstrTime, schdEvent, postEventPpl, currId)

-- | Return a randomly sampled Disaster event
-- TODO Move this into the epidemic module to keep things DRY.
randomDisasterEvent :: (AbsoluteTime,Probability) -- ^ Time and probability of sampling in the disaster
                    -> LogisticBDSDPopulation    -- ^ The state of the population prior to the disaster
                    -> GenIO
                    -> IO (EpidemicEvent,LogisticBDSDPopulation)
randomDisasterEvent (dsstrTime, dsstrProb) (LogisticBDSDPopulation (People currPpl)) gen = do
  randBernoullis <- G.replicateM (V.length currPpl) (bernoulli dsstrProb gen)
  let filterZip predicate a b = fst . V.unzip . V.filter predicate $ V.zip a b
      sampledPeople = filterZip snd currPpl randBernoullis
      unsampledPeople = filterZip (not . snd) currPpl randBernoullis
   in return
        ( Disaster dsstrTime (People sampledPeople)
        , LogisticBDSDPopulation (People unsampledPeople))

-- | From a full list of epidemic events that occurred in a simulation return
-- just the ones that were observed. For the logistic-BDSD this is those that
-- are part of the reconstructed tree and those that occurred in disasters. If
-- this is not possible return an error message in a string.
-- TODO Implement this function
observedEvents :: [EpidemicEvent] -> Either String [EpidemicEvent]
observedEvents = undefined
