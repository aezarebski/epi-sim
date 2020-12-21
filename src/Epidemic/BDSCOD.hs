{-# LANGUAGE RecordWildCards #-}

module Epidemic.BDSCOD
  ( configuration
  , allEvents
  , observedEvents
  ) where

import Data.List (nub)
import Data.Maybe (fromJust)
import qualified Data.Vector as V
import qualified Data.Vector.Generic as G
import Epidemic
import Epidemic.Types.Events
  ( EpidemicEvent(..)
  , PointProcessEvents(..)
  , ReconstructedTree(..)
  , maybeEpidemicTree
  , maybeReconstructedTree
  , pointProcessEvents
  )
import Epidemic.Types.Parameter
import Epidemic.Types.Population
import Epidemic.Utility
import System.Random.MWC
import System.Random.MWC.Distributions (bernoulli, categorical, exponential)


data BDSCODParameters
  -- | birth rate, death rate, sampling rate, catastrophe specification, occurrence rate and disaster specification
  = BDSCODParameters Rate Rate Rate (Timed Probability) Rate (Timed Probability)

instance ModelParameters BDSCODParameters where
  rNaught (BDSCODParameters birthRate deathRate samplingRate _ occurrenceRate _) _ =
    Just $ birthRate / (deathRate + samplingRate + occurrenceRate)
  eventRate (BDSCODParameters birthRate deathRate samplingRate _ occurrenceRate _) _ =
    Just $ birthRate + deathRate + samplingRate + occurrenceRate
  birthProb (BDSCODParameters birthRate deathRate samplingRate _ occurrenceRate _) _ =
    Just $ birthRate / (birthRate + deathRate + samplingRate + occurrenceRate)

newtype BDSCODPopulation =
  BDSCODPopulation People
  deriving (Show)

instance Population BDSCODPopulation where
  susceptiblePeople _ = Nothing
  infectiousPeople (BDSCODPopulation people) = Just people
  removedPeople _ = Nothing
  isInfected (BDSCODPopulation (People people)) = not $ V.null people

-- | Configuration of a birth-death-sampling-occurrence simulation
configuration :: AbsoluteTime                                                    -- ^ Duration of the simulation
              -> (Rate,Rate,Rate,[(AbsoluteTime,Probability)],Rate,[(AbsoluteTime,Probability)]) -- ^ Birth, Death, Sampling, Catastrophe probability and Occurrence rates
              -> Maybe (SimulationConfiguration BDSCODParameters BDSCODPopulation)
configuration maxTime (birthRate, deathRate, samplingRate, catastropheSpec, occurrenceRate, disasterSpec) =
  do catastropheSpec' <- asTimed catastropheSpec
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
       in return $ SimulationConfiguration bdscodParams bdscodPop newId maxTime

-- | Return a random event from the BDSCOD-process given the current state of the process.
randomEvent :: BDSCODParameters  -- ^ Parameters of the process
            -> AbsoluteTime              -- ^ The current time within the process
            -> BDSCODPopulation  -- ^ The current state of the populaion
            -> Identifier        -- ^ The current state of the identifier generator
            -> GenIO             -- ^ The current state of the PRNG
            -> IO (AbsoluteTime, EpidemicEvent, BDSCODPopulation, Identifier)
randomEvent params@(BDSCODParameters br dr sr catastInfo occr disastInfo) currTime currPop@(BDSCODPopulation currPeople) currId gen =
  let netEventRate = fromJust $ eventRate params currTime
      eventWeights = V.fromList [br, dr, sr, occr]
   in do delay <- exponential (fromIntegral (numPeople currPeople) * netEventRate) gen
         newEventTime <- pure $ timeAfterDelta currTime (TimeDelta delay)
         if noScheduledEvent currTime newEventTime (catastInfo <> disastInfo)
           then do eventIx <- categorical eventWeights gen
                   (selectedPerson, unselectedPeople) <- randomPerson currPeople gen
                   return $ case eventIx of
                     0 -> let (birthedPerson, newId) = newPerson currId
                              event = Infection newEventTime selectedPerson birthedPerson
                       in ( newEventTime
                          , event
                          , BDSCODPopulation (addPerson birthedPerson currPeople)
                          , newId)
                     1 -> (newEventTime, Removal newEventTime selectedPerson, BDSCODPopulation unselectedPeople, currId)
                     2 -> (newEventTime, Sampling newEventTime selectedPerson, BDSCODPopulation unselectedPeople, currId)
                     3 -> (newEventTime, Occurrence newEventTime selectedPerson, BDSCODPopulation unselectedPeople, currId)
                     _ -> error "no birth, death, sampling, occurrence event selected."

           else if noScheduledEvent currTime newEventTime catastInfo
                  then let (Just (disastTime,disastProb)) = firstScheduled currTime disastInfo
                        in do (disastEvent,postDisastPop) <- randomDisasterEvent (disastTime,disastProb) currPop gen
                              return (disastTime,disastEvent,postDisastPop,currId)
                else if noScheduledEvent currTime newEventTime disastInfo
                        then let (Just (catastTime,catastProb)) = firstScheduled currTime catastInfo
                              in do (catastEvent,postCatastPop) <- randomCatastropheEvent (catastTime,catastProb) currPop gen
                                    return (catastTime,catastEvent,postCatastPop,currId)
                     else let (Just (catastTime,catastProb)) = firstScheduled currTime catastInfo
                              (Just (disastTime,disastProb)) = firstScheduled currTime disastInfo
                           in do (scheduledEvent,postEventPop) <- if catastTime < disastTime then
                                                                    randomCatastropheEvent (catastTime,catastProb) currPop gen else
                                                                    randomDisasterEvent (disastTime,disastProb) currPop gen
                                 return (min catastTime disastTime,scheduledEvent,postEventPop,currId)


-- | Return a randomly sampled Catastrophe event
randomCatastropheEvent :: (AbsoluteTime,Probability) -- ^ Time and probability of sampling in the catastrophe
                       -> BDSCODPopulation    -- ^ The state of the population prior to the catastrophe
                       -> GenIO
                       -> IO (EpidemicEvent,BDSCODPopulation)
randomCatastropheEvent (catastTime, rhoProb) (BDSCODPopulation (People currPeople)) gen = do
  rhoBernoullis <- G.replicateM (V.length currPeople) (bernoulli rhoProb gen)
  let filterZip predicate a b = fst . V.unzip . V.filter predicate $ V.zip a b
      sampledPeople = filterZip snd currPeople rhoBernoullis
      unsampledPeople = filterZip (not . snd) currPeople rhoBernoullis
   in return
        ( Catastrophe catastTime (People sampledPeople)
        , BDSCODPopulation (People unsampledPeople))

-- | Return a randomly sampled Disaster event
randomDisasterEvent :: (AbsoluteTime,Probability) -- ^ Time and probability of sampling in the disaster
                    -> BDSCODPopulation    -- ^ The state of the population prior to the disaster
                    -> GenIO
                    -> IO (EpidemicEvent,BDSCODPopulation)
randomDisasterEvent (disastTime, nuProb) (BDSCODPopulation (People currPeople)) gen = do
  nuBernoullis <- G.replicateM (V.length currPeople) (bernoulli nuProb gen)
  let filterZip predicate a b = fst . V.unzip . V.filter predicate $ V.zip a b
      sampledPeople = filterZip snd currPeople nuBernoullis
      unsampledPeople = filterZip (not . snd) currPeople nuBernoullis
   in return
        ( Disaster disastTime (People sampledPeople)
        , BDSCODPopulation (People unsampledPeople))

allEvents ::
     BDSCODParameters
  -> AbsoluteTime
  -> (AbsoluteTime, [EpidemicEvent], BDSCODPopulation, Identifier)
  -> GenIO
  -> IO (AbsoluteTime, [EpidemicEvent], BDSCODPopulation, Identifier)
allEvents rates maxTime currState@(currTime, currEvents, currPop, currId) gen =
  if isInfected currPop
    then do
      (newTime, event, newPop, newId) <-
        randomEvent rates currTime currPop currId gen
      if newTime < maxTime
        then allEvents
               rates
               maxTime
               (newTime, event : currEvents, newPop, newId)
               gen
        else return currState
    else return currState

-- | The events from the nodes of a reconstructed tree __not__ in time sorted
-- order.
reconstructedTreeEvents :: ReconstructedTree -> [EpidemicEvent]
reconstructedTreeEvents node = case node of
  (RBranch e lt rt) -> e:(reconstructedTreeEvents lt ++ reconstructedTreeEvents rt)
  (RLeaf e) -> [e]

-- | Just the observable events from a list of all the events that occurred in a
-- simulation of the BDSCOD-process. These events are the result of extracting
-- the events from the reconstructed tree and getting the point process events
-- that make up the unsequenced samples (see `pointProcessEvents` for details on
-- this latter data.)
observedEvents :: [EpidemicEvent] -- ^ All of the simulation events
               -> Maybe [EpidemicEvent]
observedEvents eEvents = do
  epiTree <- maybeEpidemicTree eEvents
  reconTree <- maybeReconstructedTree epiTree
  let (PointProcessEvents nonReconTreeEvents) = pointProcessEvents epiTree
  let reconTreeEvents = reconstructedTreeEvents reconTree
  return . sort . nub $ nonReconTreeEvents ++ reconTreeEvents
