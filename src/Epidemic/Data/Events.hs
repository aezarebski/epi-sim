{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Epidemic.Data.Events
  ( EpidemicEvent(Infection, Removal, IndividualSample,
              PopulationSample, StoppingTime, Extinction)
  , popSampPeople
  , popSampSeq
  , popSampTime
  , indSampPerson
  , indSampSeq
  , indSampTime
  , indSampRemoved
  , infTime
  , infInfector
  , infInfectee
  , EpidemicTree(Branch, Burr, Leaf, Shoot)
  , hasSequencedObs
  , epiTree
  , isExtinctionOrStopping
  , isIndividualSample
  , derivedFrom
  ) where

import qualified Data.Aeson               as Json
import           Data.Function            ((&))
import qualified Data.List.NonEmpty       as NonEmpty
import           Epidemic.Data.Parameter  (Probability, noScheduledEvent)
import           Epidemic.Data.Population
import           Epidemic.Data.Time       (AbsoluteTime (..), TimeStamp (..))
import           GHC.Generics

-- | Events that can occur during an epidemic.
--
-- >>> p1 = Person initialIdentifier
-- >>> p2 = p1 & personId & succ & Person
-- >>> infEvent = Infection (AbsoluteTime 1.0) p1 p2
--
data EpidemicEvent
  = Infection
      { infTime     :: AbsoluteTime
      , infInfector :: Person -- ^ the person doing the infecting
      , infInfectee :: Person -- ^ the person who got infected
      }
  | Removal
      { remTime   :: AbsoluteTime
      , remPerson :: Person
      }
  | IndividualSample
      { indSampTime    :: AbsoluteTime
      , indSampPerson  :: Person
      , indSampSeq     :: Bool -- ^ whether the sample was sequenced
      , indSampRemoved :: Bool -- ^ whether the individual was removed upon observation
      }
  | PopulationSample
      { popSampTime   :: AbsoluteTime
      , popSampPeople :: People
      , popSampSeq    :: Bool -- ^ whether the samples were sequenced
      }
  | Extinction AbsoluteTime -- ^ epidemic went extinct
  | StoppingTime AbsoluteTime -- ^ the simulation reached the stopping time
  deriving (Show, Generic, Eq)

instance Json.FromJSON EpidemicEvent

instance Json.ToJSON EpidemicEvent

instance TimeStamp EpidemicEvent where
  absTime ee =
    case ee of
      Infection {..}        -> infTime
      Removal  {..}         -> remTime
      IndividualSample {..} -> indSampTime
      PopulationSample {..} -> popSampTime
      StoppingTime absT     -> absT
      Extinction absT       -> absT

-- | Predicate for the event being an infection event.
isInfection :: EpidemicEvent -> Bool
isInfection ee = case ee of
  Infection {} -> True
  _            -> False

-- | Predicate for the event being an individual sample event.
isIndividualSample :: EpidemicEvent -> Bool
isIndividualSample ee =
  case ee of
    IndividualSample {} -> True
    _                   -> False

-- | Predicate for the event being an population sample event.
isPopulationSample :: EpidemicEvent -> Bool
isPopulationSample ee =
  case ee of
    PopulationSample {} -> True
    _                   -> False

-- | Predicate for whether an @EpidemicEvent@ is one of the terminal events of
-- extinction or the stopping time having been reached.
isExtinctionOrStopping :: EpidemicEvent -> Bool
isExtinctionOrStopping e =
  case e of
    Extinction {}   -> True
    StoppingTime {} -> True
    _               -> False

-- | Epidemic Events are ordered based on which occurred first. Since
-- 'Extinction' and 'StoppingTime' events are there as placeholders they are
-- placed as the end of the order.
instance Ord EpidemicEvent where
  e1 <= e2 = absTime e1 <= absTime e2

-- | The events that occurred as a result of the existance of the given person,
-- ie the epidemic events that are descendent of an individual in the full
-- epidemic tree.
derivedFrom ::
     Person
  -> [EpidemicEvent] -- ^ ordered epidemic events
  -> [EpidemicEvent]
derivedFrom person = derivedFromPeople (asPeople [person])

-- | The events that occurred as a result of the existance of a group of people
derivedFromPeople ::
     People
  -> [EpidemicEvent] -- ^ ordered epidemic events
  -> [EpidemicEvent]
derivedFromPeople _ [] = []
derivedFromPeople people (e:es) =
  case e of
    Infection {..} ->
      if includesPerson people infInfector || includesPerson people infInfectee
        then let people' = addPersons [infInfectee, infInfector] people
              in e : derivedFromPeople people' es
        else derivedFromPeople people es
    Removal _ p ->
      let derivedEvents = derivedFromPeople people es
       in if includesPerson people p
            then e : derivedEvents
            else derivedEvents
    IndividualSample {..} ->
      let derivedEvents = derivedFromPeople people es
       in if includesPerson people indSampPerson
            then e : derivedEvents
            else derivedEvents
    PopulationSample {..} ->
      let derivedEvents = derivedFromPeople people es
       in if haveCommonPeople people popSampPeople
            then e : derivedEvents
            else derivedEvents
    Extinction {} -> derivedFromPeople people es
    StoppingTime {} -> derivedFromPeople people es

-- | A tree representing every event in the epidemic, this includes both the
-- transmission and removal events, and the observation process.
--
--     * Branch - an infection event
--     * Burr - an individual sample without removal. If there no descendent
--       events the subtree will just be a shoot.
--     * Leaf - removal (including those without observation) or an observation.
--     * Shoot - an active lineage at present
--
data EpidemicTree
  = Branch EpidemicEvent EpidemicTree EpidemicTree
  | Burr EpidemicEvent EpidemicTree
  | Leaf EpidemicEvent
  | Shoot Person
  deriving (Show, Eq)

-- | Predicate for whether an 'EpidemicTree' has any node which corresponds to a
-- sequenced observation.
hasSequencedObs :: EpidemicTree -> Bool
hasSequencedObs Shoot {} = False
hasSequencedObs (Leaf e) =
  case e of
    IndividualSample {..} -> indSampSeq
    PopulationSample {..} -> popSampSeq
    _                     -> False
hasSequencedObs (Branch _ lt rt) = hasSequencedObs lt || hasSequencedObs rt
hasSequencedObs (Burr e t) =
  case e of
    IndividualSample {..} -> indSampSeq || hasSequencedObs t
    _                     -> False

-- | Helper function to construct a subtree which starts from a particular
-- lineage.
epiTreeDescendedFrom :: Person
                     -> NonEmpty.NonEmpty EpidemicEvent
                     -> Either String EpidemicTree
epiTreeDescendedFrom p es =
  let derivedEvents = derivedFrom p $ NonEmpty.toList es
  in case derivedEvents of
       [] -> Right $ Shoot p
       de:des ->
         case de of
           Infection {..}
             | infInfector == p ->
               do infectorSET <-
                    case NonEmpty.nonEmpty $ derivedFrom p des of
                      (Just infectorEs) -> epiTreeDescendedFrom p infectorEs
                      Nothing           -> Right $ Shoot p
                  infecteeSET <-
                    case NonEmpty.nonEmpty $ derivedFrom infInfectee des of
                      (Just infecteeEs) -> epiTreeDescendedFrom infInfectee infecteeEs
                      Nothing -> Right $ Shoot infInfectee
                  Right $ Branch de infectorSET infecteeSET
             | otherwise -> Left errMsg
           Removal {..}
             | remPerson == p -> Right $ Leaf de
             | otherwise      -> Left errMsg
           IndividualSample {..}
             | indSampPerson == p && indSampRemoved && null des -> Right $ Leaf de
             | indSampPerson == p && indSampRemoved -> Left errMsg
             | indSampPerson == p && not indSampRemoved && null des ->
               Right . Burr de $ Shoot p
             | indSampPerson == p && not indSampRemoved ->
               do subEpiTree <- epiTreeDescendedFrom p $ NonEmpty.fromList des
                  Right $ Burr de subEpiTree
             | otherwise -> Left errMsg
           PopulationSample {..}
             | includesPerson popSampPeople p && null des -> Right $ Leaf de
             | otherwise -> Left errMsg
           StoppingTime {} -> Right $ Shoot p
           Extinction {} -> Left errMsg
           where
             errMsg = "invalid derived event: " <> show de <> " from " <> show p

-- | Epidemic tree from a non-empty list of ordered epidemic events.
epiTree ::
     NonEmpty.NonEmpty EpidemicEvent -- ^ ordered epidemic events
  -> Either String EpidemicTree
epiTree es =
  if NonEmpty.length es == 1
  then case NonEmpty.head es of
         e@Infection {..} ->
           Right $ Branch e (Shoot infInfector) (Shoot infInfectee)
         e@Removal {} -> Right $ Leaf e
         e@IndividualSample {..} ->
           Right $ if indSampRemoved
                   then Leaf e
                   else Burr e $ Shoot indSampPerson
         e@PopulationSample {..} -> if numPeople popSampPeople == 1
                                    then Right $ Leaf e
                                    else Left $ errMsg e
         e -> Left $ errMsg e
  else case NonEmpty.head es of
         e@Infection {..} ->
           let remainingEvents = NonEmpty.fromList $ NonEmpty.drop 1 es
           in do infectorSET <- epiTreeDescendedFrom infInfector remainingEvents
                 infecteeSET <- epiTreeDescendedFrom infInfectee remainingEvents
                 return $ Branch e infectorSET infecteeSET
         e@IndividualSample {..}
           | not indSampRemoved ->
             do subET <- epiTreeDescendedFrom indSampPerson (nonEmptyTail es)
                return $ Burr e subET
           | otherwise -> Left $ errMsg e
         e@PopulationSample {..}
           | nullPeople popSampPeople -> epiTree . nonEmptyTail $ es
           | numPeople popSampPeople == 1 -> return $ Leaf e
           | otherwise -> Left $ errMsg e
         e -> Left $ errMsg e
  where errMsg e = "cannot construct EpidemicTree when first event is " <> show e
        nonEmptyTail = NonEmpty.fromList . NonEmpty.tail
