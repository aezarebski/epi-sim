{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Epidemic.Types.Events
  ( EpidemicEvent(Infection, Removal, Sampling, Catastrophe,
              Occurrence, Disaster, Extinction, StoppingTime)
  , EpidemicTree(Branch, Leaf, Shoot)
  , maybeEpidemicTree
  , isExtinctionOrStopping
  , eventTime
  , derivedFrom
  , Newick
  , asNewickString
  ) where

import qualified Data.Aeson as Json
import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as BBuilder
import qualified Data.Csv as Csv
import qualified Data.List as List
import qualified Data.Vector as V
import Epidemic.Types.Parameter
import Epidemic.Types.Population
import GHC.Generics

-- | Events that can occur in an epidemic with their absolute time.
data EpidemicEvent
  = Infection AbsoluteTime Person Person -- ^ infection time, infector, infectee
  | Removal AbsoluteTime Person -- ^ removal without observation
  | Sampling AbsoluteTime Person -- ^ removal and inclusion in phylogeny
  | Catastrophe AbsoluteTime People -- ^ scheduled sampling of lineages
  | Occurrence AbsoluteTime Person -- ^ removal and observed by not in phylogeny
  | Disaster AbsoluteTime People -- ^ scheduled occurrence of lineages
  | Extinction -- ^ epidemic went extinct time time can be recovered from the preceeding removal
  | StoppingTime -- ^ the simulation reached the stopping time
  deriving (Show, Generic, Eq)

instance Json.FromJSON EpidemicEvent

instance Json.ToJSON EpidemicEvent

-- | A representation of the whole transmission tree in a realisation of an
-- epidemic including the unobserved leaves. Lineages that are still extant are
-- modelled as @Shoots@ and contain a `Person` as their data rather than an
-- event since definition they do not have an event associated with them as a
-- leaf.
data EpidemicTree
  = Branch EpidemicEvent EpidemicTree EpidemicTree -- ^ Internal node representing infection event
  | Leaf EpidemicEvent -- ^ External node representing removal event which could be observed or not
  | Shoot Person -- ^ External node representing extant lineages
  deriving (Show, Eq)

instance Csv.ToRecord EpidemicEvent where
  toRecord e =
    case e of
      (Infection time person1 person2) ->
        Csv.record
          [ "infection"
          , Csv.toField time
          , Csv.toField person1
          , Csv.toField person2
          ]
      (Removal time person) ->
        Csv.record ["removal", Csv.toField time, Csv.toField person, "NA"]
      (Sampling time person) ->
        Csv.record ["sampling", Csv.toField time, Csv.toField person, "NA"]
      (Catastrophe time people) ->
        Csv.record ["catastrophe", Csv.toField time, Csv.toField people, "NA"]
      (Occurrence time person) ->
        Csv.record ["occurrence", Csv.toField time, Csv.toField person, "NA"]
      (Disaster time people) ->
        Csv.record ["disaster", Csv.toField time, Csv.toField people, "NA"]
      Extinction -> Csv.record ["extinction", "NA", "NA", "NA"]
      StoppingTime -> Csv.record ["stop", "NA", "NA", "NA"]

-- | Predicate for whether an @EpidemicEvent@ is one of the terminal events of
-- extinction or the stopping time having been reached.
isExtinctionOrStopping :: EpidemicEvent -> Bool
isExtinctionOrStopping e =
  case e of
    Extinction -> True
    StoppingTime -> True
    _ -> False

et :: B.ByteString -> Csv.Record -> Bool
et bs r = (== bs) . head $ V.toList r

instance Csv.FromRecord EpidemicEvent where
  parseRecord r
    | et "infection" r =
      Infection <$> (r Csv..! 1) <*> (Person <$> Identifier <$> (r Csv..! 2)) <*>
      (Person <$> (r Csv..! 3))
    | et "removal" r =
      Removal <$> (r Csv..! 1) <*> (Person <$> Identifier <$> (r Csv..! 2))
    | et "sampling" r =
      Sampling <$> (r Csv..! 1) <*> (Person <$> Identifier <$> (r Csv..! 2))
    | et "catastrophe" r = Catastrophe <$> (r Csv..! 1) <*> (r Csv..! 2)
    | et "occurrence" r =
      Occurrence <$> (r Csv..! 1) <*> (Person <$> Identifier <$> (r Csv..! 2))
    | et "disaster" r = Disaster <$> (r Csv..! 1) <*> (r Csv..! 2)
    | otherwise = undefined

-- | Epidemic Events are ordered based on which occurred first. Since
-- 'Extinction' and 'StoppingTime' events are there as placeholders they are
-- placed as the end of the order.
instance Ord EpidemicEvent where
  Extinction <= Extinction = True
  Extinction <= StoppingTime = True
  Extinction <= _ = False
  StoppingTime <= Extinction = False
  StoppingTime <= StoppingTime = True
  StoppingTime <= _ = False
  e1 <= e2 = eventTime e1 <= eventTime e2

-- | The absolute time an event occurred.
eventTime :: EpidemicEvent -> AbsoluteTime
eventTime e =
  case e of
    Infection time _ _ -> time
    Removal time _ -> time
    Sampling time _ -> time
    Catastrophe time _ -> time
    Occurrence time _ -> time
    Disaster time _ -> time
    _ -> undefined

-- | The events that occurred as a result of the existance of the given person.
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
    Infection _ p1 p2 ->
      if includesPerson people p1 || includesPerson people p2
        then let people' = addPerson p2 (addPerson p1 people)
              in e : derivedFromPeople people' es
        else derivedFromPeople people es
    Removal _ p ->
      let derivedEvents = derivedFromPeople people es
       in if includesPerson people p
            then e : derivedEvents
            else derivedEvents
    Sampling _ p ->
      let derivedEvents = derivedFromPeople people es
       in if includesPerson people p
            then e : derivedEvents
            else derivedEvents
    Catastrophe _ ps ->
      let derivedEvents = derivedFromPeople people es
       in if haveCommonPeople people ps
            then e : derivedEvents
            else derivedEvents
    Occurrence _ p ->
      let derivedEvents = derivedFromPeople people es
       in if includesPerson people p
            then e : derivedEvents
            else derivedEvents
    Disaster _ ps ->
      let derivedEvents = derivedFromPeople people es
       in if haveCommonPeople people ps
            then e : derivedEvents
            else derivedEvents
    _ -> [e]

-- | A tree representation of /all/ the epidemic events including those that
-- were not observed. The scheduled observations can return a 'Left' or just
-- proceed to the next epidemic event if there were no people sampled. This is
-- because in these cases the event had no effect upon the transmission tree. If
-- there were multiple people sampled in a scheduled event, then this event will
-- appear in multiple leaves since each leaf is due to this event.
maybeEpidemicTree ::
     [EpidemicEvent] -- ^ ordered epidemic events
  -> Either String EpidemicTree
maybeEpidemicTree [] =
  Left "There are no EpidemicEvents to construct a tree with."
maybeEpidemicTree [e] =
  case e of
    Catastrophe _ people ->
      if nullPeople people
        then Left "The last event is a catastrophe with no people sampled"
        else Right (Leaf e)
    Disaster _ people ->
      if nullPeople people
        then Left "The last event is a disaster with no people sampled"
        else Right (Leaf e)
    Infection _ p1 p2 -> Right (Branch e (Shoot p1) (Shoot p2))
    Extinction -> Left "Extinction event encountered"
    StoppingTime -> Left "Stopping time encountered"
    _ -> Right (Leaf e)
maybeEpidemicTree (e:es:ess) =
  case e of
    Infection _ p1 p2 ->
      let infectorEvents = derivedFrom p1 (es : ess)
          infecteeEvents = derivedFrom p2 (es : ess)
       in do leftTree <-
               if null infectorEvents
                 then Right (Shoot p1)
                 else maybeEpidemicTree infectorEvents
             rightTree <-
               if null infecteeEvents
                 then Right (Shoot p2)
                 else maybeEpidemicTree infecteeEvents
             return $ Branch e leftTree rightTree
    Catastrophe _ people ->
      if nullPeople people
        then maybeEpidemicTree (es : ess)
        else Right (Leaf e)
    Disaster _ people ->
      if nullPeople people
        then maybeEpidemicTree (es : ess)
        else Right (Leaf e)
    _ -> Right (Leaf e)

class Newick t
  -- | Return a representation of the tree in Newick format.
  where
  asNewickString ::
       (AbsoluteTime, Person) -- ^ The person and time of the start of the tree
    -> t
    -> Maybe (BBuilder.Builder, [EpidemicEvent])

colonBuilder :: BBuilder.Builder
colonBuilder = BBuilder.charUtf8 ':'

leftBraceBuilder :: BBuilder.Builder
leftBraceBuilder = BBuilder.charUtf8 '('

rightBraceBuilder :: BBuilder.Builder
rightBraceBuilder = BBuilder.charUtf8 ')'

commaBuilder :: BBuilder.Builder
commaBuilder = BBuilder.charUtf8 ','

instance Newick EpidemicTree where
  asNewickString (_, p) (Shoot p') =
    if p /= p'
      then Nothing
      else let identifier = personByteString p
               branchLength = BBuilder.stringUtf8 "Infinity"
            in Just (identifier <> colonBuilder <> branchLength, [])
  asNewickString (t, p) (Leaf e) =
    let identifier = personByteString p
        branchLength a b = BBuilder.doubleDec td
          where
            (TimeDelta td) = timeDelta a b
     in case e of
          Infection {} -> Nothing
          (Removal t' p') ->
            if p /= p'
              then Nothing
              else Just (identifier <> colonBuilder <> branchLength t t', [e])
          (Sampling t' p') ->
            if p /= p'
              then Nothing
              else Just (identifier <> colonBuilder <> branchLength t t', [e])
          (Catastrophe t' ps) ->
            if ps `includesPerson` p
              then Just (identifier <> colonBuilder <> branchLength t t', [e])
              else Nothing
          (Occurrence t' p') ->
            if p /= p'
              then Nothing
              else Just (identifier <> colonBuilder <> branchLength t t', [e])
          (Disaster t' ps) ->
            if ps `includesPerson` p
              then Just (identifier <> colonBuilder <> branchLength t t', [e])
              else Nothing
          Extinction -> Nothing
          StoppingTime -> Nothing
  asNewickString (t, p) (Branch e lt rt) =
    case e of
      (Infection t' p1 p2) ->
        if p /= p1
          then Nothing
          else do
            (leftNS, leftEs) <- asNewickString (t', p1) lt
            (rightNS, rightEs) <- asNewickString (t', p2) rt
            let branchLength = BBuilder.doubleDec td
                  where
                    (TimeDelta td) = timeDelta t t'
            return
              ( leftBraceBuilder <>
                leftNS <>
                commaBuilder <>
                rightNS <> rightBraceBuilder <> colonBuilder <> branchLength
              , List.sort $ leftEs ++ rightEs)
      _ -> Nothing
