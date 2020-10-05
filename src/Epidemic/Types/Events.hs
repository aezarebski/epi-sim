{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Epidemic.Types.Events
  ( EpidemicEvent(Infection, Removal, Sampling, Catastrophe,
              Occurrence, Disaster)
  , EpidemicTree(Branch, Leaf, Shoot)
  , maybeEpidemicTree
  , eventTime
  , ReconstructedTree(RBranch, RLeaf)
  , maybeReconstructedTree
  , PointProcessEvents(PointProcessEvents)
  , pointProcessEvents
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
  = Infection Time Person Person -- ^ infection time, infector, infectee
  | Removal Time Person          -- ^ removal without observation
  | Sampling Time Person         -- ^ removal and inclusion in phylogeny
  | Catastrophe Time People      -- ^ scheduled sampling of lineages
  | Occurrence Time Person       -- ^ removal and observed by not in phylogeny
  | Disaster Time People         -- ^ scheduled occurrence of lineages
  deriving (Show, Generic, Eq)

instance Json.FromJSON EpidemicEvent

instance Json.ToJSON EpidemicEvent

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

et :: B.ByteString -> Csv.Record -> Bool
et bs r = (== bs) . head $ V.toList r

instance Csv.FromRecord EpidemicEvent where
  parseRecord r
    | et "infection" r =
      Infection <$> (r Csv..! 1) <*> (Person <$> (r Csv..! 2)) <*>
      (Person <$> (r Csv..! 3))
    | et "removal" r = Removal <$> (r Csv..! 1) <*> (Person <$> (r Csv..! 2))
    | et "sampling" r = Sampling <$> (r Csv..! 1) <*> (Person <$> (r Csv..! 2))
    | et "catastrophe" r = Catastrophe <$> (r Csv..! 1) <*> (r Csv..! 2)
    | et "occurrence" r =
      Occurrence <$> (r Csv..! 1) <*> (Person <$> (r Csv..! 2))
    | et "disaster" r = Disaster <$> (r Csv..! 1) <*> (r Csv..! 2)
    | otherwise = undefined

-- | Epidemic Events are ordered based on which occurred first.
instance Ord EpidemicEvent where
  e1 <= e2 = eventTime e1 <= eventTime e2

-- | The absolute time an event occurred.
eventTime :: EpidemicEvent -> Time
eventTime e =
  case e of
    Infection time _ _ -> time
    Removal time _ -> time
    Sampling time _ -> time
    Catastrophe time _ -> time
    Occurrence time _ -> time
    Disaster time _ -> time

-- | The events that occurred as a result of the existance of the given person.
derivedFrom :: Person
            -> [EpidemicEvent]  -- ^ ordered epidemic events
            -> [EpidemicEvent]
derivedFrom person = derivedFromPeople (asPeople [person])

-- | The events that occurred as a result of the existance of a group of people
derivedFromPeople :: People
                  -> [EpidemicEvent]  -- ^ ordered epidemic events
                  -> [EpidemicEvent]
derivedFromPeople _ [] = []
derivedFromPeople people (e:es) = case e of
  Infection _ p1 p2 -> if includesPerson people p1 || includesPerson people p2
                       then let people' = addPerson p2 (addPerson p1 people)
                             in e : derivedFromPeople people' es
                       else derivedFromPeople people es
  Removal _ p -> let derivedEvents = derivedFromPeople people es
                  in if includesPerson people p
                     then e:derivedEvents
                     else derivedEvents
  Sampling _ p -> let derivedEvents = derivedFromPeople people es
                   in if includesPerson people p
                      then e:derivedEvents
                      else derivedEvents
  Catastrophe _ ps -> let derivedEvents = derivedFromPeople people es
                       in if haveCommonPeople people ps
                          then e:derivedEvents
                          else derivedEvents
  Occurrence _ p -> let derivedEvents = derivedFromPeople people es
                     in if includesPerson people p
                        then e:derivedEvents
                        else derivedEvents
  Disaster _ ps -> let derivedEvents = derivedFromPeople people es
                    in if haveCommonPeople people ps
                       then e:derivedEvents
                       else derivedEvents

{-| A representation of the whole transmission tree in a realisation of an
epidemic including the unobserved leaves. Lineages that are still extant are
modelled as shoots and contain a `Person` as their data rather than an event.
-}
data EpidemicTree
  = Branch EpidemicEvent EpidemicTree EpidemicTree -- ^ Internal node representing infection event
  | Leaf EpidemicEvent                             -- ^ External node representing removal event
  | Shoot Person                                   -- ^ External node representing extant lineages
  deriving (Show,Eq)


-- | A tree representation of the epidemic events.
maybeEpidemicTree :: [EpidemicEvent] -- ^ ordered epidemic events
                  -> Maybe EpidemicTree
maybeEpidemicTree [] = Nothing
maybeEpidemicTree [e] = case e of
  Catastrophe _ people -> if nullPeople people
                          then Nothing
                          else Just (Leaf e)
  Disaster _ people -> if nullPeople people
                       then Nothing
                       else Just (Leaf e)
  Infection _ p1 p2 -> Just (Branch e (Shoot p1) (Shoot p2))
  _ -> Just (Leaf e)
maybeEpidemicTree (e:es:ess) =
  case e of
    Infection _ p1 p2 ->
      let infectorEvents = derivedFrom p1 (es : ess)
          infecteeEvents = derivedFrom p2 (es : ess)
       in do leftTree <-
               if null infectorEvents
               then Just (Shoot p1)
               else maybeEpidemicTree infectorEvents
             rightTree <-
               if null infecteeEvents
               then Just (Shoot p2)
               else maybeEpidemicTree infecteeEvents
             return $ Branch e leftTree rightTree
    Catastrophe _ people -> if nullPeople people
                            then maybeEpidemicTree (es:ess)
                            else Just (Leaf e)
    Disaster _ people -> if nullPeople people
                         then maybeEpidemicTree (es:ess)
                         else Just (Leaf e)
    _ -> Just (Leaf e)

{-| A representation of the reconstructed tree which is the phylogeny connecting
  all the `Sampling` and `Catastrophe` events.
-}
data ReconstructedTree
  = RBranch EpidemicEvent ReconstructedTree ReconstructedTree
  | RLeaf EpidemicEvent
  deriving (Show, Eq)


-- | A tree representation of the reconstructed phylogeny.
maybeReconstructedTree :: EpidemicTree -> Maybe ReconstructedTree

maybeReconstructedTree Shoot{} = Nothing

maybeReconstructedTree (Leaf e) = case e of
  Sampling{} -> Just $ RLeaf e
  Catastrophe{} -> Just $ RLeaf e
  _ -> Nothing

maybeReconstructedTree (Branch e@Infection{} lt rt)
  | hasSequencedLeaf lt && hasSequencedLeaf rt =
    do
      rlt <- maybeReconstructedTree lt
      rrt <- maybeReconstructedTree rt
      Just $ RBranch e rlt rrt
  | hasSequencedLeaf lt = maybeReconstructedTree lt
  | hasSequencedLeaf rt = maybeReconstructedTree rt
  | otherwise = Nothing
maybeReconstructedTree Branch{} = Nothing

-- | Predicate for whether an `EpidemicTree` has a leaf which corresponds to a
-- node in the `ReconstructedTree`.
hasSequencedLeaf :: EpidemicTree -> Bool

hasSequencedLeaf Shoot {} = False

hasSequencedLeaf (Leaf e) =
  case e of
    Sampling {} -> True
    Catastrophe {} -> True
    _ -> False

hasSequencedLeaf (Branch _ lt rt) = hasSequencedLeaf lt || hasSequencedLeaf rt


{-| A representation of the events that can be observed in an epidemic but which
  are not included in the reconstructed tree, i.e. the `Occurrence` and
  `Disaster` events.
-}
newtype PointProcessEvents = PointProcessEvents [EpidemicEvent]

-- | Extract the events from an epidemic tree which are observed but not part of
-- the reconstructed tree.
pointProcessEvents :: EpidemicTree -> PointProcessEvents

pointProcessEvents Shoot {} = PointProcessEvents []

pointProcessEvents (Leaf e) = case e of
  Occurrence {} -> PointProcessEvents [e]
  Disaster {} -> PointProcessEvents [e]
  _ -> PointProcessEvents []

pointProcessEvents (Branch _ lt rt) =
  let (PointProcessEvents lEs) = pointProcessEvents lt
      (PointProcessEvents rEs) = pointProcessEvents rt
      allEs = List.sort $ lEs ++ rEs
      in PointProcessEvents allEs

class Newick t where
  -- | Return a representation of the tree in Newick format.
  asNewickString :: (Time,Person) -> t -> Maybe (BBuilder.Builder, [EpidemicEvent])


ampersandBuilder :: BBuilder.Builder
ampersandBuilder = BBuilder.charUtf8 '&'

colonBuilder :: BBuilder.Builder
colonBuilder = BBuilder.charUtf8 ':'

leftBraceBuilder :: BBuilder.Builder
leftBraceBuilder = BBuilder.charUtf8 '('

rightBraceBuilder :: BBuilder.Builder
rightBraceBuilder = BBuilder.charUtf8 ')'

commaBuilder :: BBuilder.Builder
commaBuilder = BBuilder.charUtf8 ','

catastrophePeopleBuilder :: People -> BBuilder.Builder
catastrophePeopleBuilder (People persons) =
  mconcat $
  List.intersperse ampersandBuilder [personByteString p | p <- V.toList persons]


instance Newick EpidemicTree where
  asNewickString (_, p) (Shoot p') =
    if p /= p'
      then Nothing
      else let identifier = personByteString p
               bl = BBuilder.stringUtf8 "Infinity"
            in Just (identifier <> colonBuilder <> bl, [])

  asNewickString (t, p) (Leaf e) =
    let identifier = personByteString p
        bl a b = BBuilder.doubleDec $ b - a
    in case e of
      Infection {} -> Nothing
      (Removal t' p') ->
        if p /= p'
        then Nothing
        else Just (identifier <> colonBuilder <> bl t t', [e])
      (Sampling t' p') ->
        if p /= p'
        then Nothing
        else Just (identifier <> colonBuilder <> bl t t', [e])
      (Catastrophe t' ps) ->
        if ps `includesPerson` p
        then Just (identifier <> colonBuilder <> bl t t', [e])
        else Nothing
      (Occurrence t' p') ->
        if p /= p'
        then Nothing
        else Just (identifier <> colonBuilder <> bl t t', [e])
      (Disaster t' ps) ->
        if ps `includesPerson` p
        then Just (identifier <> colonBuilder <> bl t t', [e])
        else Nothing

  asNewickString (t, p) (Branch e lt rt) =
    case e of
      (Infection t' p1 p2) ->
        if p /= p1
          then Nothing
          else do
            (leftNS, leftEs) <- asNewickString (t', p1) lt
            (rightNS, rightEs) <- asNewickString (t', p2) rt
            let bl = BBuilder.doubleDec $ t' - t
            return
              ( leftBraceBuilder <>
                leftNS <>
                commaBuilder <> rightNS <> rightBraceBuilder <> colonBuilder <> bl
              , List.sort $ leftEs ++ rightEs)
      _ -> Nothing



instance Newick ReconstructedTree where
  asNewickString (t, _) (RLeaf e) =
    let bl a b = BBuilder.doubleDec $ b - a
    in case e of
      (Sampling t' p) -> Just ((personByteString p) <> colonBuilder <> bl t t', [e])
      Infection {} -> Nothing
      Removal {} -> Nothing
      (Catastrophe t' ps) -> Just (catastrophePeopleBuilder ps <> colonBuilder <> bl t t', [e])
      Occurrence {} -> Nothing
      Disaster {} -> Nothing

  asNewickString (t, _) (RBranch e lt rt) =
    case e of
      (Infection t' p1 p2) ->
        do
          (leftNS, leftEs) <- asNewickString (t', p1) lt
          (rightNS, rightEs) <- asNewickString (t', p2) rt
          let bl = BBuilder.doubleDec $ t' - t
          return
            ( leftBraceBuilder <>
              leftNS <>
              commaBuilder <> rightNS <> rightBraceBuilder <> colonBuilder <> bl
            , List.sort $ leftEs ++ rightEs)
      _ -> Nothing