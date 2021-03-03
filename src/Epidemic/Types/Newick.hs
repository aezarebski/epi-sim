{-# LANGUAGE RecordWildCards #-}

module Epidemic.Types.Newick where

import qualified Data.Aeson as Json
import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as BBuilder
import qualified Data.List as List
import qualified Data.Vector as V
import Epidemic.Types.Parameter
import Epidemic.Types.Observations
import Epidemic.Types.Events
import Epidemic.Types.Population
import Epidemic.Types.Time
import GHC.Generics

-- | Class of types that can be expressed in Newick format.
class Newick t
  where
  asNewickString ::
       (AbsoluteTime, Person) -- ^ The person and time of the root of the tree
    -> t
    -> Maybe (BBuilder.Builder, [EpidemicEvent])

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

instance Newick ReconstructedTree where
  asNewickString (t, _) (RLeaf (Observation e)) =
    let branchLength a b = BBuilder.doubleDec td
          where
            (TimeDelta td) = timeDelta a b
     in case e of
          IndividualSample {..} ->
            if indSampSeq
              then Just
                     ( (personByteString indSampPerson) <>
                       colonBuilder <> branchLength t indSampTime
                     , [e])
              else Nothing
          PopulationSample {..} ->
            if popSampSeq
              then Just
                     ( catastrophePeopleBuilder popSampPeople <>
                       colonBuilder <> branchLength t popSampTime
                     , [e])
              else Nothing
          _ -> Nothing
  asNewickString (t, _) (RBranch (Observation e) lt rt) =
    case e of
      (Infection t' p1 p2) -> do
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
