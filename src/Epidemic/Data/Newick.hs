{-# LANGUAGE RecordWildCards #-}

module Epidemic.Data.Newick where

import qualified Data.ByteString.Builder    as BBuilder
import qualified Data.List                  as List
import qualified Data.Set                   as Set
import           Epidemic.Data.Events
import           Epidemic.Data.Observations
import           Epidemic.Data.Population
import           Epidemic.Data.Time

-- | Class of types that can be expressed in Newick format.
class Newick t
  where
  asNewickString ::
       (AbsoluteTime, Person) -- ^ The person and time of the root of the tree
    -> t
    -> Either String (BBuilder.Builder, [EpidemicEvent])

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
catastrophePeopleBuilder (People ps) =
  mconcat . List.intersperse ampersandBuilder $ map personByteString pList
  where pList = Set.toList ps

instance Newick ReconstructedTree where
  asNewickString (t, _) (RLeaf (Observation e)) =
    let branchLength a b = BBuilder.doubleDec td
          where
            (TimeDelta td) = timeDelta a b
     in case e of
          IndividualSample {..} ->
            if indSampSeq
              then return
                     ( personByteString indSampPerson <>
                       colonBuilder <> branchLength t indSampTime
                     , [e])
              else Left $ "non-sequenced individual sample in reconstructed tree: " <> show e
          PopulationSample {..} ->
            if popSampSeq
              then return
                     ( catastrophePeopleBuilder popSampPeople <>
                       colonBuilder <> branchLength t popSampTime
                     , [e])
              else Left $ "non-sequenced population sample in reconstructed tree: " <> show e
          _ -> Left $ "leaf of reconstructed tree does not contain a sample: " <> show e
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
      _ -> Left $ "branch of reconstructed tree does not contain an infection: " <> show e
