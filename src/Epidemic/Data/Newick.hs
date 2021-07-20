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
    -> Either String BBuilder.Builder

bb :: Char -> BBuilder.Builder
bb = BBuilder.charUtf8

branchLengthBuilder :: AbsoluteTime -> AbsoluteTime -> BBuilder.Builder
branchLengthBuilder t1 t2 =
  let (TimeDelta td) = timeDelta t1 t2
  in BBuilder.doubleDec td

catastrophePeopleBuilder :: People -> BBuilder.Builder
catastrophePeopleBuilder (People ps) =
  mconcat . List.intersperse (bb '&') $ map personByteString pList
  where pList = Set.toList ps

instance Newick ReconstructedTree where
  asNewickString (t, _) (RLeaf (Observation e)) =
    case e of
      IndividualSample {..} ->
        if indSampSeq
        then return $ personByteString indSampPerson <> bb ':' <> branchLengthBuilder t indSampTime
        else Left $ "non-sequenced individual sample in reconstructed tree: " <> show e
      PopulationSample {..} ->
        if popSampSeq
        then return $ catastrophePeopleBuilder popSampPeople <> bb ':' <> branchLengthBuilder t popSampTime
        else Left $ "non-sequenced population sample in reconstructed tree: " <> show e
      _ -> Left $ "leaf of reconstructed tree does not contain a sample: " <> show e
  asNewickString (t, _) (RBranch (Observation e) lt rt) =
    case e of
      (Infection t' p1 p2) -> do
        leftNS <- asNewickString (t', p1) lt
        rightNS <- asNewickString (t', p2) rt
        let branchLength = branchLengthBuilder t t'
        return $ bb '(' <> leftNS <> bb ',' <> rightNS <> bb ')' <> bb ':' <> branchLength
      _ -> Left $ "branch of reconstructed tree does not contain an infection: " <> show e
  asNewickString (t, _) (RBurr (Observation e) mt) =
    case e of
      IndividualSample {..} ->
        let branchLength = branchLengthBuilder t indSampTime
            idColonLength = personByteString indSampPerson <> bb ':' <> branchLength
        in case mt of
             Just reconT ->
               do tNS <- asNewickString (indSampTime, indSampPerson) reconT
                  return $ bb '(' <> tNS <> bb ')' <> idColonLength
             Nothing ->
               return idColonLength
      _ -> Left $ "burr of reconstructed tree does not contain an individual sample: " <> show e
