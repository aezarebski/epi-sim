{-# LANGUAGE RecordWildCards #-}

module Epidemic.Data.Newick (Newick, asNewickString) where

import qualified Data.ByteString.Builder    as BBuilder
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

instance Newick ReconstructedTree where
  asNewickString p rt = do
    ns <- reconTreeNewickHelper p rt
    return $ ns <> bb ';'

reconTreeNewickHelper (t, _) (RBranch (Observation e) lt rt) =
  case e of
    (Infection t' p1 p2) -> do
      leftNS <- reconTreeNewickHelper (t', p1) lt
      rightNS <- reconTreeNewickHelper (t', p2) rt
      let branchLength = branchLengthBuilder t t'
      return $ bb '(' <> leftNS <> bb ',' <> rightNS <> bb ')' <> bb ':' <> branchLength
    _ -> Left $ "branch of reconstructed tree does not contain an infection: " <> show e
reconTreeNewickHelper (t, _) (RBurr (Observation e) mt) =
  case e of
    IndividualSample {..} ->
      let branchLength = branchLengthBuilder t indSampTime
          idColonLength = personByteString indSampPerson <> bb ':' <> branchLength
      in case mt of
           Just reconT ->
             do tNS <- reconTreeNewickHelper (indSampTime, indSampPerson) reconT
                return $ bb '(' <> tNS <> bb ')' <> idColonLength
           Nothing ->
             return idColonLength
    _ -> Left $ "burr of reconstructed tree does not contain an individual sample: " <> show e
reconTreeNewickHelper (t, _) (RLeaf (Observation e)) =
  case e of
    IndividualSample {..}
      | indSampSeq && indSampRemoved -> return $ personByteString indSampPerson <> bb ':' <> branchLengthBuilder t indSampTime
      | indSampSeq -> Left $ "sequenced individual sample not removed in reconstructed leaf: " <> show e
      | otherwise -> Left $ "non-sequenced individual sample in reconstructed tree: " <> show e
    PopulationSample {..} ->
      if popSampSeq
      then return $ peopleByteString popSampPeople <> bb ':' <> branchLengthBuilder t popSampTime
      else Left $ "non-sequenced population sample in reconstructed tree: " <> show e
    _ -> Left $ "leaf of reconstructed tree does not contain a sample: " <> show e

instance Newick EpidemicTree where
  asNewickString p rt = do
    ns <- epiTreeNewickHelper p rt
    return $ ns <> bb ';'

epiTreeNewickHelper (t, _) (Branch e lt rt) =
  case e of
    Infection {..} -> do
      lNS <- epiTreeNewickHelper (infTime, undefined) lt
      rNS <- epiTreeNewickHelper (infTime, undefined) rt
      let bl = branchLengthBuilder t infTime
      return $ bb '(' <> lNS <> bb ',' <> rNS <> bb ')' <> bb ':' <> bl
    _ -> Left $ "branch of epidemic tree does not contain and infection: " <> show e
epiTreeNewickHelper (t, _) (Burr e st) =
  case (e,st) of
    (IndividualSample {..}, Shoot p) -> do
      let bl = branchLengthBuilder t indSampTime
          id = personByteString indSampPerson
      return $ id <> bb ':' <> bl
    (IndividualSample {..}, _) -> do
      sNS <- epiTreeNewickHelper (indSampTime, undefined) st
      let bl = branchLengthBuilder t indSampTime
          id = personByteString indSampPerson
      return $ bb '(' <> sNS <> bb ')' <> id <> bb ':' <> bl
    _ -> Left $ "burr of epidemic tree does not contain an individual sample: " <> show e
epiTreeNewickHelper (t, _) (Leaf e) =
  case e of
    IndividualSample {..} ->
      return $ personByteString indSampPerson <> bb ':' <> branchLengthBuilder t indSampTime
    PopulationSample {..} ->
      return $ peopleByteString popSampPeople <> bb ':' <> branchLengthBuilder t popSampTime
    _ -> Left $ "leaf of epidemic tree does not contain individual or population sample, nor removal: " <> show e
epiTreeNewickHelper _ Shoot {} = Left "shoot branch should never be reached."
