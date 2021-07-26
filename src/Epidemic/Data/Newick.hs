{-# LANGUAGE RecordWildCards #-}

-- |
-- Module: Epidemic.Data.Parameter
-- Copyright: (c) 2021 Alexander E. Zarebski
-- License: MIT
--
-- Maintainer: Alexander E. Zarebski <aezarebski@gmail.com>
-- Stability: unstable
-- Portability: ghc
--
-- This module provides functionality for representing tree-like data in [Newick
-- format](https://en.wikipedia.org/wiki/Newick_format).
--

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
  asNewickString tAndP rt = do
    ns <- reconTreeNewickHelper tAndP rt
    return $ ns <> bb ';'

reconTreeNewickHelper :: (AbsoluteTime,Person) -> ReconstructedTree -> Either String BBuilder.Builder
reconTreeNewickHelper (t,p) rt =
  case rt of
    RBranch (ObsBranch t') lrt rrt ->
      do leftNS <- reconTreeNewickHelper (t', undefined) lrt
         rightNS <- reconTreeNewickHelper (t', undefined) rrt
         let branchLength = branchLengthBuilder t t'
         return $ bb '(' <> leftNS <> bb ',' <> rightNS <> bb ')' <> bb ':' <> branchLength
    RBurr (ObsBurr t' p') maybeRt ->
      let branchLength = branchLengthBuilder t t'
          idColonLength = personByteString p' <> bb ':' <> branchLength
      in case maybeRt of
           Just reconT ->
             do tNS <- reconTreeNewickHelper (t', p') reconT
                return $ bb '(' <> tNS <> bb ')' <> idColonLength
           Nothing -> return idColonLength
    RLeaf (ObsLeafRemoved t' p') -> return $ personByteString p' <> bb ':' <> branchLengthBuilder t t'
    RLeaf (ObsLeafScheduled t' ps') -> return $ peopleByteString ps' <> bb ':' <> branchLengthBuilder t t'
    _ -> Left $ "invalid reconstructed tree encountered: " <> show rt

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
