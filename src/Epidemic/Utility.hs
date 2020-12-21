{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Epidemic.Utility where

import Epidemic.Types.Events
import Epidemic.Types.Parameter
import Epidemic.Types.Population
import Control.Monad (liftM)
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as Char8
import GHC.Generics (Generic)
import qualified Data.Vector as V
import System.Random.MWC
import System.Random.MWC.Distributions (exponential)
import Control.Monad.Primitive (PrimMonad, PrimState)
import Control.Applicative
import Text.Trifecta

import Epidemic

data SimulationConfiguration r p =
  SimulationConfiguration
    { rates :: r
    , population :: p
    , newIdentifier :: Identifier
    , timeLimit :: AbsoluteTime
    }

initialIdentifier :: Identifier
initialIdentifier = Identifier 1

newPerson :: Identifier -> (Person, Identifier)
newPerson id@(Identifier idInt)= (Person id, Identifier (idInt + 1))

selectElem :: V.Vector a -> Int -> (a, V.Vector a)
selectElem v n
  | n == 0 = (V.head v, V.tail v)
  | otherwise =
    let (foo, bar) = V.splitAt n v
     in (V.head bar, foo V.++ (V.tail bar))

randomPerson :: People -> GenIO -> IO (Person, People)
randomPerson people@(People persons) gen = do
  u <- uniform gen
  let personIx = floor (u * (fromIntegral $ numPeople people :: Double))
      (person, remPeople) = selectElem persons personIx
   in return (person, People remPeople)


type NName = Maybe String

type NLength = Maybe Double

data NBranch = NBranch NSubtree NLength deriving (Eq)

instance Show NBranch where
  show (NBranch st (Just l)) = show st ++ ":" ++ show l
  show (NBranch st Nothing) = show st

data NBranchSet = NBranchSet [NBranch] deriving (Eq)

instance Show NBranchSet where
  show (NBranchSet bs) = "(" ++ (List.intercalate "," (map show bs)) ++ ")"

data NSubtree = NLeaf NName | NInternal NBranchSet deriving (Eq)

instance Show NSubtree where
  show (NLeaf (Just n)) = n
  show (NLeaf Nothing) = ""
  show (NInternal bs) = show bs

data NTree = NTree [NBranch] deriving (Eq)

instance Show NTree where
  show (NTree bs) = show (NBranchSet bs) ++ ";"

-- Name → empty | string
newickName :: (Monad f, CharParsing f) => f NName
newickName = optional (some alphaNum) >>= pure

-- Leaf → Name
newickLeaf :: (Monad f, CharParsing f) => f NSubtree
newickLeaf = do
  n <- newickName
  pure (NLeaf n)

-- Length → empty | ":" number
newickLength :: (TokenParsing f, Monad f, CharParsing f) => f NLength
newickLength = do
  maybeLength <- optional ((symbolic ':') >> double)
  pure maybeLength

-- Branch → Subtree Length
newickBranch :: (TokenParsing f, Monad f, CharParsing f) => f NBranch
newickBranch = do
  st <- newickSubtree
  l <- newickLength
  pure (NBranch st l)

-- BranchSet → Branch | Branch "," BranchSet
newickBranchSet :: (TokenParsing f, Monad f, CharParsing f) => f NBranchSet
newickBranchSet = do
  bs <- sepBy1 newickBranch comma
  pure (NBranchSet bs)

-- Internal → "(" BranchSet ")" Name
newickInternal :: (TokenParsing f, Monad f, CharParsing f) => f NSubtree
newickInternal = do
  bs <- parens newickBranchSet
  pure (NInternal bs)

-- Subtree → Leaf | Internal
newickSubtree :: (TokenParsing f, Monad f, CharParsing f) => f NSubtree
newickSubtree = choice [newickInternal,newickLeaf]

-- Tree → Subtree ";" | Branch ";"
newickTree :: (TokenParsing f, Monad f, CharParsing f) => f NTree
newickTree = do
  (NBranchSet bs) <- parens newickBranchSet
  symbolic ';'
  pure (NTree bs)

-- | Example run
--   > (Success foo) = parseString newickTree mempty "((foo:1.1,bar:1.2):1.3,baz:1.4);"
--   > (Success bar) = parseString newickTree mempty $ show foo
--   > foo == bar
--   True

sort :: Ord a => [a] -> [a]
sort = List.sort

count' :: (a -> Bool) -> [a] -> Int
count' p = go 0
  where go n [] = n
        go n (x:xs) | p x       = go (n+1) xs
                     | otherwise = go n xs

-- | Run a simulation described by a configuration object.
simulation :: (ModelParameters a)
           => Bool  -- ^ Condition upon at least two leaves in the reconstructed tree
           -> SimulationConfiguration a b
           -> (a -> AbsoluteTime -> (AbsoluteTime, [EpidemicEvent], b, Identifier) -> GenIO -> IO (AbsoluteTime, [EpidemicEvent], b, Identifier))
           -> IO [EpidemicEvent]
simulation True config allEvents = do
  gen <- System.Random.MWC.create :: IO GenIO
  simulation' config allEvents gen
simulation False SimulationConfiguration {..} allEvents = do
  gen <- System.Random.MWC.create :: IO GenIO
  (_, events, _, _) <-
    allEvents rates timeLimit (AbsoluteTime 0, [], population, newIdentifier) gen
  return $ sort events

-- | Predicate for whether an epidemic event is either an occurrence or a
-- disaaster.
isNonReconTreeObservation :: EpidemicEvent -> Bool
isNonReconTreeObservation e = case e of
  Occurrence {} -> True
  Disaster {} -> True
  _ -> False

-- | Predicate for whether an epidemic event will appear as a leaf in the
-- reconstructed tree.
isReconTreeLeaf :: EpidemicEvent -> Bool
isReconTreeLeaf e = case e of
  Sampling {} -> True
  Catastrophe {} -> True
  _ -> False


simulation' :: (ModelParameters a) => SimulationConfiguration a b
           -> (a -> AbsoluteTime -> (AbsoluteTime, [EpidemicEvent], b, Identifier) -> GenIO -> IO (AbsoluteTime, [EpidemicEvent], b, Identifier))
           -> GenIO
           -> IO [EpidemicEvent]
simulation' config@SimulationConfiguration {..} allEvents gen = do
  (_, events, _, _) <-
    allEvents rates timeLimit (AbsoluteTime 0, [], population, newIdentifier) gen
  if count' isReconTreeLeaf events >= 2
    then return $ sort events
    else simulation' config allEvents gen


-- | Run a simulation described by a configuration object but using a random
-- seed generated by the system rather than a seed
simulationWithSystemRandom :: (ModelParameters a)
                           => Bool  -- ^ Condition upon at least two leaves in the reconstructed tree
                           -> SimulationConfiguration a b
                           -> (a -> AbsoluteTime -> (AbsoluteTime, [EpidemicEvent], b, Identifier) -> GenIO -> IO (AbsoluteTime, [EpidemicEvent], b, Identifier))
                           -> IO [EpidemicEvent]
simulationWithSystemRandom atLeastCherry config@SimulationConfiguration {..} allEvents = do
  (_, events, _, _) <-
    withSystemRandom $ \g ->
      allEvents rates timeLimit (AbsoluteTime 0, [], population, newIdentifier) g
  if atLeastCherry
    then (if count' isReconTreeLeaf events >= 2
           then return $ sort events
           else simulationWithSystemRandom True config allEvents)
    else return $ sort events


-- | The number of lineages at the end of a simulation.
finalSize :: [EpidemicEvent] -- ^ The events from the simulation
          -> Integer
finalSize = foldl (\x y -> x + eventPopDelta y) 1

-- | Generate exponentially distributed random variates with inhomogeneous rate
-- starting from a particular point in time.
--
-- Assuming the @stepFunc@ is the intensity of arrivals and @t0@ is the start
-- time this returns @t1@ the time of the next arrival.
inhomExponential :: PrimMonad m
                 => Timed Double      -- ^ Step function
                 -> AbsoluteTime      -- ^ Start time
                 -> Gen (PrimState m) -- ^ Generator
                 -> m (Maybe AbsoluteTime)
inhomExponential stepFunc t0 = randInhomExp t0 stepFunc

-- | Generate exponentially distributed random variates with inhomogeneous rate.
--
-- __TODO__ The algorithm used here generates more variates than are needed. It
-- would be nice to use a more efficient implementation.
--
randInhomExp :: PrimMonad m
             => AbsoluteTime      -- ^ Timer
             -> Timed Double      -- ^ Step function
             -> Gen (PrimState m) -- ^ Generator.
             -> m (Maybe AbsoluteTime)
randInhomExp crrT stepFunc gen = 
  let crrR = cadlagValue stepFunc crrT
      nxtT = nextTime stepFunc crrT
   in if Maybe.isJust crrR && Maybe.isJust nxtT
        then do
          crrD <- exponential (Maybe.fromJust crrR) gen
          let propT = timeAfterDelta crrT (TimeDelta crrD)
          if propT < Maybe.fromJust nxtT
            then return $ Just propT
            else randInhomExp (Maybe.fromJust nxtT) stepFunc gen
        else return Nothing
