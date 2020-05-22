{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Epidemic.Utility where

import Epidemic.Types
import Control.Monad (liftM)
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as Char8
import GHC.Generics (Generic)
import qualified Data.Vector as V
import System.Random.MWC
import System.Random.MWC.Distributions
import Control.Monad.Primitive (PrimMonad, PrimState)
import Control.Applicative
import Text.Trifecta

import Epidemic

data SimulationConfiguration r p =
  SimulationConfiguration
    { rates :: r
    , population :: p
    , newIdentifier :: Identifier
    , timeLimit :: Time
    }

initialIdentifier :: Identifier
initialIdentifier = 1

newPerson :: Identifier -> (Person, Identifier)
newPerson identifier = (Person identifier, identifier + 1)

selectElem :: V.Vector a -> Int -> (a, V.Vector a)
selectElem v n
  | n == 0 = (V.head v, V.tail v)
  | otherwise =
    let (foo, bar) = V.splitAt n v
     in (V.head bar, foo V.++ (V.tail bar))

randomPerson :: V.Vector Person -> GenIO -> IO (Person, V.Vector Person)
randomPerson persons gen = do
  u <- uniform gen
  return $ selectElem persons (floor (u * numPersons))
  where
    numPersons = fromIntegral $ V.length persons :: Double



eventAsTreeObject :: Event -> Char8.ByteString
eventAsTreeObject e =
  case e of
    (RemovalEvent _ _) -> B.empty
    (InfectionEvent t (Person infectorId) (Person infecteeId)) ->
      B.concat
        ["{", infecteeByteString, infectorByteString, timeByteString, "}"]
      where infecteeByteString =
              Char8.pack ("\"id\":" ++ (Prelude.show infecteeId))
            infectorByteString =
              Char8.pack (",\"parent\":" ++ (Prelude.show infectorId))
            timeByteString = Char8.pack (",\"time\":" ++ (Prelude.show t))

eventsAsJsonTree :: [Event] -> Char8.ByteString
eventsAsJsonTree es =
  let objects =
        B.intercalate "," $ [eventAsTreeObject e | e <- es, isInfection e]
   in B.concat ["[", objects, ",{\"id\":1,\"time\":0}", "]"]



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
           -> (a -> Time -> (Time, [Event], b, Identifier) -> GenIO -> IO (Time, [Event], b, Identifier))
           -> IO [Event]
simulation True config allEvents = do
  gen <- System.Random.MWC.create :: IO GenIO
  simulation' config allEvents gen
simulation False SimulationConfiguration {..} allEvents = do
  gen <- System.Random.MWC.create :: IO GenIO
  (_, events, _, _) <-
    allEvents rates timeLimit (0, [], population, newIdentifier) gen
  return $ sort events

simulation' :: (ModelParameters a) => SimulationConfiguration a b
           -> (a -> Time -> (Time, [Event], b, Identifier) -> GenIO -> IO (Time, [Event], b, Identifier))
           -> GenIO
           -> IO [Event]
simulation' config@SimulationConfiguration {..} allEvents gen = do
  (_, events, _, _) <-
    allEvents rates timeLimit (0, [], population, newIdentifier) gen
  if count' isSampling events >= 2
    then return $ sort events
    else simulation' config allEvents gen


-- | Run a simulation described by a configuration object but using a random
-- seed generated by the system rather than a seed
simulationWithSystemRandom :: (ModelParameters a)
                           => Bool  -- ^ Condition upon at least two leaves in the reconstructed tree
                           -> SimulationConfiguration a b
                           -> (a -> Time -> (Time, [Event], b, Identifier) -> GenIO -> IO (Time, [Event], b, Identifier))
                           -> IO [Event]
simulationWithSystemRandom atLeastCherry config@SimulationConfiguration {..} allEvents = do
  (_, events, _, _) <-
    withSystemRandom $ \g ->
      allEvents rates timeLimit (0, [], population, newIdentifier) g
  if atLeastCherry
    then (if count' isSampling events >= 2
           then return $ sort events
           else simulationWithSystemRandom True config allEvents)
    else return $ sort events


-- | The number of lineages at the end of a simulation.
finalSize :: [Event] -- ^ The events from the simulation
          -> Integer
finalSize = foldl (\x y -> x + eventPopDelta y) 1

-- | Generate exponentially distributed random variates with inhomogeneous rate.
inhomExponential :: PrimMonad m
                 => Timed Double      -- ^ Step function
                 -> Gen (PrimState m) -- ^ Generator.
                 -> m Double
inhomExponential stepFunc gen = do
  maybeVariate <- randInhomExp 0 stepFunc gen
  if Maybe.isJust maybeVariate
    then return $ Maybe.fromJust maybeVariate
    else inhomExponential stepFunc gen

-- | Generate exponentially distributed random variates with inhomogeneous rate.
randInhomExp :: PrimMonad m
             => Double            -- ^ Timer
             -> Timed Double      -- ^ Step function
             -> Gen (PrimState m) -- ^ Generator.
             -> m (Maybe Double)
randInhomExp crrT stepFunc gen =
  let crrR = cadlagValue stepFunc crrT
      nxtT = nextTime stepFunc crrT
   in if (Maybe.isJust crrR && Maybe.isJust nxtT)
        then do
          crrD <- exponential (Maybe.fromJust crrR) gen
          if crrT + crrD < (Maybe.fromJust nxtT)
            then return $ Just (crrD + crrT)
            else (randInhomExp (Maybe.fromJust nxtT) stepFunc gen)
        else return Nothing

-- TODO This function should really be moved into @epi-types@!!!
joinTimed :: Timed a -> Timed a -> Timed a
joinTimed (Timed t1) (Timed t2) = Timed $ List.sortOn fst (t1 ++ t2)
