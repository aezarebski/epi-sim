{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Epidemic.Utility where

import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as Char8
import GHC.Generics (Generic)
import qualified Data.Vector as V
import System.Random.MWC
import System.Random.MWC.Distributions

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



-- | Run a simulation described by a configuration object.
simulation :: (ModelParameters a) => SimulationConfiguration a b
           -> (a -> Time -> (Time, [Event], b, Identifier) -> GenIO -> IO (Time, [Event], b, Identifier))
           -> IO [Event]
simulation SimulationConfiguration {..} allEvents = do
  gen <- System.Random.MWC.create :: IO GenIO
  (_, events, _, _) <-
    allEvents rates timeLimit (0, [], population, newIdentifier) gen
  return $ sort events

-- | Run a simulation described by a configuration object but using a random
-- seed generated by the system rather than a seed
simulationWithSystemRandom :: (ModelParameters a) => SimulationConfiguration a b
                           -> (a -> Time -> (Time, [Event], b, Identifier) -> GenIO -> IO (Time, [Event], b, Identifier))
                           -> IO [Event]
simulationWithSystemRandom SimulationConfiguration {..} allEvents = do
  (_, events, _, _) <- withSystemRandom $ \g -> allEvents rates timeLimit (0, [], population, newIdentifier) g
  return $ sort events


-- | The number of lineages at the end of a simulation.
finalSize :: [Event] -- ^ The events from the simulation
          -> Integer
finalSize = foldl (\x y -> x + eventPopDelta y) 1



-- | Construct a timed list if possible.
asTimed :: [(Time,a)] -> Maybe (Timed a)
asTimed tas = if isAscending $ map fst tas then Just tas else Nothing

-- | Predicate to check if a list of orderable objects is in ascending order.
isAscending :: Ord a => [a] -> Bool
isAscending xs = case xs of
  [] -> True
  [_] -> True
  (x:y:xs') -> x <= y && isAscending (y:xs')

-- | Evaluate the timed object treating it as a cadlag function
cadlagValue :: Timed a -> Time -> Maybe a
cadlagValue = undefined

-- | Evaluate the timed object treating it as a direct delta function
diracDeltaValue :: Timed a -> Time -> Maybe a
diracDeltaValue = undefined

-- | Check if there is a pair at a particular time
hasTime :: Timed a -> Time -> Bool
hasTime = undefined

-- | Return the value of the next time if possible
nextTime :: Timed a -> Time -> Maybe Time
nextTime = undefined
