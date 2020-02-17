module Epidemic.Simulation where

import qualified Data.Vector as V
import System.Random.MWC
import System.Random.MWC.Distributions

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
