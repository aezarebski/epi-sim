{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Epidemic.Utility where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as Char8
import GHC.Generics (Generic)
import Data.Csv
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

instance ToField Person where
  toField (Person identifier) = toField identifier

instance ToRecord Event where
  toRecord e = case e of
    (InfectionEvent t p1 p2) -> record ["infection", toField t, toField p1, toField p2]
    (RemovalEvent t p1) -> record ["removal", toField t, toField p1, "NA"]
    (SamplingEvent t p1) -> record ["sample", toField t, toField p1, "NA"]
    (OccurrenceEvent t p1) -> record ["occurrence", toField t, toField p1, "NA"]


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
