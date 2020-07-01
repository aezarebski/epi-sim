{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString.Builder as BBuilder
import qualified Data.ByteString.Lazy as L
import Data.Csv
import Data.Maybe (fromJust, isJust)
import Epidemic.Types.Population
import Epidemic.Types.Events
import qualified Epidemic.BDSCOD as BDSCOD
import qualified Epidemic.Utility as EpiUtil

main :: IO ()
main =
  let simConfig =
        BDSCOD.configuration
          1.0
          (2.5, 0.2, 0.15, [(3, 0.5), (4, 0.5)], 0.2, [(3.5, 0.5)])
   in if isJust simConfig
         then do events <- EpiUtil.simulation True (fromJust simConfig) BDSCOD.allEvents
                 let Just (newickBuilder,newickMetaData) = asNewickString (0, Person 1) =<< maybeEpidemicTree events
                 L.writeFile "demo-newick-string.txt" $ BBuilder.toLazyByteString newickBuilder
                 L.writeFile "demo-newick-metadata.csv" $ encode newickMetaData
                 -- L.writeFile "demo-output-observed-events.csv" . encode $
                   -- BDSCOD.observedEvents events
                 return ()
         else do putStrLn "Broken simulation configuration!!!"
                 return ()
