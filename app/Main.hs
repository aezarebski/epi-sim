{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad
import Data.Csv
import Data.Maybe (fromJust)
import qualified Data.ByteString.Lazy as L
import Epidemic
import Epidemic.Utility
import qualified Epidemic.BirthDeath as Model

mean xs = fromIntegral (sum xs) / (fromIntegral $ length xs)

meanFinalSize = exp ((2.1 - 0.2) * 1.5)

randomBDEvents =
  simulationWithSystemRandom
    (fromJust $ Model.configuration 1.5 (2.1, 0.2))
    Model.allEvents


withinNPercent n x y = x - d < y && y < x + d where d = n * x / 100

-- main :: IO ()
main =
  let numRepeats = 1000
   in do finalSizes <- replicateM numRepeats (finalSize <$> randomBDEvents)
         putStrLn $
           if withinNPercent 5 (mean finalSizes) meanFinalSize
             then "Correct!"
             else "Fail :("
