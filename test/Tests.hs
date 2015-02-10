module Main where

import           Herbert.Storage.Test
import           Test.Framework       (defaultMain)

main :: IO ()
main = defaultMain [storageSuite]
