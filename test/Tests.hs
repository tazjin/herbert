module Main where

import           Herbert.Storage.Test
import           Test.Tasty

main :: IO ()
main = defaultMain $ testGroup "Herbert tests" [storageSuite]
