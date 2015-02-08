module Main where

import           Config
import           Server
import           Storage

main :: IO ()
main = server undefined (Config 1212)
