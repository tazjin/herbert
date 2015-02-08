module Main where

import           Config
import           Control.Exception (bracket)
import           Data.Acid
import           Data.Acid.Local   (createCheckpointAndClose)
import           Options
import           Server
import           Storage


main :: IO ()
main = runCommand $ \config _ ->
  bracket (openLocalStateFrom (stateDir config) emptyState)
          createCheckpointAndClose
          (\acid -> server acid config)

  --server undefined (Config 1212)
