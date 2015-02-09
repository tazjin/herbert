{-# LANGUAGE TemplateHaskell #-}
module Main where

import           Config
import           Control.Applicative ((<$>))
import           Control.Exception   (bracket)
import           Control.Lens
import           Data.Acid
import           Data.Acid.Local     (createCheckpointAndClose)
import           Options
import           Server
import           Storage
import           System.Exit         (exitFailure)

data CLIOptions = CLIOptions {
  _configLocation :: String
} deriving (Show)

makeLenses ''CLIOptions

instance Options CLIOptions where
  defineOptions = CLIOptions
    <$> simpleOption "config" "/etc/herbert.conf"
        "Configuration file location. Default is /etc/herbert.conf"

main :: IO ()
main = runCommand $ \options _ -> do
  maybeConfig <- loadConfig $ options ^. configLocation
  case maybeConfig of
      (Left err)   -> putStrLn ("Error reading config: " ++ err) >> exitFailure
      (Right conf) -> runHerbert conf

loadState :: Config -> IO AppState
loadState config = openLocalStateFrom (config ^. stateDir) emptyState

runHerbert :: Config -> IO ()
runHerbert config =
  bracket (loadState config)
          createCheckpointAndClose
          (\acid -> server acid config)
