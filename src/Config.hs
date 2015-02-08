-- | Various settings for Herbert
module Config where

import           Control.Applicative (pure, (<$>), (<*>))
import           Options

data Config = Config {
  port     :: Int,
  stateDir :: String
}

instance Options Config where
  defineOptions = pure Config
    <*> simpleOption "port" 1212
        "Port to run on. Default is 1212"
    <*> simpleOption "statedir" "/tmp/herbert"
        "Directory to keep state files"
