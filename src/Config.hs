{-# LANGUAGE TemplateHaskell #-}

-- | Various settings for Herbert
module Config where

import           Control.Lens

data Config = Config {
  _port :: Int
}

makeLenses ''Config
