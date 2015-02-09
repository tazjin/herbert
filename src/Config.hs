{-# LANGUAGE TemplateHaskell #-}

-- | Various settings for Herbert
module Config where

import           Control.Applicative (pure, (<$>), (<*>))
import           Control.Lens        hiding ((.=))
import qualified Data.ByteString     as BS
import           Data.Yaml
import           Options

data Config = Config {
  _port     :: Int,
  _stateDir :: String,
  _caCert   :: FilePath,
  _caKey    :: FilePath
} deriving (Show)

makeLenses ''Config

instance ToJSON Config where
    toJSON config = object [ "port"     .= (config ^. port)
                           , "stateDir" .= (config ^. stateDir)
                           , "caCert"   .= (config ^. caCert)
                           , "caKey"    .= (config ^. caKey) ]

instance FromJSON Config where
    parseJSON (Object v) =
      Config <$> v .: "port"
             <*> v .: "stateDir"
             <*> v .: "caCert"
             <*> v .: "caKey"

loadConfig :: FilePath -> IO (Either String Config)
loadConfig = fmap decodeEither . BS.readFile
