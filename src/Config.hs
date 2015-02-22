{-# LANGUAGE TemplateHaskell #-}

-- | Various settings for Herbert
module Config where

import           Control.Applicative (pure, (<$>), (<*>))
import           Control.Lens        hiding ((.=))
import qualified Data.ByteString     as BS
import           Data.Text           (Text)
import           Data.Yaml
import           Options

type StateDirectory = FilePath
type CACertificate = FilePath
type CAKey = FilePath
type CAPassword = String
type Sender = Text
type Recipient = Text

data Config = Config {
  _port      :: Int,
  _stateDir  :: StateDirectory,
  _caCert    :: CACertificate,
  _caKey     :: CAKey,
  _caPass    :: CAPassword,
  _notifyCSR :: Bool,
  _sender    :: Sender, -- ^ Mail address to send from
  _recipient :: Recipient -- ^ Mail address to send to
} deriving (Show)

makeLenses ''Config

instance ToJSON Config where
    toJSON config = object [ "port"      .= (config ^. port)
                           , "stateDir"  .= (config ^. stateDir)
                           , "caCert"    .= (config ^. caCert)
                           , "caKey"     .= (config ^. caKey)
                           , "caPass"    .= (config ^. caPass)
                           , "notifyCSR" .= (config ^. notifyCSR)
                           , "sender"    .= (config ^. sender)
                           , "recipient" .= (config ^. recipient)]

instance FromJSON Config where
    parseJSON (Object v) =
      Config <$> v .: "port"
             <*> v .: "stateDir"
             <*> v .: "caCert"
             <*> v .: "caKey"
             <*> v .: "caPass"
             <*> v .: "notifyCSR"
             <*> v .: "sender"
             <*> v .: "recipient"

loadConfig :: FilePath -> IO (Either String Config)
loadConfig = fmap decodeEither . BS.readFile
