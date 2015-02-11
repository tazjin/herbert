{-# LANGUAGE TemplateHaskell #-}

-- | This is really the Main module.
module Herbert where

import           Config
import           Control.Applicative ((<$>), (<*>))
import           Control.Exception   (bracket)
import           Control.Lens
import           Data.Acid
import           Data.Acid.Local     (createCheckpointAndClose)
import qualified Data.Text.IO        as TIO
import           OpenSSL
import           OpenSSL.EVP.PKey
import           OpenSSL.PEM
import           Options
import           Server
import           Storage
import           System.Exit         (exitFailure)
import           Types.CA

data CLIOptions = CLIOptions {
    _configLocation  :: String
  , _initialCASerial :: Integer
} deriving (Show)

makeLenses ''CLIOptions

instance Options CLIOptions where
  defineOptions = CLIOptions
    <$> simpleOption "config" "/etc/herbert.conf"
        "Configuration file location. Default is /etc/herbert.conf"
    <*> simpleOption "serial" 0
        "Current CA serial number (if running for the first time)."

herbertMain :: IO ()
herbertMain = runCommand $ \options _ -> do
  maybeConfig <- loadConfig $ options ^. configLocation
  case maybeConfig of
      (Left err)   -> putStrLn ("Error reading config: " ++ err) >> exitFailure
      (Right conf) -> runHerbert conf $ SerialNumber (options ^. initialCASerial)

loadCAKeyPair :: Config -> IO SomeKeyPair
loadCAKeyPair config = do
    keyFile    <- readFile keyLoc
    withOpenSSL $ readPrivateKey keyFile keyPass
  where
    keyLoc  = config ^. caKey
    keyPass = PwStr (config ^. caPass)

loadState :: Config
          -> CertificateAuthority -- ^ CA used for first state instantiation
          -> IO AppState
loadState config = openLocalStateFrom (config ^. stateDir) . emptyState

runHerbert :: Config
           -> SerialNumber -- ^ CA serial number (only used on CA import)
           -> IO ()
runHerbert config caSerial = do
  key <- loadCAKeyPair config
  crt <- TIO.readFile (config ^. caCert)
  let ca = CA caSerial crt
  bracket (loadState config ca)
          createCheckpointAndClose
          (server config key)
