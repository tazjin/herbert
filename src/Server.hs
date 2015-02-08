module Server where

import           Config
import           Control.Lens
import           Control.Monad.IO.Class (liftIO)
import           CSRUtils
import           Data.Acid
import           Data.Acid.Advanced     (query', update')
import           Data.Monoid            (mconcat)
import           Data.Text              (pack)
import           Data.Time
import           Data.UUID.V4
import           Network.Wai            (remoteHost)
import           Network.Wai.Logger     (showSockAddr)
import           Storage
import           Web.Scotty

-- testing
import qualified Data.Text.Lazy         as LT

server :: AcidState HerbertState -> Config -> IO ()
server state config = scotty scottyPort $ do
  post "/csr" $ handlePostCSR state
  where
    scottyPort = port config

handlePostCSR :: AcidState HerbertState -> ActionM ()
handlePostCSR state = do
  now <- liftIO getCurrentTime
  csrId <- liftIO (fmap CSRID nextRandom)
  csrBody <- param "csr"
  csrBuilder <- liftIO (parseCSR csrBody)
  clientIP <- fmap (showSockAddr . remoteHost) request
  let csr = csrBuilder csrId (RequestingHost $ pack clientIP) now
  storeCSR state csr
  text $ LT.pack $ show $ getCSRID csrId

storeCSR :: AcidState HerbertState -> CSR -> ActionM CSR
storeCSR state csr = update' state $ InsertCSR csr
