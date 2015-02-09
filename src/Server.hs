module Server where

import           Config
import           Control.Lens
import           Control.Monad.IO.Class    (liftIO)
import           CSRUtils
import           Data.Acid
import           Data.Acid.Advanced        (query', update')
import           Data.Monoid               (mconcat)
import           Data.Text                 (pack)
import qualified Data.Text.Lazy            as LT
import           Data.Time
import           Data.UUID.V4
import           Network.HTTP.Types.Status (notFound404)
import           Network.Wai               (remoteHost)
import           Network.Wai.Logger        (showSockAddr)
import           Storage
import           Types.Certificate
import           Types.CSR
import           Web.Scotty

server :: Config -> AppState -> IO ()
server config state = scotty scottyPort $ do
  post "/csr"               $ handlePostCSR state
  get  "/csr/all"           $ handleListRequests state
  get  "/csr/pending"       $ handleListByStatus state Pending
  get  "/csr/rejected"      $ handleListByStatus state Rejected
  get  "/csr/reject/:csrid" $ handleRejectCSR state
  get  "/csr/:csrid"        $ handlePollCSRState state
  get  "/cert/:certid"      $ handleGetCertificate state
  where
    scottyPort = config ^. port

-- Posting CSRs
handlePostCSR :: AppState -> ActionM ()
handlePostCSR state = do
  now        <- liftIO getCurrentTime
  csrId      <- liftIO (fmap CSRID nextRandom)
  csrBody    <- param "csr"
  csrBuilder <- liftIO (parseCSR csrBody)
  clientIP   <- fmap (showSockAddr . remoteHost) request
  let csr = csrBuilder csrId (RequestingHost $ pack clientIP) now Pending
  storeCSR state csr
  text $ LT.pack $ show $ getCSRID csrId

storeCSR :: AppState -> CSR -> ActionM CSR
storeCSR state csr = update' state $ InsertCSR csr

-- Polling CSRs
handlePollCSRState :: AppState -> ActionM ()
handlePollCSRState state = do
  csrId    <- param "csrid"
  maybeCsr <- query' state $ RetrieveCSR csrId
  case maybeCsr of
    Nothing -> status notFound404
    (Just csr) -> text $ LT.pack $ show $ csr ^. requestStatus

-- Retrieving certificates
handleGetCertificate :: AppState -> ActionM ()
handleGetCertificate state = do
  certId    <- param "certid"
  maybeCert <- query' state $ RetrieveCert certId
  case maybeCert of
    Nothing    -> status notFound404
    (Just crt) -> json crt

-- * Administration functions

handleListRequests :: AppState -> ActionM ()
handleListRequests state = do
  csrList <- query' state ListCSR
  json csrList

handleListByStatus :: AppState -> CSRStatus -> ActionM ()
handleListByStatus state status = do
  csrList <- query' state $ ListCSRByStatus status
  json csrList

handleRejectCSR :: AppState -> ActionM ()
handleRejectCSR state = do
  csrId <- param "csrid"
  updatedCsr <- update' state $ RejectCSR csrId
  case updatedCsr of
    Nothing -> status notFound404
    (Just csr) -> json csr
