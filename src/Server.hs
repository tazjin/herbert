module Server where

import           Config
import           Control.Lens
import           Control.Monad.IO.Class    (liftIO)
import           CSRUtils
import           Data.Acid
import           Data.Acid.Advanced        (query', update')
import           Data.Monoid               (mconcat)
import           Data.Text                 (Text, pack)
import qualified Data.Text.Lazy            as LT
import           Data.Time
import           Data.UUID.V4
import           Network.HTTP.Types.Status
import           Network.Wai               (remoteHost)
import           Network.Wai.Logger        (showSockAddr)
import           OpenSSL.EVP.PKey
import           Storage
import           Types.Certificate
import           Types.CSR
import           Web.Scotty

server :: Config -> SomeKeyPair -> AppState -> IO ()
server config key state = scotty scottyPort $ do
  clientRoutes state
  adminRoutes state key
  where
    scottyPort = config ^. port

-- | Routes accessible by all clients under /
clientRoutes :: AppState -> ScottyM ()
clientRoutes state = do
  post "/client/csr"          $ handlePostCSR state
  get  "/client/csr/:csrid"   $ handlePollCSRState state
  get  "/client/cert/:certid" $ handleGetCertificate state

-- | Routes for administrators accessible under /admin
--   These routes should be protected with client-certificate checks. Refer to
--   the provided nginx configuration for an example.
adminRoutes :: AppState -> SomeKeyPair -> ScottyM ()
adminRoutes state key = do
  get  "/admin/csr/all"           $ handleListRequests state
  get  "/admin/csr/pending"       $ handleListByStatus state Pending
  get  "/admin/csr/rejected"      $ handleListByStatus state Rejected
  get  "/admin/csr/reject/:csrid" $ handleRejectCSR state
  get  "/admin/csr/sign/:csrid"   $ handleSignCertificate state key

-- Error helper
serveError :: Status -> Text -> ActionM ()
serveError s t = status s >> json t

-- Posting CSRs
handlePostCSR :: AppState -> ActionM ()
handlePostCSR state = do
  now             <- liftIO getCurrentTime
  csrId           <- liftIO (fmap CSRID nextRandom)
  csrBody         <- param "csr"
  maybeCsrBuilder <- liftIO (parseCSR csrBody)
  clientIP        <- fmap (showSockAddr . remoteHost) request
  case maybeCsrBuilder of
    Nothing        -> serveError badRequest400 "Could not parse CSR"
    (Just builder) -> do
      let csr = builder csrId (RequestingHost $ pack clientIP) now Pending
      storeCSR state csr
      status created201
      json csr

storeCSR :: AppState -> CSR -> ActionM CSR
storeCSR state csr = update' state $ InsertCSR csr

-- Polling CSR status
handlePollCSRState :: AppState -> ActionM ()
handlePollCSRState state = do
  csrId    <- param "csrid"
  maybeCsr <- query' state $ RetrieveCSR csrId
  case maybeCsr of
    Nothing -> serveError notFound404 "Could not find CSR"
    (Just csr) -> json $ csr ^. requestStatus

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
    Nothing -> serveError notFound404 "Could not find CSR"
    (Just csr) -> json csr

-- Returns a reason if signing is not possible, and Nothing otherwise.
checkSignable :: CSR -> Maybe Text
checkSignable csr =
  case csr ^. requestStatus of
    Rejected   -> Just "This CSR is rejected and can not be signed"
    Signed csr -> Just "This CSR is already signed"
    Pending    -> Nothing

signAndRespond :: AppState -> CSR -> SomeKeyPair -> ActionM ()
signAndRespond state csr key = do
  ca   <- update' state GetNextSerialNumber
  cert <- liftIO $ signCSR csr ca key
  update' state $ SetSignedCSR (csr ^. requestId) $ cert ^. certId
  update' state $ InsertCertificate cert
  json cert

handleSignCertificate :: AppState -> SomeKeyPair -> ActionM ()
handleSignCertificate state key = do
  csrId    <- param "csrid"
  maybeCsr <- query' state $ RetrieveCSR csrId
  case maybeCsr of
    Nothing    -> serveError notFound404 "Could not find CSR"
    (Just csr) ->
      maybe (signAndRespond state csr key) (serveError conflict409)
            (checkSignable csr)
