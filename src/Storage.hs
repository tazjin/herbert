{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeFamilies       #-}

-- | This module contains types and instances for storing the CA state.
module Storage where

import           Control.Lens         hiding (Indexable)
import           Control.Monad.Reader (ask)
import           Control.Monad.State  (get, put)
import           Data.Acid
import           Data.Data            (Data, Typeable)
import           Data.IxSet           as IxSet
import           Data.SafeCopy
import           Data.Time            (UTCTime)
import           Types.CA
import           Types.Certificate
import           Types.CSR

-- | State of the application to store in acid-state
data HerbertState = HerbertState {
    _signingRequests :: IxSet CSR
  , _certificates    :: IxSet Certificate
  , _certAuthority   :: CertificateAuthority
} deriving (Data, Typeable)

makeLenses ''HerbertState
$(deriveSafeCopy 0 'base ''HerbertState)

emptyState :: CertificateAuthority -> HerbertState
emptyState ca = HerbertState { _signingRequests = empty
                             , _certificates = empty
                             , _certAuthority = ca }

-- Type synonym to shorten some things
type AppState = AcidState HerbertState

-- * Acid state functions

-- Updates

insertCSR :: CSR -> Update HerbertState CSR
insertCSR csr = do
    state <- get
    put $ state & signingRequests %~ (IxSet.insert csr)
    return csr

insertCertificate :: Certificate -> Update HerbertState Certificate
insertCertificate cert = do
    state <- get
    put $ state & certificates %~ (IxSet.insert cert)
    return cert

changeCSRStatus :: CSRID -> CSRStatus -> Update HerbertState (Maybe CSR)
changeCSRStatus csrId newStatus = do
    state <- get
    let requests = state ^. signingRequests
        maybeCsr = do
          oldCsr <- getOne $ requests @= csrId
          return $ (oldCsr & requestStatus .~ newStatus)
    case maybeCsr of
      Nothing    -> return Nothing
      (Just csr) -> do
        put $ state & signingRequests .~ (updateIx csrId csr requests)
        return maybeCsr

rejectCSR :: CSRID -> Update HerbertState (Maybe CSR)
rejectCSR csrId = changeCSRStatus csrId Rejected

setSignedCSR :: CSRID -> CertID -> Update HerbertState (Maybe CSR)
setSignedCSR csrId certId = changeCSRStatus csrId $ Signed certId

-- | Returns the CA with the next serial number.
getNextSerialNumber :: Update HerbertState CertificateAuthority
getNextSerialNumber = do
     state <- get
     let newSerial = (+) 1 $ (state ^. certAuthority) ^. caSerialNumber
         newState  = state & (certAuthority . caSerialNumber) .~ newSerial
     put newState
     return $ newState ^. certAuthority

-- Queries

retrieveCSR :: CSRID -> Query HerbertState (Maybe CSR)
retrieveCSR csrId =
  do state <- ask
     return $ getOne $ (state ^. signingRequests) @= csrId

retrieveCert :: CertID -> Query HerbertState (Maybe Certificate)
retrieveCert certId =
  ask >>= \state -> return $ getOne $ (state ^. certificates) @= certId

retrieveCA :: Query HerbertState CertificateAuthority
retrieveCA = ask >>= \state -> return $ state ^. certAuthority

-- | List all CSRs
listCSR :: Query HerbertState [CSR]
listCSR = do
    state <- ask
    return $ IxSet.toDescList (Proxy :: Proxy UTCTime) $ state ^. signingRequests

-- | List CSRs filtered by state
listCSRByStatus :: CSRStatus -> Query HerbertState [CSR]
listCSRByStatus status = do
    state <- ask
    let requests = (state ^. signingRequests) @= status
    return $ IxSet.toDescList (Proxy :: Proxy UTCTime) requests

$(makeAcidic ''HerbertState
  [ 'insertCSR
  , 'insertCertificate
  , 'rejectCSR
  , 'setSignedCSR
  , 'getNextSerialNumber
  , 'retrieveCSR
  , 'retrieveCert
  , 'retrieveCA
  , 'listCSR
  , 'listCSRByStatus ])
