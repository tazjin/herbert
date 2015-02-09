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
import           Types.Certificate
import           Types.CSR

-- | State of the application to store in acid-state
data HerbertState = HerbertState {
    _signingRequests :: IxSet CSR
  , _certificates    :: IxSet Certificate
} deriving (Data, Typeable)

makeLenses ''HerbertState
$(deriveSafeCopy 0 'base ''HerbertState)

emptyState :: HerbertState
emptyState = HerbertState { _signingRequests = empty, _certificates = empty }

-- Type synonym to shorten some things
type AppState = AcidState HerbertState

-- * Acid state functions

-- Updates

insertCSR :: CSR -> Update HerbertState CSR
insertCSR csr =
  do hs <- get
     put $ hs & signingRequests %~ (IxSet.insert csr)
     return csr

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

-- Queries

retrieveCSR :: CSRID -> Query HerbertState (Maybe CSR)
retrieveCSR csrId =
  do state <- ask
     return $ getOne $ (state ^. signingRequests) @= csrId

retrieveCert :: CertID -> Query HerbertState (Maybe Certificate)
retrieveCert certId =
  ask >>= \state -> return $ getOne $ (state ^. certificates) @= certId

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
  , 'rejectCSR
  , 'retrieveCSR
  , 'retrieveCert
  , 'listCSR
  , 'listCSRByStatus ])
