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
import           Types.CSR

-- | State of the application to store in acid-state
data HerbertState = HerbertState {
  _signingRequests :: IxSet CSR
} deriving (Data, Typeable)

makeLenses ''HerbertState
$(deriveSafeCopy 0 'base ''HerbertState)

emptyState :: HerbertState
emptyState = HerbertState { _signingRequests = empty }

-- Type synonym to shorten some things
type AppState = AcidState HerbertState

-- * Acid state functions

-- Updates

insertCSR :: CSR -> Update HerbertState CSR
insertCSR csr =
  do hs <- get
     put $ hs & signingRequests %~ (IxSet.insert csr)
     return csr

-- Queries

retrieveCSR :: CSRID -> Query HerbertState (Maybe CSR)
retrieveCSR csrId =
  do state <- ask
     return $ getOne $ (state ^. signingRequests) @= csrId

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
  , 'retrieveCSR
  , 'listCSR
  , 'listCSRByStatus ])
