{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeFamilies       #-}

-- | This module contains types and instances for storing the CA state.
module Storage where

import           Control.Lens        hiding (Indexable)
import           Control.Monad.State (get, put)
import           Data.Acid
import           Data.Data           (Data, Typeable)
import           Data.IxSet          as IxSet
import           Data.SafeCopy
import           Data.Text           (Text)
import           Data.Time           (UTCTime)

-- * IxSet index types
newtype CommonName = CommonName Text
    deriving (Eq, Ord, Data, Typeable, Show)

$(deriveSafeCopy 0 'base ''CommonName)

newtype OrganizationName = OrganizationName Text
    deriving (Eq, Ord, Data, Typeable, Show)

$(deriveSafeCopy 0 'base ''OrganizationName)

newtype RequestingHost = RequestingHost Text
    deriving (Eq, Ord, Data, Typeable, Show)

$(deriveSafeCopy 0 'base ''RequestingHost)

-- * Acid-state types

-- | Certificate signing request, including information about the requester.
data CSR = CSR {
    _commonName       :: CommonName
  , _organizationName :: OrganizationName
  , _requestingHost   :: RequestingHost -- ^ Not initially part of the CSR, added by Herbert
  , _requestDate      :: UTCTime
  , _requestBody      :: String -- ^ Consumed from String type by OpenSSL
} deriving (Eq, Ord, Data, Typeable, Show)

makeLenses ''CSR
$(deriveSafeCopy 0 'base ''CSR)

instance Indexable CSR where
     empty = ixSet [ ixFun $ \e -> [ e ^. commonName ]
                   , ixFun $ \e -> [ e ^. organizationName ]
                   , ixFun $ \e -> [ e ^. requestingHost ]]

-- | State of the application to store in acid-state
data HerbertState = HerbertState {
  _signingRequests :: IxSet CSR
} deriving (Data, Typeable)

makeLenses ''HerbertState
$(deriveSafeCopy 0 'base ''HerbertState)

emptyState :: HerbertState
emptyState = HerbertState { _signingRequests = empty }

-- | Acid state functions

insertCSR :: CSR -> Update HerbertState CSR
insertCSR csr =
  do hs <- get
     put $ hs & signingRequests %~ (IxSet.insert csr)
     return csr

$(makeAcidic ''HerbertState
  [ 'insertCSR ])
