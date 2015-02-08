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
import           Data.Text            (Text)
import qualified Data.Text.Lazy       as LT
import           Data.Time            (UTCTime)
import           Data.UUID            (UUID, fromString)
import           Web.Scotty           (Parsable (..))

-- * IxSet index types
newtype CSRID = CSRID { getCSRID :: UUID }
    deriving (Eq, Ord, Data, Typeable, Show)

$(deriveSafeCopy 0 'base ''UUID)
$(deriveSafeCopy 0 'base ''CSRID)

-- CSR IDs need to be parsable from route captures
instance Parsable CSRID where
  parseParam routeId =
    let maybeUUID = fromString $ LT.unpack routeId
    in case maybeUUID of (Just id) -> Right $ CSRID id
                         Nothing   -> Left "ID not valid UUID"

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

data CSRState = Pending
              | Rejected
              | Signed UUID
  deriving (Eq, Ord, Data, Typeable, Show)

$(deriveSafeCopy 0 'base ''CSRState)

-- | Certificate signing request, including information about the requester.
data CSR = CSR {
    _requestId        :: CSRID -- ^ UUID that identifies this CSR for Herbert
  , _commonName       :: CommonName
  , _organizationName :: OrganizationName
  , _requestingHost   :: RequestingHost -- ^ Not initially part of the CSR, added by Herbert
  , _requestDate      :: UTCTime
  , _requestState     :: CSRState
  , _requestBody      :: String -- ^ Consumed from String type by OpenSSL
} deriving (Eq, Ord, Data, Typeable, Show)

makeLenses ''CSR
$(deriveSafeCopy 0 'base ''CSR)

instance Indexable CSR where
     empty = ixSet [ ixFun $ \e -> [ e ^. commonName ]
                   , ixFun $ \e -> [ e ^. requestId ]
                   , ixFun $ \e -> [ e ^. requestingHost ]]

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

-- | Acid state functions

insertCSR :: CSR -> Update HerbertState CSR
insertCSR csr =
  do hs <- get
     put $ hs & signingRequests %~ (IxSet.insert csr)
     return csr

retrieveCSR :: CSRID -> Query HerbertState (Maybe CSR)
retrieveCSR csrId =
  do state <- ask
     return $ getOne $ (state ^. signingRequests) @= csrId

$(makeAcidic ''HerbertState
  [ 'insertCSR
  , 'retrieveCSR ])
