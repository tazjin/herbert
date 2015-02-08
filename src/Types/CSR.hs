{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE GeneralizedNewtypeDeriving    #-}

-- | This module contains types and instances specific to CSRs
module Types.CSR where

import Data.Aeson
import           Data.UUID            (UUID, fromString)
import           Data.Data            (Data, Typeable)
import           Data.SafeCopy
import           Web.Scotty           (Parsable (..))
import qualified Data.Text.Lazy       as LT
import           Data.Text            (Text, pack)
import           Data.Time            (UTCTime)
import           Control.Lens         hiding (Indexable, (.=))
import           Data.IxSet           as IxSet

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
    deriving (Eq, Ord, Data, Typeable, Show, ToJSON)

$(deriveSafeCopy 0 'base ''CommonName)

newtype OrganizationName = OrganizationName Text
    deriving (Eq, Ord, Data, Typeable, Show)

$(deriveSafeCopy 0 'base ''OrganizationName)

newtype RequestingHost = RequestingHost Text
    deriving (Eq, Ord, Data, Typeable, Show, ToJSON)

$(deriveSafeCopy 0 'base ''RequestingHost)

-- * Acid-state types

data CSRStatus = Pending
               | Rejected
               | Signed UUID
  deriving (Eq, Ord, Data, Typeable, Show)

$(deriveSafeCopy 0 'base ''CSRStatus)

-- | Certificate signing request, including information about the requester.
data CSR = CSR {
    _requestId        :: CSRID -- ^ UUID that identifies this CSR for Herbert
  , _commonName       :: CommonName
  , _organizationName :: OrganizationName
  , _requestingHost   :: RequestingHost -- ^ Time that Herbert received CSR
  , _requestDate      :: UTCTime
  , _requestStatus    :: CSRStatus
  , _requestBody      :: String -- ^ Consumed from String type by OpenSSL
} deriving (Eq, Ord, Data, Typeable, Show)

makeLenses ''CSR
$(deriveSafeCopy 0 'base ''CSR)

instance Indexable CSR where
    empty = ixSet [ ixFun $ \e -> [ e ^. commonName ]
                  , ixFun $ \e -> [ e ^. requestId ]
                  , ixFun $ \e -> [ e ^. requestStatus ]
                  , ixFun $ \e -> [ e ^. requestingHost ]
                  , ixFun $ \e -> [ e ^. requestDate ]]

-- Aeson instances

instance ToJSON CSRID where
    toJSON = toJSON . pack . show . getCSRID

instance ToJSON CSRStatus where
    toJSON = toJSON . pack . show

instance ToJSON CSR where
    toJSON csr = object [ "id" .= (csr ^. requestId)
                        , "commonName" .= (csr ^. commonName)
                        , "requestingHost" .= (csr ^. requestingHost)
                        , "requestDate" .= (csr ^. requestDate)
                        , "requestStatus" .= (csr ^. requestStatus) ]