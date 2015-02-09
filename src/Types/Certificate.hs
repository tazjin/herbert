{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}

-- | This module contains types and instances specific to certificates
module Types.Certificate where

import           Control.Lens  hiding (Indexable, (.=))
import           Data.Aeson
import           Data.Data     (Data, Typeable)
import           Data.IxSet
import           Data.SafeCopy
import           Data.Text
import           Data.UUID     (UUID)
import           Types.Common
import           Web.Scotty    (Parsable (..))

-- * IxSet index types

newtype CertID = CertID { getCertID :: UUID }
    deriving (Eq, Ord, Data, Typeable, Show)

$(deriveSafeCopy 0 'base ''CertID)

instance Parsable CertID where
  parseParam = fmap CertID . parseParam

data Certificate = Certificate {
    _certId   :: CertID
  , _certBody :: Text
} deriving (Eq, Ord, Data, Typeable, Show)

makeLenses ''Certificate

$(deriveSafeCopy 0 'base ''Certificate)

instance Indexable Certificate where
     empty = ixSet [ ixFun $ \c -> [ c ^. certId ] ]

-- Aeson instances

instance ToJSON CertID where
    toJSON = toJSON . pack . show . getCertID

instance ToJSON Certificate where
    toJSON cert = object [ "id" .= (cert ^. certId)
                         , "cert" .= (cert ^. certBody) ]
