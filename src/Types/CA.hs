{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}

-- | This holds the certificate authority (without the private key)
module Types.CA where

import           Control.Lens
import           Data.Data     (Data, Typeable)
import           Data.SafeCopy
import           Data.Text     (Text)
import           Types.Common

data CertificateAuthority = CA {
    _caSerialNumber :: SerialNumber -- ^ Current CA serial number. Updated with every signing
  , _caCertificate  :: Text         -- ^ The CA certificate without the private key.
} deriving (Data, Typeable, Show)

makeLenses ''CertificateAuthority

$(deriveSafeCopy 0 'base ''CertificateAuthority)
