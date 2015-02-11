module CSRUtils where

import           Control.Lens
import qualified Data.Text            as TS
import           Data.Text.Lazy
import           Data.Time
import           Data.UUID.V4
import           OpenSSL
import           OpenSSL.EVP.PKey
import           OpenSSL.PEM
import           OpenSSL.X509         hiding (getSerialNumber, getSubjectName)
import           OpenSSL.X509.Request as Rq
import           Types.CA
import           Types.Certificate
import           Types.CSR

-- testing
import           Data.Maybe           (fromJust)

type CSRBuilder = CSRID -> RequestingHost -> UTCTime -> CSRStatus -> CSR

parseCSR :: Text -> IO CSRBuilder
parseCSR csrBody = do
  parsed <- withOpenSSL $ readX509Req csrBodyString
  subjectName <- getSubjectName parsed True
  let maybeCsrBuilder = getCsrBuilder subjectName csrBodyString
  return $ fromJust maybeCsrBuilder
  where
    csrBodyString = unpack csrBody

getCsrBuilder :: [(String, String)] -- ^ Key value pair from certificate info
              -> String             -- ^ CSR body
              -> Maybe CSRBuilder
getCsrBuilder subjectName body = do
  cn <- lookup "commonName" subjectName
  on <- lookup "organizationName" subjectName
  Just (\i r t s -> CSR i (CommonName $ TS.pack cn)
                   (OrganizationName $ TS.pack on)
                   r t s body)

-- | This function is the "meat" at the moment. It does the required OpenSSL
--   calls to sign a certificate request. This function is very impure (for now)
signCSR :: CSR
        -> CertificateAuthority
        -> SomeKeyPair
        -> IO Certificate
signCSR csr ca key = withOpenSSL $ do
  -- Current time for validity period calculation
  now     <- getCurrentTime
  -- Parse CSR & CA again
  x509req <- readX509Req $ csr ^. requestBody
  caCert  <- readX509 $ TS.unpack $ ca ^. caCertificate
  x509crt <- makeX509FromReq x509req caCert
  -- Fill in empty values
  setSerialNumber x509crt $ getSerialNumber $ ca ^. caSerialNumber
  setNotBefore x509crt $ addUTCTime (-1) now
  setNotAfter x509crt $ addUTCTime (730 * 24 * 60 * 60) now
  -- Go!
  signX509 x509crt key Nothing
  certId <- fmap CertID nextRandom
  certPem <- writeX509 x509crt
  return $ Certificate certId $ TS.pack certPem
