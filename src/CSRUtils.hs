module CSRUtils where

import qualified Data.Text            as TS
import           Data.Text.Lazy
import           Data.Time            (UTCTime)
import           OpenSSL.PEM
import           OpenSSL.X509.Request
import           Types.CSR

-- testing
import           Data.Maybe           (fromJust)

type CSRBuilder = CSRID -> RequestingHost -> UTCTime -> CSRState -> CSR

parseCSR :: Text -> IO CSRBuilder
parseCSR csrBody = do
  parsed <- readX509Req csrBodyString
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
