module CSRUtils where

--import           OpenSSL.X509
import qualified Data.Text            as TS
import           Data.Text.Lazy
import           Data.Time            (UTCTime)
import           OpenSSL.PEM
import           OpenSSL.X509.Request
import           Storage

-- testing
import           Data.Maybe           (fromJust)

type CSRBuilder =  RequestingHost -> UTCTime -> CSR

parseCSR :: Text -> IO CSRBuilder
parseCSR csrBody = do
  parsed <- readX509Req csrBodyString
  subjectName <- getSubjectName parsed True
  let maybeCsrBuilder = getCsrBuilder subjectName csrBodyString
  return $ fromJust maybeCsrBuilder
  where
    csrBodyString = unpack csrBody

getCsrBuilder :: [(String, String)] -> String -> Maybe CSRBuilder
getCsrBuilder subjectName body = do
  cn <- lookup "commonName" subjectName
  on <- lookup "organizationName" subjectName
  Just (\r t -> CSR (CommonName $ TS.pack cn)
                (OrganizationName $ TS.pack on)
                r t body)
