-- | Simple module that provides a function to notify for emails
module Notify (notifyForCSR) where

import           Config
import           Control.Lens
import           Control.Monad.IO.Class
import           Data.Text
import           Data.Text.Lazy          (fromStrict)
import           Network.Mail.Mime
import           Network.Mail.SMTP
import           Network.Mail.SMTP.Types
import           Types.CSR

-- | Fire and forget notify function. We don't care about status codes
--   from the SMTP connection at the moment because notifications aren't
--   critical.
notifyForCSR :: MonadIO m
             => Config
             -> (CSR -> m ())
notifyForCSR config csr =
  case (config ^. notifyCSR) of
    True  -> liftIO $ sendMail "localhost" mail
    False -> return ()
  where
    senderAddr = Address (Just "Herbert Signing Server") (config ^. sender)
    receiverAddr = [Address Nothing (config ^. recipient)]
    headers  = [("Subject", "Herbert received CSR")]
    body     = plainPart $ fromStrict $ makeMailBody csr
    mail     = (emptyMail senderAddr) {
      mailTo = receiverAddr,
      mailHeaders = headers,
      mailParts = [[body]]
    }

makeMailBody :: CSR -> Text
makeMailBody csr =
  intercalate (singleton '\n') $ [
    "Herbert has received a new certificate signing request", "",
    append "CSR ID: " (pack $ show $ getCSRID $ csr ^. requestId),
    append "Client IP: " (getHost $ csr ^. requestingHost),
    append "CSR Subject: " (getCN $ csr ^. commonName),
    append "Received at: " (pack $ show $ csr ^. requestDate)
    ]

