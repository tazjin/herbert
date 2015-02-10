{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE TypeFamilies              #-}
module Herbert.Storage.Test where

import           Control.Lens
import           Data.Acid
import           Data.Acid.Advanced
import           Data.Acid.Memory
import           Data.IxSet
import           Data.Time
import           Storage
import           System.Locale                  (defaultTimeLocale)
import           Test.Framework                 (Test (..), testGroup)
import           Test.Framework.Providers.HUnit
import           Test.HUnit                     hiding (Test)
import           Types.CA
import           Types.Certificate
import           Types.Common
import           Types.CSR

storageSuite :: Test
storageSuite = testGroup "Acid-state storage tests"
    [ testCase "Fetching CA should increase serial number" testIncSerialNumber
    , testCase "Rejecting CSR should set state to rejected" testRejectCSR
    , testCase "setSignedCSR should set state to signed" testSetSignedCSR ]

-- | Test something with an in-memory acid-state
acidTest :: forall event query result.
         (UpdateEvent event, MethodState event ~ HerbertState,
          QueryEvent query, MethodState query ~ HerbertState,
          MethodResult query ~ result)
         => String           -- ^ Test description
         -> HerbertState     -- ^ Initial state for test
         -> event            -- ^ Event to test
         -> query            -- ^ Query to check
         -> (result -> Bool) -- ^ Function to test query result
         -> Assertion
acidTest desc initialState event query' checker = do
    state <- openMemoryState initialState
    update state event
    result <- query state query'
    assertBool desc $ checker result


-- | States for testing

testCA :: CertificateAuthority
testCA = CA (SerialNumber 2) "BEGIN TEST CERTIFICATE"

testCSRID :: CSRID
testCSRID = CSRID $ read "db9fc4bc-3ef3-47d0-b1b8-8bcc80e443f9"

testCertID :: CertID
testCertID = CertID $ read "7883ba93-c5b5-44cf-87cf-42bf633bf64d"

testCSR :: CSR
testCSR = CSR
    testCSRID
    (CommonName "herbert-test.tazj.in")
    (OrganizationName "Acme Corp")
    (RequestingHost "127.0.0.1")
    (readTime defaultTimeLocale "%s" "1423603903")
    Pending
    "BEGIN TEST CSR"


testState :: HerbertState
testState = emptyState testCA

testIncSerialNumber :: Assertion
testIncSerialNumber =
    acidTest "getNextSerialNumber should increase serial in state"
             (emptyState testCA) GetNextSerialNumber RetrieveCA checker
  where
    checker ca = (ca ^. caSerialNumber & getSerialNumber) == 3

testRejectCSR :: Assertion
testRejectCSR =
    acidTest "rejectCSR should set status to rejected"
             testState' (RejectCSR testCSRID) (RetrieveCSR testCSRID) checker
  where
    testState' = testState & signingRequests .~ requests
    requests :: IxSet CSR
    requests = fromList [testCSR]
    checker (Just csr) = (csr ^. requestStatus) == Rejected

testSetSignedCSR :: Assertion
testSetSignedCSR =
    acidTest "setSignedCSR should set status to signed"
             testState' (SetSignedCSR testCSRID testCertID)
             (RetrieveCSR testCSRID) checker
  where
    testState' = testState & signingRequests .~ requests
    requests :: IxSet CSR
    requests = fromList [testCSR]
    checker (Just csr) = (csr ^. requestStatus) == (Signed testCertID)
