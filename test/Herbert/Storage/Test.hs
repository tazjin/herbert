{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE TypeFamilies              #-}
module Herbert.Storage.Test where

import           Control.Lens
import           Data.Acid
import           Data.Acid.Advanced
import           Data.Acid.Memory
import           Storage
import           Test.Framework                 (Test (..), testGroup)
import           Test.Framework.Providers.HUnit
import           Test.HUnit                     hiding (Test)
import           Types.CA
import           Types.Common

storageSuite :: Test
storageSuite = testGroup "Acid-state storage tests"
    [testCase "Fetching CA should increase serial number" testIncSerialNumber]

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

testCA :: CertificateAuthority
testCA = CA (SerialNumber 2) "BEGIN TEST CERTIFICATE"

testIncSerialNumber :: Assertion
testIncSerialNumber =
    acidTest "getNextSerialNumber should increase serial in state"
             (emptyState testCA) GetNextSerialNumber RetrieveCA checker
  where
    checker :: CertificateAuthority -> Bool
    checker ca = (ca ^. caSerialNumber & getSerialNumber) == 3
