{-# LANGUAGE TemplateHaskell #-}

-- | Provides common types and instances
module Types.Common where

import           Data.SafeCopy
import           Data.Text.Lazy (unpack)
import           Data.UUID
import           Web.Scotty     (Parsable (..))

-- UUIDs used for IDs in CRT / Certs need SafeCopy instance
$(deriveSafeCopy 0 'base ''UUID)

-- UUIDs need to be parsable from web routes
instance Parsable UUID where
  parseParam routeId =
    let maybeUUID = fromString $ unpack routeId
    in case maybeUUID of (Just id) -> Right id
                         Nothing   -> Left "ID not valid UUID"
