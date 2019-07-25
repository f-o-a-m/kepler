module Network.ABCI.Middleware.RequestLogger
    ( logStdout
    ) where

import Katip
import Data.Aeson (genericToJSON, ToJSON(..), FromJSON(..),genericParseJSON)
import Data.Aeson.Casing (aesonPrefix, camelCase)


import Network.ABCI.Types.Messages.FieldTypes
import Network.ABCI.Types.App (Middleware)

-- | Production request logger middleware for ABCI requests
logStdout :: Middleware m
logStdout = undefined
  -- runKatipContextT


-- | Orphans

instance ToJSON Timestamp
instance FromJSON Timestamp

instance ToJSON BlockParams where
  toJSON = genericToJSON $ aesonPrefix camelCase
instance FromJSON BlockParams where
  parseJSON = genericParseJSON $ aesonPrefix camelCase
instance ToObject BlockParams
instance LogItem BlockParams where
  payloadKeys _ _ = AllKeys

instance ToJSON EvidenceParams where
  toJSON = genericToJSON $ aesonPrefix camelCase
instance FromJSON EvidenceParams where
  parseJSON = genericParseJSON $ aesonPrefix camelCase
instance ToObject EvidenceParams
instance LogItem EvidenceParams where
  payloadKeys _ _ = AllKeys

instance ToJSON ValidatorParams where
  toJSON = genericToJSON $ aesonPrefix camelCase
instance FromJSON ValidatorParams where
  parseJSON = genericParseJSON $ aesonPrefix camelCase
instance ToObject ValidatorParams
instance LogItem ValidatorParams where
  payloadKeys _ _ = AllKeys

instance ToJSON ConsensusParams where
  toJSON = genericToJSON $ aesonPrefix camelCase
instance FromJSON ConsensusParams where
  parseJSON = genericParseJSON $ aesonPrefix camelCase
instance ToObject ConsensusParams
instance LogItem ConsensusParams where
  payloadKeys _ _ = AllKeys

instance ToJSON PubKey where
  toJSON = genericToJSON $ aesonPrefix camelCase
instance FromJSON PubKey where
  parseJSON = genericParseJSON $ aesonPrefix camelCase
instance ToObject PubKey
instance LogItem PubKey where
  payloadKeys _ _ = AllKeys

instance ToJSON ValidatorUpdate where
  toJSON = genericToJSON $ aesonPrefix camelCase
instance FromJSON ValidatorUpdate where
  parseJSON = genericParseJSON $ aesonPrefix camelCase
instance ToObject ValidatorUpdate
instance LogItem ValidatorUpdate where
  payloadKeys _ _ = AllKeys
