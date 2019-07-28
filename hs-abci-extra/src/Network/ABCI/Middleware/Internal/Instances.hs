
module Network.ABCI.Middleware.Internal.Instances where

import           Data.Aeson                             (FromJSON (..),
                                                         ToJSON (..),
                                                         genericParseJSON,
                                                         genericToJSON,
                                                         withText)
import           Data.Aeson.Casing                      (aesonDrop, aesonPrefix,
                                                         camelCase)
import           Data.ByteArray.Encoding                (Base (..),
                                                         convertFromBase,
                                                         convertToBase)
import           Data.ByteString                        (ByteString)
import           Data.Either                            (either)
import           Data.Text.Encoding                     (decodeUtf8, encodeUtf8)
import           Katip


import           Network.ABCI.Types.Messages.FieldTypes
import qualified Network.ABCI.Types.Messages.Request    as Req
import qualified Network.ABCI.Types.Messages.Response   as Res


-- | ByteString
instance ToJSON ByteString where
  toJSON = toJSON . decodeUtf8 . convertToBase Base16
instance FromJSON ByteString where
  parseJSON = withText "ByteString" $ \v ->
   either fail pure . convertFromBase Base16 $ encodeUtf8 v

-- | Timestamp
instance ToJSON Timestamp
instance FromJSON Timestamp


-- | BlockParams
instance ToJSON BlockParams where
  toJSON = genericToJSON $ aesonDrop 11 camelCase
instance FromJSON BlockParams where
  parseJSON = genericParseJSON $ aesonDrop 11 camelCase
instance ToObject BlockParams
instance LogItem BlockParams where
  payloadKeys _ _ = AllKeys

-- | EvidenceParams
instance ToJSON EvidenceParams where
  toJSON = genericToJSON $ aesonDrop 14 camelCase
instance FromJSON EvidenceParams where
  parseJSON = genericParseJSON $ aesonDrop 14 camelCase
instance ToObject EvidenceParams
instance LogItem EvidenceParams where
  payloadKeys _ _ = AllKeys

-- | ValidatorParams
instance ToJSON ValidatorParams where
  toJSON = genericToJSON $ aesonDrop 15 camelCase
instance FromJSON ValidatorParams where
  parseJSON = genericParseJSON $ aesonDrop 15 camelCase
instance ToObject ValidatorParams
instance LogItem ValidatorParams where
  payloadKeys _ _ = AllKeys

-- | ConsensusParams
instance ToJSON ConsensusParams where
  toJSON = genericToJSON $ aesonDrop 15 camelCase
instance FromJSON ConsensusParams where
  parseJSON = genericParseJSON $ aesonDrop 15 camelCase
instance ToObject ConsensusParams
instance LogItem ConsensusParams where
  payloadKeys _ _ = AllKeys

-- | PubKey
instance ToJSON PubKey where
  toJSON = genericToJSON $ aesonDrop 6 camelCase
instance FromJSON PubKey where
  parseJSON = genericParseJSON $ aesonDrop 6 camelCase
instance ToObject PubKey
instance LogItem PubKey where
  payloadKeys _ _ = AllKeys

-- | ValidatorUpdate
instance ToJSON ValidatorUpdate where
  toJSON = genericToJSON $ aesonDrop 15 camelCase
instance FromJSON ValidatorUpdate where
  parseJSON = genericParseJSON $ aesonDrop 15 camelCase
instance ToObject ValidatorUpdate
instance LogItem ValidatorUpdate where
  payloadKeys _ _ = AllKeys

-- | Validator
instance ToJSON Validator where
  toJSON = genericToJSON $ aesonDrop 9 camelCase
instance FromJSON Validator where
  parseJSON = genericParseJSON $ aesonDrop 9 camelCase
instance ToObject Validator
instance LogItem Validator where
  payloadKeys _ _ = AllKeys

-- | VoteInfo
instance ToJSON VoteInfo where
  toJSON = genericToJSON $ aesonDrop 8 camelCase
instance FromJSON VoteInfo where
  parseJSON = genericParseJSON $ aesonDrop 8 camelCase
instance ToObject VoteInfo
instance LogItem VoteInfo where
  payloadKeys _ _ = AllKeys

-- | LastCommitInfo
instance ToJSON LastCommitInfo where
  toJSON = genericToJSON $ aesonDrop 14 camelCase
instance FromJSON LastCommitInfo where
  parseJSON = genericParseJSON $ aesonDrop 14 camelCase
instance ToObject LastCommitInfo
instance LogItem LastCommitInfo where
  payloadKeys _ _ = AllKeys

-- | PartSetHeader
instance ToJSON PartSetHeader where
  toJSON = genericToJSON $ aesonDrop 13 camelCase
instance FromJSON PartSetHeader where
  parseJSON = genericParseJSON $ aesonDrop 13 camelCase
instance ToObject PartSetHeader
instance LogItem PartSetHeader where
  payloadKeys _ _ = AllKeys

-- | BlockID
instance ToJSON BlockID where
  toJSON = genericToJSON $ aesonDrop 7 camelCase
instance FromJSON BlockID where
  parseJSON = genericParseJSON $ aesonDrop 7 camelCase
instance ToObject BlockID
instance LogItem BlockID where
  payloadKeys _ _ = AllKeys

-- | Version
instance ToJSON Version where
  toJSON = genericToJSON $ aesonDrop 7 camelCase
instance FromJSON Version where
  parseJSON = genericParseJSON $ aesonDrop 7 camelCase
instance ToObject Version
instance LogItem Version where
  payloadKeys _ _ = AllKeys

-- | Header
instance ToJSON Header where
  toJSON = genericToJSON $ aesonDrop 6 camelCase
instance FromJSON Header where
  parseJSON = genericParseJSON $ aesonDrop 6 camelCase
instance ToObject Header
instance LogItem Header where
  payloadKeys _ _ = AllKeys

-- | Evidence
instance ToJSON Evidence where
  toJSON = genericToJSON $ aesonDrop 8 camelCase
instance FromJSON Evidence where
  parseJSON = genericParseJSON $ aesonDrop 8 camelCase
instance ToObject Evidence
instance LogItem Evidence where
  payloadKeys _ _ = AllKeys

-- | KVPair
instance ToJSON KVPair where
  toJSON = genericToJSON $ aesonDrop 6 camelCase
instance FromJSON KVPair where
  parseJSON = genericParseJSON $ aesonDrop 6 camelCase
instance ToObject KVPair
instance LogItem KVPair where
  payloadKeys _ _ = AllKeys

-- | Proof
instance ToJSON Proof where
  toJSON = genericToJSON $ aesonDrop 5 camelCase
instance FromJSON Proof where
  parseJSON = genericParseJSON $ aesonDrop 5 camelCase
instance ToObject Proof
instance LogItem Proof where
  payloadKeys _ _ = AllKeys

-- | ProofOp
instance ToJSON ProofOp where
  toJSON = genericToJSON $ aesonDrop 7 camelCase
instance FromJSON ProofOp where
  parseJSON = genericParseJSON $ aesonDrop 7 camelCase
instance ToObject ProofOp
instance LogItem ProofOp where
  payloadKeys _ _ = AllKeys

-- | Event
instance ToJSON Event where
  toJSON = genericToJSON $ aesonDrop 5 camelCase
instance FromJSON Event where
  parseJSON = genericParseJSON $ aesonDrop 5 camelCase
instance ToObject Event
instance LogItem Event where
  payloadKeys _ _ = AllKeys

-- | Req.Echo
instance ToJSON Req.Echo where
  toJSON = genericToJSON $ aesonDrop 4 camelCase
instance FromJSON Req.Echo where
  parseJSON = genericParseJSON $ aesonDrop 4 camelCase
instance ToObject Req.Echo
instance LogItem Req.Echo where
  payloadKeys _ _ = AllKeys

-- | Req.Flush
instance ToJSON Req.Flush where
  toJSON = genericToJSON $ aesonDrop 5 camelCase
instance FromJSON Req.Flush where
  parseJSON = genericParseJSON $ aesonDrop 5 camelCase
instance ToObject Req.Flush
instance LogItem Req.Flush where
  payloadKeys _ _ = AllKeys

-- | Req.Info
instance ToJSON Req.Info where
  toJSON = genericToJSON $ aesonDrop 4 camelCase
instance FromJSON Req.Info where
  parseJSON = genericParseJSON $ aesonDrop 4 camelCase
instance ToObject Req.Info
instance LogItem Req.Info where
  payloadKeys _ _ = AllKeys

-- | Req.SetOption
instance ToJSON Req.SetOption where
  toJSON = genericToJSON $ aesonDrop 9 camelCase
instance FromJSON Req.SetOption where
  parseJSON = genericParseJSON $ aesonDrop 9 camelCase
instance ToObject Req.SetOption
instance LogItem Req.SetOption where
  payloadKeys _ _ = AllKeys

-- | Req.InitChain
instance ToJSON Req.InitChain where
  toJSON = genericToJSON $ aesonDrop 9 camelCase
instance FromJSON Req.InitChain where
  parseJSON = genericParseJSON $ aesonDrop 9 camelCase
instance ToObject Req.InitChain
instance LogItem Req.InitChain where
  payloadKeys _ _ = AllKeys

-- | Req.Query
instance ToJSON Req.Query where
  toJSON = genericToJSON $ aesonDrop 5 camelCase
instance FromJSON Req.Query where
  parseJSON = genericParseJSON $ aesonDrop 5 camelCase
instance ToObject Req.Query
instance LogItem Req.Query where
  payloadKeys _ _ = AllKeys

-- | Req.BeginBlock
instance ToJSON Req.BeginBlock where
  toJSON = genericToJSON $ aesonDrop 10 camelCase
instance FromJSON Req.BeginBlock where
  parseJSON = genericParseJSON $ aesonDrop 10 camelCase
instance ToObject Req.BeginBlock
instance LogItem Req.BeginBlock where
  payloadKeys _ _ = AllKeys

-- | Req.CheckTx
instance ToJSON Req.CheckTx where
  toJSON = genericToJSON $ aesonDrop 7 camelCase
instance FromJSON Req.CheckTx where
  parseJSON = genericParseJSON $ aesonDrop 7 camelCase
instance ToObject Req.CheckTx
instance LogItem Req.CheckTx where
  payloadKeys _ _ = AllKeys

-- | Req.DeliverTx
instance ToJSON Req.DeliverTx where
  toJSON = genericToJSON $ aesonDrop 9 camelCase
instance FromJSON Req.DeliverTx where
  parseJSON = genericParseJSON $ aesonDrop 9 camelCase
instance ToObject Req.DeliverTx
instance LogItem Req.DeliverTx where
  payloadKeys _ _ = AllKeys

-- | Req.EndBlock
instance ToJSON Req.EndBlock where
  toJSON = genericToJSON $ aesonDrop 8 camelCase
instance FromJSON Req.EndBlock where
  parseJSON = genericParseJSON $ aesonDrop 8 camelCase
instance ToObject Req.EndBlock
instance LogItem Req.EndBlock where
  payloadKeys _ _ = AllKeys

-- | Req.Commit
instance ToJSON Req.Commit where
  toJSON = genericToJSON $ aesonDrop 6 camelCase
instance FromJSON Req.Commit where
  parseJSON = genericParseJSON $ aesonDrop 6 camelCase
instance ToObject Req.Commit
instance LogItem Req.Commit where
  payloadKeys _ _ = AllKeys

-- | Res.Echo
instance ToJSON Res.Echo where
  toJSON = genericToJSON $ aesonDrop 4 camelCase
instance FromJSON Res.Echo where
  parseJSON = genericParseJSON $ aesonDrop 4 camelCase
instance ToObject Res.Echo
instance LogItem Res.Echo where
  payloadKeys _ _ = AllKeys

-- | Res.Flush
instance ToJSON Res.Flush where
  toJSON = genericToJSON $ aesonDrop 5 camelCase
instance FromJSON Res.Flush where
  parseJSON = genericParseJSON $ aesonDrop 5 camelCase
instance ToObject Res.Flush
instance LogItem Res.Flush where
  payloadKeys _ _ = AllKeys

-- | Res.Info
instance ToJSON Res.Info where
  toJSON = genericToJSON $ aesonDrop 4 camelCase
instance FromJSON Res.Info where
  parseJSON = genericParseJSON $ aesonDrop 4 camelCase
instance ToObject Res.Info
instance LogItem Res.Info where
  payloadKeys _ _ = AllKeys

-- | Res.SetOption
instance ToJSON Res.SetOption where
  toJSON = genericToJSON $ aesonDrop 9 camelCase
instance FromJSON Res.SetOption where
  parseJSON = genericParseJSON $ aesonDrop 9 camelCase
instance ToObject Res.SetOption
instance LogItem Res.SetOption where
  payloadKeys _ _ = AllKeys

-- | Res.InitChain
instance ToJSON Res.InitChain where
  toJSON = genericToJSON $ aesonDrop 9 camelCase
instance FromJSON Res.InitChain where
  parseJSON = genericParseJSON $ aesonDrop 9 camelCase
instance ToObject Res.InitChain
instance LogItem Res.InitChain where
  payloadKeys _ _ = AllKeys

-- | Res.Query
instance ToJSON Res.Query where
  toJSON = genericToJSON $ aesonDrop 5 camelCase
instance FromJSON Res.Query where
  parseJSON = genericParseJSON $ aesonDrop 5 camelCase
instance ToObject Res.Query
instance LogItem Res.Query where
  payloadKeys _ _ = AllKeys

-- | Res.BeginBlock
instance ToJSON Res.BeginBlock where
  toJSON = genericToJSON $ aesonDrop 10 camelCase
instance FromJSON Res.BeginBlock where
  parseJSON = genericParseJSON $ aesonDrop 10 camelCase
instance ToObject Res.BeginBlock
instance LogItem Res.BeginBlock where
  payloadKeys _ _ = AllKeys

-- | Res.CheckTx
instance ToJSON Res.CheckTx where
  toJSON = genericToJSON $ aesonDrop 7 camelCase
instance FromJSON Res.CheckTx where
  parseJSON = genericParseJSON $ aesonDrop 7 camelCase
instance ToObject Res.CheckTx
instance LogItem Res.CheckTx where
  payloadKeys _ _ = AllKeys

-- | Res.DeliverTx
instance ToJSON Res.DeliverTx where
  toJSON = genericToJSON $ aesonDrop 9 camelCase
instance FromJSON Res.DeliverTx where
  parseJSON = genericParseJSON $ aesonDrop 9 camelCase
instance ToObject Res.DeliverTx
instance LogItem Res.DeliverTx where
  payloadKeys _ _ = AllKeys

-- | Res.EndBlock
instance ToJSON Res.EndBlock where
  toJSON = genericToJSON $ aesonDrop 8 camelCase
instance FromJSON Res.EndBlock where
  parseJSON = genericParseJSON $ aesonDrop 8 camelCase
instance ToObject Res.EndBlock
instance LogItem Res.EndBlock where
  payloadKeys _ _ = AllKeys

-- | Res.Commit
instance ToJSON Res.Commit where
  toJSON = genericToJSON $ aesonDrop 6 camelCase
instance FromJSON Res.Commit where
  parseJSON = genericParseJSON $ aesonDrop 6 camelCase
instance ToObject Res.Commit
instance LogItem Res.Commit where
  payloadKeys _ _ = AllKeys

-- | Res.Exception
instance ToJSON Res.Exception where
  toJSON = genericToJSON $ aesonDrop 9 camelCase
instance FromJSON Res.Exception where
  parseJSON = genericParseJSON $ aesonDrop 9 camelCase
instance ToObject Res.Exception
instance LogItem Res.Exception where
  payloadKeys _ _ = AllKeys



