module Network.Tendermint.Client
  ( module Network.Tendermint.Client

  -- * ReExports
  , RPC.Config(..)
  , RPC.JsonRpcException(..)
  )
where

import           Control.Monad.Reader                         (ReaderT,
                                                               runReaderT)
import           Data.Aeson                                   (FromJSON (..),
                                                               ToJSON (..),
                                                               genericParseJSON,
                                                               genericToJSON)
import qualified Data.Aeson                                   as Aeson
import           Data.Aeson.Casing                            (aesonDrop,
                                                               snakeCase)
import qualified Data.ByteArray.Base64String                  as Base64
import           Data.ByteArray.HexString                     (HexString)
import           Data.ByteString                              (ByteString)
import           Data.Default.Class                           (Default (..))
import           Data.Text                                    (Text)
import           Data.Word                                    (Word32)
import           GHC.Generics                                 (Generic)
import qualified Network.ABCI.Types.Messages.FieldTypes       as FieldTypes
import qualified Network.ABCI.Types.Messages.Response         as Response
import qualified Network.HTTP.Simple                          as HTTP
import qualified Network.Tendermint.Client.Internal.RPCClient as RPC



type TendermintM a = ReaderT RPC.Config IO a

-- | Execute an RPC request with the given configuration.
runTendermintM :: RPC.Config -> TendermintM a -> IO a
runTendermintM = flip runReaderT

defaultConfig
  :: ByteString
  -- ^ Hostname or IP (e.g. "localhost", "127.0.0.1", "151.101.208.68")
  -> Int
  -- ^ Port
  -> RPC.Config
defaultConfig host port =
  let baseReq =
          HTTP.setRequestHost host
            $ HTTP.setRequestPort port
            $ HTTP.defaultRequest
  in  RPC.Config baseReq mempty mempty

--------------------------------------------------------------------------------
-- ABCI Query
--------------------------------------------------------------------------------

-- | invokes [/abci_query](https://tendermint.com/rpc/#abciquery) rpc call
-- https://github.com/tendermint/tendermint/blob/v0.32.2/rpc/core/abci.go#L56
abciQuery :: RequestABCIQuery -> TendermintM ResultABCIQuery
abciQuery = RPC.remote (RPC.MethodName "abci_query")

-- https://github.com/tendermint/tendermint/blob/v0.32.2/rpc/core/abci.go#L56
data RequestABCIQuery = RequestABCIQuery
  { requestABCIQueryPath   :: Maybe Text
  , requestABCIQueryData   :: HexString
  , requestABCIQueryHeight :: Maybe FieldTypes.WrappedInt64
  , requestABCIQueryProve  :: Bool
  } deriving (Eq, Show, Generic)
instance ToJSON RequestABCIQuery where
  toJSON = genericToJSON $ defaultRPCOptions "requestABCIQuery"

instance Default RequestABCIQuery where
  def = RequestABCIQuery { requestABCIQueryPath   = Nothing
                         , requestABCIQueryData   = ""
                         , requestABCIQueryHeight = Nothing
                         , requestABCIQueryProve  = False
                         }

-- https://github.com/tendermint/tendermint/blob/v0.32.2/rpc/core/types/responses.go#L193
data ResultABCIQuery = ResultABCIQuery
  { resultABCIQueryResponse :: Response.Query
  } deriving (Eq, Show, Generic)
instance FromJSON ResultABCIQuery where
  parseJSON = genericParseJSON $ defaultRPCOptions "resultABCIQuery"

--------------------------------------------------------------------------------
-- Block
--------------------------------------------------------------------------------

-- | invokes [/block](https://tendermint.com/rpc/#block) rpc call
-- https://github.com/tendermint/tendermint/blob/v0.32.2/rpc/core/blocks.go#L72
block :: RequestBlock -> TendermintM ResultBlock
block = RPC.remote (RPC.MethodName "block")

-- https://github.com/tendermint/tendermint/blob/v0.32.2/rpc/core/blocks.go#L72
data RequestBlock = RequestBlock
  { requestBlockHeightPtr :: Maybe FieldTypes.WrappedInt64
  } deriving (Eq, Show, Generic)
instance ToJSON RequestBlock where
  toJSON = genericToJSON $ defaultRPCOptions "requestBlock"

instance Default RequestBlock where
  def = RequestBlock { requestBlockHeightPtr = Nothing }

-- https://github.com/tendermint/tendermint/blob/v0.32.2/rpc/core/types/responses.go#L28
data ResultBlock = ResultBlock
  { resultBlockBlockMeta :: BlockMeta
  , resultBlockBlock     :: Block
  } deriving (Eq, Show, Generic)
instance FromJSON ResultBlock where
  parseJSON = genericParseJSON $ defaultRPCOptions "resultBlock"


--------------------------------------------------------------------------------
-- Tx
--------------------------------------------------------------------------------

-- | invokes [/tx](https://tendermint.com/rpc/#tx) rpc call
-- https://github.com/tendermint/tendermint/blob/v0.32.2/rpc/core/tx.go#L81
tx :: RequestTx -> TendermintM ResultTx
tx = RPC.remote (RPC.MethodName "tx")

-- https://github.com/tendermint/tendermint/blob/v0.32.2/rpc/core/tx.go#L81
data RequestTx = RequestTx
  { requestTxHash  :: Maybe Tx -- @NOTE: this seems to need a Base64String
  , requestTxProve :: Bool
  } deriving (Eq, Show, Generic)
instance ToJSON RequestTx where
  toJSON = genericToJSON $ defaultRPCOptions "requestTx"

instance Default RequestTx where
  def = RequestTx { requestTxHash = Nothing, requestTxProve = False }

-- https://github.com/tendermint/tendermint/blob/v0.32.2/rpc/core/types/responses.go#L164
data ResultTx = ResultTx
  { resultTxHash     :: HexString
  , resultTxHeight   :: FieldTypes.WrappedInt64
  , resultTxIndex    :: Word32
  , resultTxTxResult :: Response.DeliverTx
  , resultTxTx       :: Tx
  -- @NOTE: this key is entirely absent when a proof is not requested
  -- i.e., when requestTxProve = false
  , resultTxProof    :: Maybe TxProof
  } deriving (Eq, Show, Generic)

instance FromJSON ResultTx where
  parseJSON = genericParseJSON $ defaultRPCOptions "resultTx"

--------------------------------------------------------------------------------
-- BroadcastTxAsync
--------------------------------------------------------------------------------

-- | invokes [/broadcast_tx_async](https://tendermint.com/rpc/#broadcasttxasync) rpc call
-- https://github.com/tendermint/tendermint/blob/v0.32.2/rpc/core/mempool.go#L75
broadcastTxAsync :: RequestBroadcastTxAsync -> TendermintM ResultBroadcastTx
broadcastTxAsync = RPC.remote (RPC.MethodName "broadcast_tx_async")

-- https://github.com/tendermint/tendermint/blob/v0.32.2/rpc/core/mempool.go#L75
data RequestBroadcastTxAsync = RequestBroadcastTxAsync
  { requestBroadcastTxAsyncTx :: Tx
  } deriving (Eq, Show, Generic)
instance ToJSON RequestBroadcastTxAsync where
  toJSON = genericToJSON $ defaultRPCOptions "requestBroadcastTxAsync"

--------------------------------------------------------------------------------
-- BroadcastTxSync
--------------------------------------------------------------------------------

-- | invokes [/broadcast_tx_sync](https://tendermint.com/rpc/#broadcasttxsync) rpc call
-- https://github.com/tendermint/tendermint/blob/v0.32.2/rpc/core/mempool.go#L136
broadcastTxSync :: RequestBroadcastTxSync -> TendermintM ResultBroadcastTx
broadcastTxSync = RPC.remote (RPC.MethodName "broadcast_tx_sync")

-- https://github.com/tendermint/tendermint/blob/v0.32.2/rpc/core/mempool.go#L136
data RequestBroadcastTxSync = RequestBroadcastTxSync
  { requestBroadcastTxSyncTx :: Tx
  } deriving (Eq, Show, Generic)
instance ToJSON RequestBroadcastTxSync where
  toJSON = genericToJSON $ defaultRPCOptions "requestBroadcastTxSync"

--------------------------------------------------------------------------------
-- BroadcastTxCommit
--------------------------------------------------------------------------------

-- | invokes [/broadcast_tx_commit](https://tendermint.com/rpc/#broadcasttxcommit) rpc call
-- https://github.com/tendermint/tendermint/blob/v0.32.2/rpc/core/mempool.go#L215
broadcastTxCommit
  :: RequestBroadcastTxCommit -> TendermintM ResultBroadcastTxCommit
broadcastTxCommit = RPC.remote (RPC.MethodName "broadcast_tx_commit")

-- https://github.com/tendermint/tendermint/blob/v0.32.2/rpc/core/mempool.go#L215
data RequestBroadcastTxCommit = RequestBroadcastTxCommit
  { requestBroadcastTxCommitTx :: Tx
  } deriving (Eq, Show, Generic)
instance ToJSON RequestBroadcastTxCommit where
  toJSON = genericToJSON $ defaultRPCOptions "requestBroadcastTxCommit"

-- https://github.com/tendermint/tendermint/blob/v0.32.2/rpc/core/types/responses.go#L156
data ResultBroadcastTxCommit = ResultBroadcastTxCommit
  { resultBroadcastTxCommitCheckTx   :: Response.CheckTx
  , resultBroadcastTxCommitDeliverTx :: Response.DeliverTx
  , resultBroadcastTxCommitHash      :: HexString
  , resultBroadcastTxCommitHeight    :: FieldTypes.WrappedInt64
  } deriving (Eq, Show, Generic)
instance FromJSON ResultBroadcastTxCommit where
  parseJSON = genericParseJSON $ defaultRPCOptions "resultBroadcastTxCommit"


--------------------------------------------------------------------------------
-- Health
--------------------------------------------------------------------------------

-- | invokes [/health](https://tendermint.com/rpc/#health) rpc call
-- https://github.com/tendermint/tendermint/blob/v0.32.2/rpc/core/health.go#L35
health :: TendermintM ResultHealth
health = RPC.remote (RPC.MethodName "health") ()

-- https://github.com/tendermint/tendermint/blob/v0.32.2/rpc/core/types/responses.go#L208
data ResultHealth = ResultHealth deriving (Eq, Show)

instance FromJSON ResultHealth where
  parseJSON = Aeson.withObject "Expected emptyObject" $ \_ -> pure ResultHealth

--------------------------------------------------------------------------------
-- ABCIInfo
--------------------------------------------------------------------------------

-- | invokes [/abci_info](https://tendermint.com/rpc/#abciinfo) rpc call
-- https://github.com/tendermint/tendermint/blob/v0.32.2/rpc/core/abci.go#L100
abciInfo :: TendermintM ResultABCIInfo
abciInfo = RPC.remote (RPC.MethodName "abci_info") ()

-- https://github.com/tendermint/tendermint/blob/v0.32.2/rpc/core/types/responses.go#L188
data ResultABCIInfo = ResultABCIInfo
  { resultABCIInfoResponse :: Response.Info
  } deriving (Eq, Show, Generic)
instance FromJSON ResultABCIInfo where
  parseJSON = genericParseJSON $ defaultRPCOptions "resultABCIInfo"

--------------------------------------------------------------------------------

-- https://github.com/tendermint/tendermint/blob/v0.32.2/rpc/core/types/responses.go#L147
data ResultBroadcastTx = ResultBroadcastTx
  { resultBroadcastTxCode :: Word32
  , resultBroadcastTxData :: HexString
  , resultBroadcastTxLog  :: Text
  , resultBroadcastTxHash :: HexString
  } deriving (Eq, Show, Generic)
instance FromJSON ResultBroadcastTx where
  parseJSON = genericParseJSON $ defaultRPCOptions "resultBroadcastTx"

-- https://github.com/tendermint/tendermint/blob/v0.32.2/types/tx.go#L85
data TxProof = TxProof
  { txProofRootHash :: HexString
  , txProofData     :: Tx
  , txProofProof    :: SimpleProof
  } deriving (Eq, Show, Generic)
instance FromJSON TxProof where
  parseJSON = genericParseJSON $ defaultRPCOptions "txProof"

-- https://github.com/tendermint/tendermint/blob/v0.32.2/crypto/merkle/simple_proof.go#L18
-- example:
-- {
--     "total": "3",
--     "aunts": [
--         "J9h6nUSxNEabklxRnNIxlDWC8iuP4ikN4ldOA9db59Y="
--     ],
--     "leaf_hash": "O2xyvrxEZebIcC1W6z9VCsZCEjy4urohAS0pAjkGt88=",
--     "index": "2"
-- }
data SimpleProof = SimpleProof
  { simpleProofTotal    :: Int -- @NOTE: this is a string
  , simpleProofIndex    :: Int -- ''
  , simpleProofLeafHash :: HexString -- @NOTE: this looks like a Base64String (see above)
  , simpleProofAunts    :: [HexString] -- ''
  } deriving (Eq, Show, Generic)
instance FromJSON SimpleProof where
  parseJSON = genericParseJSON $ defaultRPCOptions "simpleProof"

-- https://github.com/tendermint/tendermint/blob/v0.32.2/types/block_meta.go#L4
data BlockMeta = BlockMeta
  { blockMetaBlockId :: FieldTypes.BlockID
  , blockMetaHeader  :: FieldTypes.Header
  } deriving (Eq, Show, Generic)
instance FromJSON BlockMeta where
  parseJSON = genericParseJSON $ defaultRPCOptions "blockMeta"

-- https://github.com/tendermint/tendermint/blob/v0.32.2/types/block.go#L36
data Block = Block
  { blockHeader     :: FieldTypes.Header
  , blockData       :: Data
  , blockEvidence   :: EvidenceData
  , blockLastCommit :: Maybe Commit
  } deriving (Eq, Show, Generic)
instance FromJSON Block where
  parseJSON = genericParseJSON $ defaultRPCOptions "block"

-- https://github.com/tendermint/tendermint/blob/v0.32.2/types/block.go#L774
data Data = Data
  { dataTxs :: [Tx]
  } deriving (Eq, Show, Generic)
instance FromJSON Data where
  parseJSON = genericParseJSON $ defaultRPCOptions "data"

-- https://github.com/tendermint/tendermint/blob/v0.32.2/types/block.go#L819~
data EvidenceData = EvidenceData
  { evidenceDataEvidence :: EvidenceList
  } deriving (Eq, Show, Generic)
instance FromJSON EvidenceData where
  parseJSON = genericParseJSON $ defaultRPCOptions "evidenceData"

-- https://github.com/tendermint/tendermint/blob/v0.32.2/types/evidence.go#L278
type EvidenceList = [FieldTypes.Evidence]

-- https://github.com/tendermint/tendermint/blob/v0.32.2/types/block.go#L488
data Commit = Commit
  { commitBlockId    :: FieldTypes.BlockID
  , commitPrecommits :: [Vote]
  } deriving (Eq, Show, Generic)
instance FromJSON Commit where
  parseJSON = genericParseJSON $ defaultRPCOptions "commit"

-- https://github.com/tendermint/tendermint/blob/v0.32.2/types/vote.go#L51
data Vote = Vote
  { voteType             :: SignedMsgType
  , voteHeight           :: FieldTypes.WrappedInt64
  , voteRound            :: Int
  , voteBlockId          :: FieldTypes.BlockID
  , voteTimestamp        :: FieldTypes.Timestamp
  , voteValidatorAddress :: HexString
  , voteValidatorIndex   :: Int
  , voteSignature        :: HexString
  } deriving (Eq, Show, Generic)
instance FromJSON Vote where
  parseJSON = genericParseJSON $ defaultRPCOptions "vote"

-- https://github.com/tendermint/tendermint/blob/v0.32.2/types/tx.go#L19
type Tx = Base64.Base64String

-- https://github.com/tendermint/tendermint/blob/v0.32.2/types/signed_msg_type.go#L4
data SignedMsgType
  = PrevoteType
  | PrecommitType
  | ProposalType
  deriving (Eq, Show, Generic)

instance FromJSON SignedMsgType where
  parseJSON = Aeson.withScientific "SignedMsgType" $ \n -> case n of
    1  -> pure PrevoteType
    2  -> pure PrecommitType
    32 -> pure ProposalType
    _  -> fail $ "invalid SignedMsg code: " <> show n

defaultRPCOptions :: String -> Aeson.Options
defaultRPCOptions prefix = aesonDrop (length prefix) snakeCase
