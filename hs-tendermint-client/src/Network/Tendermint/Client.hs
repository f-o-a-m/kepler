module Network.Tendermint.Client
  ( module Network.Tendermint.Client

  -- * ReExports
  , RPC.Config(..)
  , RPC.JsonRpcException(..)
  , RPC.RpcError(..)
  )
where

import           Control.Concurrent                           (forkIO,
                                                               killThread)
import           Control.Concurrent.STM.TQueue                (newTQueueIO,
                                                               writeTQueue)
import           Control.Lens                                 ((^?))
import           Control.Monad.Catch                          (throwM)
import           Control.Monad.IO.Class                       (liftIO)
import           Control.Monad.Reader                         (ReaderT, ask,
                                                               runReaderT)
import           Control.Monad.STM                            (atomically)
import           Control.Monad.Trans.Resource                 (ResourceT)
import           Data.Aeson                                   (FromJSON (..),
                                                               ToJSON (..),
                                                               genericParseJSON,
                                                               genericToJSON)
import qualified Data.Aeson                                   as Aeson
import           Data.Aeson.Casing                            (aesonDrop,
                                                               snakeCase)
import qualified Data.Aeson.Lens                              as AL
import qualified Data.ByteArray.Base64String                  as Base64
import           Data.ByteArray.HexString                     (HexString)
import           Data.ByteString                              (ByteString)
import           Data.Conduit                                 (ConduitT,
                                                               bracketP)
import           Data.Conduit.TQueue                          (sourceTQueue)
import           Data.Default.Class                           (Default (..))
import           Data.Int                                     (Int64)
import           Data.Text                                    (Text)
import           Data.Word                                    (Word32)
import           GHC.Generics                                 (Generic)
import qualified Network.ABCI.Types.Messages.FieldTypes       as FieldTypes
import qualified Network.ABCI.Types.Messages.Response         as Response
import qualified Network.HTTP.Simple                          as HTTP
import qualified Network.Tendermint.Client.Internal.RPCClient as RPC



type TendermintM = ReaderT RPC.Config IO

-- | Execute an RPC request with the given configuration.
runTendermintM :: RPC.Config -> TendermintM a -> IO a
runTendermintM = flip runReaderT

defaultConfig
  :: ByteString
  -- ^ Hostname or IP (e.g. "localhost", "127.0.0.1", "151.101.208.68")
  -> Int
  -- ^ Port
  -> Bool
  -- ^ TLS True/False
  -> RPC.Config
defaultConfig host port tls =
  let baseReq =
          HTTP.setRequestHost host
            $ HTTP.setRequestPort port
            $ HTTP.defaultRequest
  in  RPC.Config baseReq mempty mempty host port tls

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
  , requestABCIQueryHeight :: Maybe (FieldTypes.WrappedVal Int64)
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
  { requestBlockHeight :: Maybe (FieldTypes.WrappedVal Int64)
  } deriving (Eq, Show, Generic)
instance ToJSON RequestBlock where
  toJSON = genericToJSON $ defaultRPCOptions "requestBlock"

instance Default RequestBlock where
  def = RequestBlock { requestBlockHeight = Nothing }

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
  { requestTxHash  :: Maybe Tx
  , requestTxProve :: Bool
  } deriving (Eq, Show, Generic)
instance ToJSON RequestTx where
  toJSON = genericToJSON $ defaultRPCOptions "requestTx"

instance Default RequestTx where
  def = RequestTx { requestTxHash = Nothing, requestTxProve = False }

-- https://github.com/tendermint/tendermint/blob/v0.32.2/rpc/core/types/responses.go#L164
data ResultTx = ResultTx
  { resultTxHash     :: HexString
  , resultTxHeight   :: FieldTypes.WrappedVal Int64
  , resultTxIndex    :: Word32
  , resultTxTxResult :: Response.DeliverTx
  , resultTxTx       :: Tx
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
  , resultBroadcastTxCommitHeight    :: FieldTypes.WrappedVal Int64
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
-- Subscribe
--------------------------------------------------------------------------------

data TxResultEvent a = TxEvent
  { txEventBlockHeight :: FieldTypes.WrappedVal Int64
  , txEventTxIndex     :: Int64
  , txEventEvents      :: a
  } deriving (Generic)

instance FromJSON (TxResultEvent [FieldTypes.Event]) where
  parseJSON val = do
    let mtxRes = val ^? AL.key "result"
                      . AL.key "data"
                      . AL.key "value"
                      . AL.key "TxResult"
                      . AL._Object
    txRes <- maybe (fail "key not found: result.data.value.TxResult") pure mtxRes
    height <- txRes Aeson..: "height"
    idx <- txRes Aeson..: "index"
    res' <- txRes Aeson..: "result"
    es <- res' Aeson..: "events"
    pure TxEvent
      { txEventBlockHeight = height
      , txEventTxIndex = idx
      , txEventEvents = es
      }

-- | invokes [/subscribe](https://tendermint.com/rpc/#subscribe) rpc call
-- https://github.com/tendermint/tendermint/blob/master/rpc/core/events.go#L17
subscribe
  :: RequestSubscribe
  -> ConduitT () (TxResultEvent [FieldTypes.Event]) (ResourceT TendermintM) ()
subscribe req = do
  queue <- liftIO newTQueueIO
  let handler (val :: Aeson.Value) =
        let isEmptyResult = val ^? AL.key "result" == Just (Aeson.Object mempty)
        in if isEmptyResult
             then pure ()
             else case Aeson.eitherDecode . Aeson.encode $ val of
               Left err -> throwM (RPC.ParsingException err)
               Right a  -> atomically $ writeTQueue queue a
  cfg <- ask
  bracketP
    (forkIO $ RPC.remoteWS cfg (RPC.MethodName "subscribe") req handler)
    killThread
    (const $ sourceTQueue queue)

newtype RequestSubscribe = RequestSubscribe
  { requestSubscribeQuery   :: Text
  } deriving (Eq, Show, Generic)
instance ToJSON RequestSubscribe where
  toJSON = genericToJSON $ defaultRPCOptions "requestSubscribe"

-- https://github.com/tendermint/tendermint/blob/v0.32.2/rpc/core/types/responses.go#L208
data ResultSubscribe = ResultSubscribe deriving (Eq, Show)

instance FromJSON ResultSubscribe where
  parseJSON = Aeson.withObject "Expected emptyObject" $ \_ -> pure ResultSubscribe

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
data SimpleProof = SimpleProof
  { simpleProofTotal    :: FieldTypes.WrappedVal Int64
  , simpleProofIndex    :: FieldTypes.WrappedVal Int64
  , simpleProofLeafHash :: Tx
  , simpleProofAunts    :: [Tx]
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
  { dataTxs :: FieldTypes.WrappedVal [Tx]
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
type EvidenceList = FieldTypes.WrappedVal [FieldTypes.Evidence]

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
  , voteHeight           :: FieldTypes.WrappedVal Int64
  , voteRound            :: FieldTypes.WrappedVal Int
  , voteBlockId          :: FieldTypes.BlockID
  , voteTimestamp        :: FieldTypes.Timestamp
  , voteValidatorAddress :: HexString
  , voteValidatorIndex   :: FieldTypes.WrappedVal Int
  , voteSignature        :: Tx
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
