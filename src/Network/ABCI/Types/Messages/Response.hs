module Network.ABCI.Types.Messages.Response where

import           Control.Lens                           (Iso', iso, traverse,
                                                         (&), (.~), (^.), (^..),
                                                         (^?), _Just)
import qualified Control.Lens                           as Lens
import           Data.ByteString                        (ByteString)
import           Data.Int                               (Int64)
import           Data.ProtoLens.Message                 (Message (defMessage))
import           Data.Text                              (Text)
import           Data.Word                              (Word32, Word64)
import           GHC.Generics                           (Generic)
import           Network.ABCI.Types.Messages.FieldTypes (ConsensusParams,
                                                         Evidence, Header,
                                                         KVPair, LastCommitInfo,
                                                         Proof, Timestamp,
                                                         ValidatorUpdate,
                                                         consensusParams,
                                                         evidence, header,
                                                         kVPair, lastCommitInfo,
                                                         proof, timestamp,
                                                         validatorUpdate)
import qualified Proto.Types                            as PT
import qualified Proto.Types_Fields                     as PT


{-
data MessageType =
    Echo
  | Flus
  | Info
  | SetOption
  | InitChain
  | Query
  | BeginBlock
  | CheckTx
  | DeliverTx
  | EndBlock
  | Commit
-}

--------------------------------------------------------------------------------
-- Echo
--------------------------------------------------------------------------------

data Echo =
  Echo
    { echoMessage :: Text
    -- ^ The input string
    } deriving (Eq, Show, Generic)

echo :: Iso' Echo PT.ResponseEcho
echo = iso to from
  where
    to Echo{..} =
      defMessage
        & PT.message .~ echoMessage
    from responseEcho =
      Echo
        { echoMessage = responseEcho ^. PT.message
        }

--------------------------------------------------------------------------------
-- Flush
--------------------------------------------------------------------------------

data Flush =
  Flush deriving (Eq, Show, Generic)

flush :: Iso' Flush PT.ResponseFlush
flush = iso to from
  where
    to Flush =
      defMessage
    from responseFlush =
      Flush

--------------------------------------------------------------------------------
-- Info
--------------------------------------------------------------------------------

data Info =
  Info
    { infoData             :: Text
    -- ^ Some arbitrary information
    , infoVersion          :: Text
    -- ^ The application software semantic version
    , infoAppVersion       :: Word64
    -- ^ The application protocol version
    , infoLastBlockHeight  :: Int64
    -- ^  Latest block for which the app has called Commit
    , infoLastBlockAppHash :: ByteString
    -- ^  Latest result of Commit
    } deriving (Eq, Show, Generic)

info :: Iso' Info PT.ResponseInfo
info = iso to from
  where
    to Info{..} =
      defMessage
        & PT.data' .~ infoData
        & PT.version .~ infoVersion
        & PT.appVersion .~ infoAppVersion
        & PT.lastBlockHeight .~ infoLastBlockHeight
        & PT.lastBlockAppHash .~ infoLastBlockAppHash
    from responseInfo =
      Info
        { infoData = responseInfo ^. PT.data'
        , infoVersion = responseInfo ^. PT.version
        , infoAppVersion = responseInfo ^. PT.appVersion
        , infoLastBlockHeight = responseInfo ^. PT.lastBlockHeight
        , infoLastBlockAppHash = responseInfo ^. PT.lastBlockAppHash
        }

--------------------------------------------------------------------------------
-- SetOption
--------------------------------------------------------------------------------

data SetOption =
  SetOption
    { setOptionCode :: Word32
    -- ^ Response code
    , setOptionLog  :: Text
    -- ^ The output of the application's logger. May be non-deterministic.
    , setOptionInfo :: Text
    -- ^ Additional information. May be non-deterministic.
    } deriving (Eq, Show, Generic)

setOption :: Iso' SetOption PT.ResponseSetOption
setOption = iso to from
  where
    to SetOption{..} =
      defMessage
        & PT.code .~ setOptionCode
        & PT.log .~ setOptionLog
        & PT.info .~ setOptionInfo
    from responseSetOption =
      SetOption
        { setOptionCode = responseSetOption ^. PT.code
        , setOptionLog = responseSetOption ^. PT.log
        , setOptionInfo = responseSetOption ^. PT.info
        }

--------------------------------------------------------------------------------
-- InitChain
--------------------------------------------------------------------------------

data InitChain =
  InitChain
    { initChainConsensusParams :: Maybe ConsensusParams
    -- ^ Initial consensus-critical parameters.
    , initChainValidators      :: [ValidatorUpdate]
    -- ^ Initial validator set (if non empty).
    } deriving (Eq, Show, Generic)

initChain :: Iso' InitChain PT.ResponseInitChain
initChain = iso to from
  where
    to InitChain{..} =
      defMessage
        & PT.maybe'consensusParams .~ initChainConsensusParams ^? _Just . consensusParams
        & PT.validators .~ initChainValidators ^.. traverse . validatorUpdate
    from responseInitChain =
      InitChain
        { initChainConsensusParams = responseInitChain ^? PT.maybe'consensusParams . _Just . Lens.from consensusParams
        , initChainValidators = responseInitChain ^.. PT.validators . traverse . Lens.from validatorUpdate
        }

--------------------------------------------------------------------------------
-- Query
--------------------------------------------------------------------------------

data Query =
  Query
    { queryCode      :: Word32
    -- ^ Response code.
    , queryLog       :: Text
    -- ^ The output of the application's logger. May be non-deterministic.
    , queryInfo      :: Text
    -- ^ Additional information. May be non-deterministic.
    , queryIndex     :: Int64
    -- ^ The index of the key in the tree.
    , queryKey       :: ByteString
    -- ^ The key of the matching data.
    , queryValue     :: ByteString
    -- ^ The value of the matching data.
    , queryProof     :: Maybe Proof
    -- ^ Serialized proof for the value data, if requested, to be verified against
    -- the AppHash for the given Height.
    , queryHeight    :: Int64
    -- ^ The block height from which data was derived.
    , queryCodespace :: Text
    -- ^ Namespace for the Code.
    } deriving (Eq, Show, Generic)

query :: Iso' Query PT.ResponseQuery
query = iso to from
  where
    to Query{..} =
      defMessage
        & PT.code .~ queryCode
        & PT.log .~ queryLog
        & PT.info .~ queryInfo
        & PT.index .~ queryIndex
        & PT.key .~ queryKey
        & PT.value .~ queryValue
        & PT.maybe'proof .~ queryProof ^? _Just . proof
        & PT.height .~ queryHeight
        & PT.codespace .~ queryCodespace
    from responseQuery =
      Query
        { queryCode = responseQuery ^. PT.code
        , queryLog = responseQuery ^. PT.log
        , queryInfo = responseQuery ^. PT.info
        , queryIndex = responseQuery ^. PT.index
        , queryKey = responseQuery ^. PT.key
        , queryValue = responseQuery ^. PT.value
        , queryProof = responseQuery ^? PT.maybe'proof . _Just . Lens.from proof
        , queryHeight = responseQuery ^. PT.height
        , queryCodespace = responseQuery ^. PT.codespace
        }

--------------------------------------------------------------------------------
-- BeginBlock
--------------------------------------------------------------------------------

data BeginBlock =
  BeginBlock
    { beginBlockTags :: [KVPair]
    -- ^ Key-Value tags for filtering and indexing
    } deriving (Eq, Show, Generic)

beginBlock :: Iso' BeginBlock PT.ResponseBeginBlock
beginBlock = iso to from
  where
    to BeginBlock{..} =
      defMessage
        & PT.tags .~ beginBlockTags ^.. traverse . kVPair
    from responseBeginBlock =
      BeginBlock
        { beginBlockTags = responseBeginBlock ^.. PT.tags . traverse . Lens.from kVPair
        }

--------------------------------------------------------------------------------
-- CheckTx
--------------------------------------------------------------------------------

data CheckTx =
  CheckTx
    { checkTxCode      :: Word32
    -- ^ Response code
    , checkTxData      :: ByteString
    -- ^ Result bytes, if any.
    , checkTxLog       :: Text
    -- ^ The output of the application's logger.
    , checkTxInfo      :: Text
    -- ^ Additional information.
    , checkTxGasWanted :: Int64
    -- ^ Amount of gas requested for transaction.
    , checkTxGasUsed   :: Int64
    -- ^ Amount of gas consumed by transaction.
    , checkTxTags      :: [KVPair]
    -- ^ Key-Value tags for filtering and indexing transactions (eg. by account).
    , checkTxCodespace :: Text
    -- ^ Namespace for the Code.
    } deriving (Eq, Show, Generic)

checkTx :: Iso' CheckTx PT.ResponseCheckTx
checkTx = iso to from
  where
    to CheckTx{..} =
      defMessage
        & PT.code .~ checkTxCode
        & PT.data' .~ checkTxData
        & PT.log .~ checkTxLog
        & PT.info .~ checkTxInfo
        & PT.gasWanted .~ checkTxGasWanted
        & PT.gasUsed .~ checkTxGasUsed
        & PT.tags .~ checkTxTags ^.. traverse . kVPair
        & PT.codespace .~ checkTxCodespace
    from responseCheckTx =
      CheckTx
        { checkTxCode = responseCheckTx ^. PT.code
        , checkTxData = responseCheckTx ^. PT.data'
        , checkTxLog = responseCheckTx ^. PT.log
        , checkTxInfo = responseCheckTx ^. PT.info
        , checkTxGasWanted = responseCheckTx ^. PT.gasWanted
        , checkTxGasUsed = responseCheckTx ^. PT.gasUsed
        , checkTxTags = responseCheckTx ^.. PT.tags . traverse . Lens.from kVPair
        , checkTxCodespace = responseCheckTx ^. PT.codespace
        }

--------------------------------------------------------------------------------
-- DeliverTx
--------------------------------------------------------------------------------

data DeliverTx =
  DeliverTx
    { deliverTxCode      :: Word32
    -- ^ Response code.
    , deliverTxData      :: ByteString
    -- ^ Result bytes, if any.
    , deliverTxLog       :: Text
    -- ^ The output of the application's logger. May be non-deterministic.
    , deliverTxInfo      :: Text
    -- ^ Additional information.
    , deliverTxGasWanted :: Int64
    -- ^ Amount of gas requested for transaction.
    , deliverTxGasUsed   :: Int64
    -- ^ Amount of gas consumed by transaction.
    , deliverTxTags      :: [KVPair]
    -- ^  Key-Value tags for filtering and indexing transactions (eg. by account).
    , deliverTxCodespace :: Text
    -- ^ Namespace for the Code.
    } deriving (Eq, Show, Generic)

deliverTx :: Iso' DeliverTx PT.ResponseDeliverTx
deliverTx = iso to from
  where
    to DeliverTx{..} =
      defMessage
        & PT.code .~ deliverTxCode
        & PT.data' .~ deliverTxData
        & PT.log .~ deliverTxLog
        & PT.info .~ deliverTxInfo
        & PT.gasWanted .~ deliverTxGasWanted
        & PT.gasUsed .~ deliverTxGasUsed
        & PT.tags .~ deliverTxTags ^.. traverse . kVPair
        & PT.codespace .~ deliverTxCodespace
    from responseDeliverTx =
      DeliverTx
        { deliverTxCode = responseDeliverTx ^. PT.code
        , deliverTxData = responseDeliverTx ^. PT.data'
        , deliverTxLog = responseDeliverTx ^. PT.log
        , deliverTxInfo = responseDeliverTx ^. PT.info
        , deliverTxGasWanted = responseDeliverTx ^. PT.gasWanted
        , deliverTxGasUsed = responseDeliverTx ^. PT.gasUsed
        , deliverTxTags = responseDeliverTx ^.. PT.tags . traverse . Lens.from kVPair
        , deliverTxCodespace = responseDeliverTx ^. PT.codespace
        }

--------------------------------------------------------------------------------
-- EndBlock
--------------------------------------------------------------------------------

data EndBlock =
  EndBlock
    { endBlockValidatorUpdates      :: [ValidatorUpdate]
    -- ^ Changes to validator set (set voting power to 0 to remove).
    , endBlockConsensusParamUpdates :: Maybe ConsensusParams
    -- ^ Changes to consensus-critical time, size, and other parameters.
    , endBlockTags                  :: [KVPair]
    -- ^ Key-Value tags for filtering and indexing
    } deriving (Eq, Show, Generic)

endBlock :: Iso' EndBlock PT.ResponseEndBlock
endBlock = iso to from
  where
    to EndBlock{..} =
      defMessage
        & PT.validatorUpdates .~ endBlockValidatorUpdates ^.. traverse . validatorUpdate
        & PT.maybe'consensusParamUpdates .~ endBlockConsensusParamUpdates ^? _Just . consensusParams
        & PT.tags .~ endBlockTags ^.. traverse . kVPair
    from responseEndBlock =
      EndBlock
        { endBlockValidatorUpdates = responseEndBlock ^.. PT.validatorUpdates . traverse . Lens.from validatorUpdate
        , endBlockConsensusParamUpdates = responseEndBlock ^? PT.maybe'consensusParamUpdates . _Just . Lens.from consensusParams
        , endBlockTags = responseEndBlock ^.. PT.tags . traverse . Lens.from kVPair
        }

data Commit =
  Commit
    { commitData :: ByteString
    -- ^ The Merkle root hash of the application state
    } deriving (Eq, Show, Generic)

--------------------------------------------------------------------------------
-- Commit
--------------------------------------------------------------------------------

commit :: Iso' Commit PT.ResponseCommit
commit = iso to from
  where
    to Commit{..} =
      defMessage
        & PT.data' .~ commitData
    from responseCommit =
      Commit
        { commitData = responseCommit ^. PT.data'
        }

--------------------------------------------------------------------------------
-- Exception
--------------------------------------------------------------------------------

data Exception =
  Exception
    { exceptionError :: Text
    } deriving (Eq, Show, Generic)

exception :: Iso' Exception PT.ResponseException
exception = iso to from
  where
    to Exception{..} =
      defMessage
        & PT.error .~ exceptionError
    from responseException =
      Exception
        { exceptionError = responseException ^. PT.error
        }
