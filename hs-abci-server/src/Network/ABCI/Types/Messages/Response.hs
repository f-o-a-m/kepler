module Network.ABCI.Types.Messages.Response
  ( Response(..)
  , toProto
  -- * Request Message Types
  , Echo(..)
  , Flush(..)
  , Info(..)
  , SetOption(..)
  , InitChain(..)
  , Query(..)
  , BeginBlock(..)
  , CheckTx(..)
  , DeliverTx(..)
  , EndBlock(..)
  , Commit(..)
  , Exception(..)

  -- * ReExports
  , MessageType(..)
  ) where

import           Control.Lens                           (iso, traverse, (&),
                                                         (.~), (?~), (^.),
                                                         (^..), (^?), _Just)
import           Control.Lens.Wrapped                   (Wrapped (..),
                                                         _Unwrapped')
import           Data.Int                               (Int64)
import           Data.Aeson                             (FromJSON (..),
                                                         ToJSON (..),
                                                         genericParseJSON,
                                                         genericToJSON)
import           Data.ByteArray.HexString
                                                                                   (HexString,
                                                                                   fromBytes,
                                                                                   toBytes)
import           Data.ProtoLens.Message                 (Message (defMessage))
import           Data.ProtoLens.Prism                   (( # ))
import           Data.Text                              (Text)
import           Data.Word                              (Word32, Word64)
import           GHC.Generics                           (Generic)
import           Network.ABCI.Types.Messages.Common (defaultABCIOptions)
import           Network.ABCI.Types.Messages.FieldTypes (ConsensusParams, Event,
                                                         Proof, ValidatorUpdate)
import           Network.ABCI.Types.Messages.Types      (MessageType (..))
import qualified Proto.Types                            as PT
import qualified Proto.Types_Fields                     as PT


--------------------------------------------------------------------------------
-- Response
--------------------------------------------------------------------------------

data Response (m :: MessageType) :: * where
  ResponseEcho :: Echo -> Response 'MTEcho
  ResponseFlush :: Flush -> Response 'MTFlush
  ResponseInfo :: Info -> Response 'MTInfo
  ResponseSetOption :: SetOption -> Response 'MTSetOption
  ResponseInitChain :: InitChain -> Response 'MTInitChain
  ResponseQuery :: Query -> Response 'MTQuery
  ResponseBeginBlock :: BeginBlock -> Response 'MTBeginBlock
  ResponseCheckTx :: CheckTx -> Response 'MTCheckTx
  ResponseDeliverTx :: DeliverTx -> Response 'MTDeliverTx
  ResponseEndBlock :: EndBlock -> Response 'MTEndBlock
  ResponseCommit :: Commit -> Response 'MTCommit
  ResponseException :: forall (m :: MessageType) . Exception -> Response m

instance ToJSON (Response (t :: MessageType)) where
  toJSON (ResponseEcho v)       = toJSON v
  toJSON (ResponseFlush v)      = toJSON v
  toJSON (ResponseInfo v)       = toJSON v
  toJSON (ResponseSetOption v)  = toJSON v
  toJSON (ResponseInitChain v)  = toJSON v
  toJSON (ResponseQuery v)      = toJSON v
  toJSON (ResponseBeginBlock v) = toJSON v
  toJSON (ResponseCheckTx v)    = toJSON v
  toJSON (ResponseDeliverTx v)  = toJSON v
  toJSON (ResponseEndBlock v)   = toJSON v
  toJSON (ResponseCommit v)     = toJSON v
  toJSON (ResponseException v)  = toJSON v

instance FromJSON (Response 'MTEcho) where
  parseJSON = fmap ResponseEcho . parseJSON
instance FromJSON (Response 'MTFlush) where
  parseJSON = fmap ResponseFlush . parseJSON
instance FromJSON (Response 'MTInfo) where
  parseJSON = fmap ResponseInfo . parseJSON
instance FromJSON (Response 'MTSetOption) where
  parseJSON = fmap ResponseSetOption . parseJSON
instance FromJSON (Response 'MTInitChain) where
  parseJSON = fmap ResponseInitChain . parseJSON
instance FromJSON (Response 'MTQuery) where
  parseJSON = fmap ResponseQuery . parseJSON
instance FromJSON (Response 'MTBeginBlock) where
  parseJSON = fmap ResponseBeginBlock . parseJSON
instance FromJSON (Response 'MTCheckTx) where
  parseJSON = fmap ResponseCheckTx . parseJSON
instance FromJSON (Response 'MTDeliverTx) where
  parseJSON = fmap ResponseDeliverTx . parseJSON
instance FromJSON (Response 'MTEndBlock) where
  parseJSON = fmap ResponseEndBlock . parseJSON
instance FromJSON (Response 'MTCommit) where
  parseJSON = fmap ResponseCommit . parseJSON

-- | Translates type-safe 'Response' GADT to the unsafe
--   auto-generated 'Proto.Response'
toProto :: Response t -> PT.Response
toProto r = case r of
  ResponseEcho msg       -> wrap PT._Response'Echo msg
  ResponseFlush msg      -> wrap PT._Response'Flush msg
  ResponseInfo msg       -> wrap PT._Response'Info msg
  ResponseSetOption msg  -> wrap PT._Response'SetOption msg
  ResponseInitChain msg  -> wrap PT._Response'InitChain msg
  ResponseQuery msg      -> wrap PT._Response'Query msg
  ResponseBeginBlock msg -> wrap PT._Response'BeginBlock msg
  ResponseCheckTx msg    -> wrap PT._Response'CheckTx msg
  ResponseDeliverTx msg  -> wrap PT._Response'DeliverTx msg
  ResponseEndBlock msg   -> wrap PT._Response'EndBlock msg
  ResponseCommit msg     -> wrap PT._Response'Commit msg
  ResponseException msg  -> wrap PT._Response'Exception msg
  where
    wrap v msg = defMessage & PT.maybe'value ?~ v # (msg ^. _Wrapped')

--------------------------------------------------------------------------------
-- Echo
--------------------------------------------------------------------------------

data Echo = Echo
  { echoMessage :: Text
  -- ^ The input string
  } deriving (Eq, Show, Generic)

instance ToJSON Echo where
  toJSON = genericToJSON $ defaultABCIOptions "echo"
instance FromJSON Echo where
  parseJSON = genericParseJSON $ defaultABCIOptions "echo"

instance Wrapped Echo where
  type Unwrapped Echo = PT.ResponseEcho

  _Wrapped' = iso t f
    where
      t Echo{..} =
        defMessage
          & PT.message .~ echoMessage
      f message =
        Echo
          { echoMessage = message ^. PT.message
          }

--------------------------------------------------------------------------------
-- Flush
--------------------------------------------------------------------------------

data Flush =
  Flush deriving (Eq, Show, Generic)

instance ToJSON Flush where
  toJSON = genericToJSON $ defaultABCIOptions "flush"
instance FromJSON Flush where
  parseJSON = genericParseJSON $ defaultABCIOptions "flush"

instance Wrapped Flush where
  type Unwrapped Flush = PT.ResponseFlush

  _Wrapped' = iso t f
    where
      t Flush =
        defMessage
      f _ =
        Flush

--------------------------------------------------------------------------------
-- Info
--------------------------------------------------------------------------------

data Info = Info
  { infoData             :: Text
  -- ^ Some arbitrary information
  , infoVersion          :: Text
  -- ^ The application software semantic version
  , infoAppVersion       :: Word64
  -- ^ The application protocol version
  , infoLastBlockHeight  :: Int64
  -- ^  Latest block for which the app has called Commit
  , infoLastBlockAppHash :: HexString
  -- ^  Latest result of Commit
  } deriving (Eq, Show, Generic)

instance ToJSON Info where
  toJSON = genericToJSON $ defaultABCIOptions "info"
instance FromJSON Info where
  parseJSON = genericParseJSON $ defaultABCIOptions "info"

instance Wrapped Info where
  type Unwrapped Info = PT.ResponseInfo

  _Wrapped' = iso t f
    where
     t Info{..} =
      defMessage
        & PT.data' .~ infoData
        & PT.version .~ infoVersion
        & PT.appVersion .~ infoAppVersion
        & PT.lastBlockHeight .~ infoLastBlockHeight
        & PT.lastBlockAppHash .~ toBytes infoLastBlockAppHash
     f message =
       Info
         { infoData = message ^. PT.data'
         , infoVersion = message ^. PT.version
         , infoAppVersion = message ^. PT.appVersion
         , infoLastBlockHeight = message ^. PT.lastBlockHeight
         , infoLastBlockAppHash = fromBytes $ message ^. PT.lastBlockAppHash
         }

--------------------------------------------------------------------------------
-- SetOption
--------------------------------------------------------------------------------

data SetOption = SetOption
  { setOptionCode :: Word32
  -- ^ Response code
  , setOptionLog  :: Text
  -- ^ The output of the application's logger. May be non-deterministic.
  , setOptionInfo :: Text
  -- ^ Additional information. May be non-deterministic.
  } deriving (Eq, Show, Generic)

instance ToJSON SetOption where
  toJSON = genericToJSON $ defaultABCIOptions "setOption"
instance FromJSON SetOption where
  parseJSON = genericParseJSON $ defaultABCIOptions "setOption"

instance Wrapped SetOption where
  type Unwrapped SetOption = PT.ResponseSetOption

  _Wrapped' = iso t f
    where
      t SetOption{..} =
        defMessage
          & PT.code .~ setOptionCode
          & PT.log .~ setOptionLog
          & PT.info .~ setOptionInfo
      f message =
        SetOption
          { setOptionCode = message ^. PT.code
          , setOptionLog = message ^. PT.log
          , setOptionInfo = message ^. PT.info
          }

--------------------------------------------------------------------------------
-- InitChain
--------------------------------------------------------------------------------

data InitChain = InitChain
  { initChainConsensusParams :: Maybe ConsensusParams
  -- ^ Initial consensus-critical parameters.
  , initChainValidators      :: [ValidatorUpdate]
  -- ^ Initial validator set (if non empty).
  } deriving (Eq, Show, Generic)

instance ToJSON InitChain where
  toJSON = genericToJSON $ defaultABCIOptions "initChain"
instance FromJSON InitChain where
  parseJSON = genericParseJSON $ defaultABCIOptions "initChain"

instance Wrapped InitChain where
  type Unwrapped InitChain = PT.ResponseInitChain

  _Wrapped' = iso t f
    where
      t InitChain{..} =
        defMessage
          & PT.maybe'consensusParams .~ initChainConsensusParams ^? _Just . _Wrapped'
          & PT.validators .~ initChainValidators ^.. traverse . _Wrapped'
      f message =
        InitChain
          { initChainConsensusParams = message ^? PT.maybe'consensusParams . _Just . _Unwrapped'
          , initChainValidators = message ^.. PT.validators . traverse . _Unwrapped'
          }

--------------------------------------------------------------------------------
-- Query
--------------------------------------------------------------------------------

data Query = Query
  { queryCode      :: Word32
  -- ^ Response code.
  , queryLog       :: Text
  -- ^ The output of the application's logger. May be non-deterministic.
  , queryInfo      :: Text
  -- ^ Additional information. May be non-deterministic.
  , queryIndex     :: Int64
  -- ^ The index of the key in the tree.
  , queryKey       :: HexString
  -- ^ The key of the matching data.
  , queryValue     :: HexString
  -- ^ The value of the matching data.
  , queryProof     :: Maybe Proof
  -- ^ Serialized proof for the value data, if requested, to be verified against
  -- the AppHash for the given Height.
  , queryHeight    :: Int64
  -- ^ The block height from which data was derived.
  , queryCodespace :: Text
  -- ^ Namespace for the Code.
  } deriving (Eq, Show, Generic)

instance ToJSON Query where
  toJSON = genericToJSON $ defaultABCIOptions "query"
instance FromJSON Query where
  parseJSON = genericParseJSON $ defaultABCIOptions "query"

instance Wrapped Query where
  type Unwrapped Query = PT.ResponseQuery

  _Wrapped' = iso t f
    where
      t Query{..} =
        defMessage
          & PT.code .~ queryCode
          & PT.log .~ queryLog
          & PT.info .~ queryInfo
          & PT.index .~ queryIndex
          & PT.key .~ toBytes queryKey
          & PT.value .~ toBytes queryValue
          & PT.maybe'proof .~ queryProof ^? _Just . _Wrapped'
          & PT.height .~ queryHeight
          & PT.codespace .~ queryCodespace
      f message =
        Query
          { queryCode = message ^. PT.code
          , queryLog = message ^. PT.log
          , queryInfo = message ^. PT.info
          , queryIndex = message ^. PT.index
          , queryKey = fromBytes $ message ^. PT.key
          , queryValue = fromBytes $ message ^. PT.value
          , queryProof = message ^? PT.maybe'proof . _Just . _Unwrapped'
          , queryHeight = message ^. PT.height
          , queryCodespace = message ^. PT.codespace
          }

--------------------------------------------------------------------------------
-- BeginBlock
--------------------------------------------------------------------------------

data BeginBlock = BeginBlock
  { beginBlockEvents :: [Event]
  -- ^ Beginning block events
  } deriving (Eq, Show, Generic)

instance ToJSON BeginBlock where
  toJSON = genericToJSON $ defaultABCIOptions "beginBlock"
instance FromJSON BeginBlock where
  parseJSON = genericParseJSON $ defaultABCIOptions "beginBlock"

instance Wrapped BeginBlock where
  type Unwrapped BeginBlock = PT.ResponseBeginBlock

  _Wrapped' = iso t f
    where
      t BeginBlock{..} =
        defMessage
          & PT.events .~ beginBlockEvents ^.. traverse . _Wrapped'
      f message =
        BeginBlock
          { beginBlockEvents = message ^.. PT.events . traverse . _Unwrapped'
          }

--------------------------------------------------------------------------------
-- CheckTx
--------------------------------------------------------------------------------

data CheckTx = CheckTx
  { checkTxCode      :: Word32
  -- ^ Response code
  , checkTxData      :: HexString
  -- ^ Result bytes, if any.
  , checkTxLog       :: Text
  -- ^ The output of the application's logger.
  , checkTxInfo      :: Text
  -- ^ Additional information.
  , checkTxGasWanted :: Int64
  -- ^ Amount of gas requested for transaction.
  , checkTxGasUsed   :: Int64
  -- ^ Amount of gas consumed by transaction.
  , checkTxEvents    :: [Event]
  -- ^ Events
  , checkTxCodespace :: Text
  -- ^ Namespace for the Code.
  } deriving (Eq, Show, Generic)

instance ToJSON CheckTx where
  toJSON = genericToJSON $ defaultABCIOptions "checkTx"
instance FromJSON CheckTx where
  parseJSON = genericParseJSON $ defaultABCIOptions "checkTx"

instance Wrapped CheckTx where
  type Unwrapped CheckTx = PT.ResponseCheckTx

  _Wrapped' = iso t f
    where
      t CheckTx{..} =
        defMessage
          & PT.code .~ checkTxCode
          & PT.data' .~ toBytes checkTxData
          & PT.log .~ checkTxLog
          & PT.info .~ checkTxInfo
          & PT.gasWanted .~ checkTxGasWanted
          & PT.gasUsed .~ checkTxGasUsed
          & PT.events .~ checkTxEvents ^.. traverse . _Wrapped'
          & PT.codespace .~ checkTxCodespace
      f message =
        CheckTx
          { checkTxCode = message ^. PT.code
          , checkTxData = fromBytes $ message ^. PT.data'
          , checkTxLog = message ^. PT.log
          , checkTxInfo = message ^. PT.info
          , checkTxGasWanted = message ^. PT.gasWanted
          , checkTxGasUsed = message ^. PT.gasUsed
          , checkTxEvents = message ^.. PT.events . traverse . _Unwrapped'
          , checkTxCodespace = message ^. PT.codespace
          }

--------------------------------------------------------------------------------
-- DeliverTx
--------------------------------------------------------------------------------

data DeliverTx = DeliverTx
  { deliverTxCode      :: Word32
  -- ^ Response code.
  , deliverTxData      :: HexString
  -- ^ Result bytes, if any.
  , deliverTxLog       :: Text
  -- ^ The output of the application's logger. May be non-deterministic.
  , deliverTxInfo      :: Text
  -- ^ Additional information.
  , deliverTxGasWanted :: Int64
  -- ^ Amount of gas requested for transaction.
  , deliverTxGasUsed   :: Int64
  -- ^ Amount of gas consumed by transaction.
  , deliverTxEvents    :: [Event]
  -- ^ Events
  , deliverTxCodespace :: Text
  -- ^ Namespace for the Code.
  } deriving (Eq, Show, Generic)

instance ToJSON DeliverTx where
  toJSON = genericToJSON $ defaultABCIOptions "deliverTx"
instance FromJSON DeliverTx where
  parseJSON = genericParseJSON $ defaultABCIOptions "deliverTx"

instance Wrapped DeliverTx where
  type Unwrapped DeliverTx = PT.ResponseDeliverTx

  _Wrapped' = iso t f
    where
      t DeliverTx{..} =
        defMessage
          & PT.code .~ deliverTxCode
          & PT.data' .~ toBytes deliverTxData
          & PT.log .~ deliverTxLog
          & PT.info .~ deliverTxInfo
          & PT.gasWanted .~ deliverTxGasWanted
          & PT.gasUsed .~ deliverTxGasUsed
          & PT.events .~ deliverTxEvents ^.. traverse . _Wrapped'
          & PT.codespace .~ deliverTxCodespace
      f responseDeliverTx =
        DeliverTx
          { deliverTxCode = responseDeliverTx ^. PT.code
          , deliverTxData = fromBytes $ responseDeliverTx ^. PT.data'
          , deliverTxLog = responseDeliverTx ^. PT.log
          , deliverTxInfo = responseDeliverTx ^. PT.info
          , deliverTxGasWanted = responseDeliverTx ^. PT.gasWanted
          , deliverTxGasUsed = responseDeliverTx ^. PT.gasUsed
          , deliverTxEvents = responseDeliverTx ^.. PT.events . traverse . _Unwrapped'
          , deliverTxCodespace = responseDeliverTx ^. PT.codespace
          }

--------------------------------------------------------------------------------
-- EndBlock
--------------------------------------------------------------------------------

data EndBlock = EndBlock
  { endBlockValidatorUpdates      :: [ValidatorUpdate]
  -- ^ Changes to validator set (set voting power to 0 to remove).
  , endBlockConsensusParamUpdates :: Maybe ConsensusParams
  -- ^ Changes to consensus-critical time, size, and other parameters.
  , endBlockEvents                :: [Event]
  -- ^ Events
  } deriving (Eq, Show, Generic)

instance ToJSON EndBlock where
  toJSON = genericToJSON $ defaultABCIOptions "endBlock"
instance FromJSON EndBlock where
  parseJSON = genericParseJSON $ defaultABCIOptions "endBlock"

instance Wrapped EndBlock where
  type Unwrapped EndBlock = PT.ResponseEndBlock

  _Wrapped' = iso t f
    where
      t EndBlock{..} =
        defMessage
          & PT.validatorUpdates .~ endBlockValidatorUpdates ^.. traverse . _Wrapped'
          & PT.maybe'consensusParamUpdates .~ endBlockConsensusParamUpdates ^? _Just . _Wrapped'
          & PT.events .~ endBlockEvents ^.. traverse . _Wrapped'
      f message =
        EndBlock
          { endBlockValidatorUpdates = message ^.. PT.validatorUpdates . traverse . _Unwrapped'
          , endBlockConsensusParamUpdates = message ^? PT.maybe'consensusParamUpdates . _Just . _Unwrapped'
          , endBlockEvents = message ^.. PT.events . traverse . _Unwrapped'
          }

--------------------------------------------------------------------------------
-- Commit
--------------------------------------------------------------------------------

data Commit = Commit
  { commitData :: HexString
  -- ^ The Merkle root hash of the application state
  } deriving (Eq, Show, Generic)

instance ToJSON Commit where
  toJSON = genericToJSON $ defaultABCIOptions "commit"
instance FromJSON Commit where
  parseJSON = genericParseJSON $ defaultABCIOptions "commit"

instance Wrapped Commit where
  type Unwrapped Commit = PT.ResponseCommit

  _Wrapped' = iso t f
    where
      t Commit{..} =
        defMessage
          & PT.data' .~ toBytes commitData
      f message =
        Commit
          { commitData = fromBytes $ message ^. PT.data'
          }

--------------------------------------------------------------------------------
-- Exception
--------------------------------------------------------------------------------

data Exception = Exception
  { exceptionError :: Text
  } deriving (Eq, Show, Generic)

instance ToJSON Exception where
  toJSON = genericToJSON $ defaultABCIOptions "exception"
instance FromJSON Exception where
  parseJSON = genericParseJSON $ defaultABCIOptions "exception"

instance Wrapped Exception where
  type Unwrapped Exception = PT.ResponseException

  _Wrapped' = iso t f
    where
      t Exception{..} =
        defMessage
          & PT.error .~ exceptionError
      f responseException =
        Exception
          { exceptionError = responseException ^. PT.error
          }
