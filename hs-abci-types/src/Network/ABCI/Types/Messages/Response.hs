{-# LANGUAGE TemplateHaskell #-}

module Network.ABCI.Types.Messages.Response where

import           Control.Lens                           (iso, traverse, (&),
                                                         (.~), (^.), (^..),
                                                         (^?), _Just)
import           Control.Lens.Wrapped                   (Wrapped (..),
                                                         _Unwrapped')
import           Data.Aeson                             (FromJSON (..),
                                                         ToJSON (..),
                                                         genericParseJSON,
                                                         genericToJSON,
                                                         withObject, (.!=),
                                                         (.:), (.:?))
import           Data.ByteArray.Base64String            (Base64String)
import qualified Data.ByteArray.Base64String            as Base64
import           Data.Default.Class                     (Default (..))
import           Data.ProtoLens.Message                 (Message (defMessage))
import           Data.Text                              (Text)
import           Data.Word                              (Word32)
import           GHC.Generics                           (Generic)
import           Network.ABCI.Types.Messages.Common     (defaultABCIOptions,
                                                         makeABCILenses)
import           Network.ABCI.Types.Messages.FieldTypes (ConsensusParams, Event,
                                                         Proof, ValidatorUpdate,
                                                         WrappedInt64 (..),
                                                         WrappedWord64 (..))
import qualified Proto.Types                            as PT
import qualified Proto.Types_Fields                     as PT

--------------------------------------------------------------------------------
-- Echo
--------------------------------------------------------------------------------

data Echo = Echo
  { echoMessage :: Text
  -- ^ The input string
  } deriving (Eq, Show, Generic)

makeABCILenses ''Echo

instance ToJSON Echo where
  toJSON = genericToJSON $ defaultABCIOptions "echo"
instance FromJSON Echo where
  parseJSON = genericParseJSON $ defaultABCIOptions "echo"


instance Wrapped Echo where
  type Unwrapped Echo = PT.ResponseEcho

  _Wrapped' = iso t f
   where
    t Echo {..} = defMessage & PT.message .~ echoMessage
    f message = Echo { echoMessage = message ^. PT.message }

instance Default Echo where
  def = defMessage ^. _Unwrapped'

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
    t Flush = defMessage
    f _ = Flush

instance Default Flush where
  def = defMessage ^. _Unwrapped'

--------------------------------------------------------------------------------
-- Info
--------------------------------------------------------------------------------

data Info = Info
  { infoData             :: Text
  -- ^ Some arbitrary information
  , infoVersion          :: Text
  -- ^ The application software semantic version
  , infoAppVersion       :: WrappedWord64
  -- ^ The application protocol version
  } deriving (Eq, Show, Generic)


makeABCILenses ''Info

instance ToJSON Info where
  toJSON = genericToJSON $ defaultABCIOptions "info"
instance FromJSON Info where
  parseJSON = genericParseJSON $ defaultABCIOptions "info"


instance Wrapped Info where
  type Unwrapped Info = PT.ResponseInfo

  _Wrapped' = iso t f
   where
    t Info {..} =
      defMessage
        & PT.data' .~ infoData
        & PT.version .~ infoVersion
        & PT.appVersion .~ unwrapWord64 infoAppVersion
    f message = Info
      { infoData             = message ^. PT.data'
      , infoVersion          = message ^. PT.version
      , infoAppVersion       = WrappedWord64 $ message ^. PT.appVersion
      }

instance Default Info where
  def = defMessage ^. _Unwrapped'

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


makeABCILenses ''SetOption

instance ToJSON SetOption where
  toJSON = genericToJSON $ defaultABCIOptions "setOption"
instance FromJSON SetOption where
  parseJSON = genericParseJSON $ defaultABCIOptions "setOption"


instance Wrapped SetOption where
  type Unwrapped SetOption = PT.ResponseSetOption

  _Wrapped' = iso t f
   where
    t SetOption {..} =
      defMessage
        & PT.code .~ setOptionCode
        & PT.log .~ setOptionLog
        & PT.info .~ setOptionInfo
    f message = SetOption { setOptionCode = message ^. PT.code
                          , setOptionLog  = message ^. PT.log
                          , setOptionInfo = message ^. PT.info
                          }

instance Default SetOption where
  def = defMessage ^. _Unwrapped'

--------------------------------------------------------------------------------
-- InitChain
--------------------------------------------------------------------------------

data InitChain = InitChain
  { initChainConsensusParams :: Maybe ConsensusParams
  -- ^ Initial consensus-critical parameters.
  , initChainValidators      :: [ValidatorUpdate]
  -- ^ Initial validator set (if non empty).
  } deriving (Eq, Show, Generic)


makeABCILenses ''InitChain

instance ToJSON InitChain where
  toJSON = genericToJSON $ defaultABCIOptions "initChain"
instance FromJSON InitChain where
  parseJSON = withObject "InitChain" $ \v -> InitChain
    <$> v .:? "consensusParams"
    <*> v .:? "validators" .!= []


instance Wrapped InitChain where
  type Unwrapped InitChain = PT.ResponseInitChain

  _Wrapped' = iso t f
   where
    t InitChain {..} =
      defMessage
        & PT.maybe'consensusParams .~ initChainConsensusParams ^? _Just . _Wrapped'
        & PT.validators .~ initChainValidators ^.. traverse . _Wrapped'
    f message = InitChain
      { initChainConsensusParams = message ^? PT.maybe'consensusParams . _Just . _Unwrapped'
      , initChainValidators = message ^.. PT.validators . traverse . _Unwrapped'
      }

instance Default InitChain where
  def = defMessage ^. _Unwrapped'

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
  , queryIndex     :: WrappedInt64
  -- ^ The index of the key in the tree.
  , queryKey       :: Base64String
  -- ^ The key of the matching data.
  , queryValue     :: Base64String
  -- ^ The value of the matching data.
  , queryProof     :: Maybe Proof
  -- ^ Serialized proof for the value data, if requested, to be verified against
  -- the AppHash for the given Height.
  , queryHeight    :: WrappedInt64
  -- ^ The block height from which data was derived.
  , queryCodespace :: Text
  -- ^ Namespace for the Code.
  } deriving (Eq, Show, Generic)


makeABCILenses ''Query

instance ToJSON Query where
  toJSON = genericToJSON $ defaultABCIOptions "query"
instance FromJSON Query where
  parseJSON = genericParseJSON $ defaultABCIOptions "query"


instance Wrapped Query where
  type Unwrapped Query = PT.ResponseQuery

  _Wrapped' = iso t f
   where
    t Query {..} =
      defMessage
        & PT.code .~ queryCode
        & PT.log .~ queryLog
        & PT.info .~ queryInfo
        & PT.index .~ unwrapInt64 queryIndex
        & PT.key .~ Base64.toBytes queryKey
        & PT.value .~ Base64.toBytes queryValue
        & PT.maybe'proof .~ queryProof ^? _Just .  _Wrapped'
        & PT.height .~ unwrapInt64 queryHeight
        & PT.codespace .~ queryCodespace
    f message = Query
      { queryCode      = message ^. PT.code
      , queryLog       = message ^. PT.log
      , queryInfo      = message ^. PT.info
      , queryIndex     = WrappedInt64 $ message ^. PT.index
      , queryKey       = Base64.fromBytes $ message ^. PT.key
      , queryValue     = Base64.fromBytes $ message ^. PT.value
      , queryProof     = message ^? PT.maybe'proof . _Just . _Unwrapped'
      , queryHeight    = WrappedInt64 $ message ^. PT.height
      , queryCodespace = message ^. PT.codespace
      }

instance Default Query where
  def = defMessage ^. _Unwrapped'

--------------------------------------------------------------------------------
-- BeginBlock
--------------------------------------------------------------------------------

data BeginBlock = BeginBlock
  { beginBlockEvents :: [Event]
  -- ^ Beginning block events
  } deriving (Eq, Show, Generic)


makeABCILenses ''BeginBlock

instance ToJSON BeginBlock where
  toJSON = genericToJSON $ defaultABCIOptions "beginBlock"
instance FromJSON BeginBlock where
  parseJSON = withObject "BeginBlock" $ \v -> BeginBlock
   <$> v .:? "events" .!= []


instance Wrapped BeginBlock where
  type Unwrapped BeginBlock = PT.ResponseBeginBlock

  _Wrapped' = iso t f
   where
    t BeginBlock {..} =
      defMessage & PT.events .~ beginBlockEvents ^.. traverse . _Wrapped'
    f message = BeginBlock
      { beginBlockEvents = message ^.. PT.events . traverse . _Unwrapped'
      }

instance Default BeginBlock where
  def = defMessage ^. _Unwrapped'

--------------------------------------------------------------------------------
-- CheckTx
--------------------------------------------------------------------------------

data CheckTx = CheckTx
  { checkTxCode      :: Word32
  -- ^ Response code
  , checkTxData      :: Base64String
  -- ^ Result bytes, if any.
  , checkTxLog       :: Text
  -- ^ The output of the application's logger.
  , checkTxInfo      :: Text
  -- ^ Additional information.
  , checkTxGasWanted :: WrappedInt64
  -- ^ Amount of gas requested for transaction.
  , checkTxGasUsed   :: WrappedInt64
  -- ^ Amount of gas consumed by transaction.
  , checkTxEvents    :: [Event]
  -- ^ Events
  , checkTxCodespace :: Text
  -- ^ Namespace for the Code.
  } deriving (Eq, Show, Generic)


makeABCILenses ''CheckTx

instance ToJSON CheckTx where
  toJSON = genericToJSON $ defaultABCIOptions "checkTx"
instance FromJSON CheckTx where
  parseJSON = withObject "CheckTx" $ \v -> CheckTx
    <$> v .: "code"
    <*> v .: "data"
    <*> v .: "log"
    <*> v .: "info"
    <*> v .: "gasWanted"
    <*> v .: "gasUsed"
    <*> v .:? "events" .!= []
    <*> v .: "codespace"


instance Wrapped CheckTx where
  type Unwrapped CheckTx = PT.ResponseCheckTx

  _Wrapped' = iso t f
   where
    t CheckTx {..} =
      defMessage
        & PT.code .~ checkTxCode
        & PT.data' .~ Base64.toBytes checkTxData
        & PT.log .~ checkTxLog
        & PT.info .~ checkTxInfo
        & PT.gasWanted .~ unwrapInt64 checkTxGasWanted
        & PT.gasUsed .~ unwrapInt64 checkTxGasUsed
        & PT.events .~ checkTxEvents ^.. traverse . _Wrapped'
        & PT.codespace .~ checkTxCodespace
    f message = CheckTx
      { checkTxCode      = message ^. PT.code
      , checkTxData      = Base64.fromBytes $ message ^. PT.data'
      , checkTxLog       = message ^. PT.log
      , checkTxInfo      = message ^. PT.info
      , checkTxGasWanted = WrappedInt64 $ message ^. PT.gasWanted
      , checkTxGasUsed   = WrappedInt64 $ message ^. PT.gasUsed
      , checkTxEvents    = message ^.. PT.events . traverse . _Unwrapped'
      , checkTxCodespace = message ^. PT.codespace
      }

instance Default CheckTx where
  def = defMessage ^. _Unwrapped'

--------------------------------------------------------------------------------
-- DeliverTx
--------------------------------------------------------------------------------

data DeliverTx = DeliverTx
  { deliverTxCode      :: Word32
  -- ^ Response code.
  , deliverTxData      :: Base64String
  -- ^ Result bytes, if any.
  , deliverTxLog       :: Text
  -- ^ The output of the application's logger. May be non-deterministic.
  , deliverTxInfo      :: Text
  -- ^ Additional information.
  , deliverTxGasWanted :: WrappedInt64
  -- ^ Amount of gas requested for transaction.
  , deliverTxGasUsed   :: WrappedInt64
  -- ^ Amount of gas consumed by transaction.
  , deliverTxEvents    :: [Event]
  -- ^ Events
  , deliverTxCodespace :: Text
  -- ^ Namespace for the Code.
  } deriving (Eq, Show, Generic)


makeABCILenses ''DeliverTx

instance ToJSON DeliverTx where
  toJSON = genericToJSON $ defaultABCIOptions "deliverTx"
instance FromJSON DeliverTx where
  parseJSON = withObject "DeliverTx" $ \v -> DeliverTx
    <$> v .: "code"
    <*> v .: "data"
    <*> v .: "log"
    <*> v .: "info"
    <*> v .: "gasWanted"
    <*> v .: "gasUsed"
    <*> v .:? "events" .!= []
    <*> v .: "codespace"


instance Wrapped DeliverTx where
  type Unwrapped DeliverTx = PT.ResponseDeliverTx

  _Wrapped' = iso t f
   where
    t DeliverTx {..} =
      defMessage
        & PT.code .~ deliverTxCode
        & PT.data' .~ Base64.toBytes deliverTxData
        & PT.log .~ deliverTxLog
        & PT.info .~ deliverTxInfo
        & PT.gasWanted .~ unwrapInt64 deliverTxGasWanted
        & PT.gasUsed .~ unwrapInt64 deliverTxGasUsed
        & PT.events .~ deliverTxEvents ^.. traverse . _Wrapped'
        & PT.codespace .~ deliverTxCodespace
    f responseDeliverTx = DeliverTx
      { deliverTxCode      = responseDeliverTx ^. PT.code
      , deliverTxData      = Base64.fromBytes $ responseDeliverTx ^. PT.data'
      , deliverTxLog       = responseDeliverTx ^. PT.log
      , deliverTxInfo      = responseDeliverTx ^. PT.info
      , deliverTxGasWanted = WrappedInt64 $ responseDeliverTx ^. PT.gasWanted
      , deliverTxGasUsed   = WrappedInt64 $ responseDeliverTx ^. PT.gasUsed
      , deliverTxEvents    = responseDeliverTx ^.. PT.events . traverse . _Unwrapped'
      , deliverTxCodespace = responseDeliverTx ^. PT.codespace
      }

instance Default DeliverTx where
  def = defMessage ^. _Unwrapped'

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

makeABCILenses ''EndBlock

instance ToJSON EndBlock where
  toJSON = genericToJSON $ defaultABCIOptions "endBlock"
instance FromJSON EndBlock where
  parseJSON = withObject "EndBlock" $ \v -> EndBlock
    <$> v .:? "validatorUpdates" .!= []
    <*> v .:? "consensusParams"
    <*> v .:? "events" .!= []


instance Wrapped EndBlock where
  type Unwrapped EndBlock = PT.ResponseEndBlock

  _Wrapped' = iso t f
   where
    t EndBlock {..} =
      defMessage
        & PT.validatorUpdates .~ endBlockValidatorUpdates ^.. traverse . _Wrapped'
        & PT.maybe'consensusParamUpdates .~ endBlockConsensusParamUpdates ^? _Just . _Wrapped'
        & PT.events .~ endBlockEvents ^.. traverse . _Wrapped'
    f message = EndBlock
      { endBlockValidatorUpdates = message ^.. PT.validatorUpdates . traverse . _Unwrapped'
      , endBlockConsensusParamUpdates = message ^? PT.maybe'consensusParamUpdates . _Just . _Unwrapped'
      , endBlockEvents = message ^.. PT.events . traverse . _Unwrapped'
      }

instance Default EndBlock where
  def = defMessage ^. _Unwrapped'

--------------------------------------------------------------------------------
-- Commit
--------------------------------------------------------------------------------

data Commit = Commit
  { commitData :: Base64String
  -- ^ The Merkle root hash of the application state
  } deriving (Eq, Show, Generic)

makeABCILenses ''Commit

instance ToJSON Commit where
  toJSON = genericToJSON $ defaultABCIOptions "commit"
instance FromJSON Commit where
  parseJSON = genericParseJSON $ defaultABCIOptions "commit"

instance Wrapped Commit where
  type Unwrapped Commit = PT.ResponseCommit

  _Wrapped' = iso t f
   where
    t Commit {..} = defMessage & PT.data' .~ Base64.toBytes commitData
    f message = Commit { commitData = Base64.fromBytes $ message ^. PT.data' }

instance Default Commit where
  def = defMessage ^. _Unwrapped'

--------------------------------------------------------------------------------
-- Exception
--------------------------------------------------------------------------------

data Exception = Exception
  { exceptionError :: Text
  } deriving (Eq, Show, Generic)


makeABCILenses ''Exception

instance ToJSON Exception where
  toJSON = genericToJSON $ defaultABCIOptions "exception"
instance FromJSON Exception where
  parseJSON = genericParseJSON $ defaultABCIOptions "exception"

instance Default Exception where
  def = defMessage ^. _Unwrapped'

instance Wrapped Exception where
  type Unwrapped Exception = PT.ResponseException

  _Wrapped' = iso t f
   where
    t Exception {..} = defMessage & PT.error .~ exceptionError
    f responseException =
      Exception { exceptionError = responseException ^. PT.error }
