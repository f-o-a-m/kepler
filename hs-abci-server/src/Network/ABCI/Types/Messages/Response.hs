{-# LANGUAGE TemplateHaskell #-}

module Network.ABCI.Types.Messages.Response where

import           Control.Lens                           (iso, traverse, (&),
                                                         (.~), (?~), (^.),
                                                         (^..), (^?), _Just)
import           Control.Lens.TH                        (makeLenses, makePrisms)
import           Control.Lens.Wrapped                   (Wrapped (..),
                                                         _Unwrapped')
import           Data.ByteString                        (ByteString)
import           Data.Default.Class                     (Default (..))
import           Data.Int                               (Int64)
import           Data.ProtoLens.Message                 (Message (defMessage))
import           Data.ProtoLens.Prism                   (( # ))
import           Data.Text                              (Text)
import           Data.Word                              (Word32, Word64)
import           GHC.Generics                           (Generic)
import           Network.ABCI.Types.Messages.FieldTypes (ConsensusParams, Event,
                                                         Proof, ValidatorUpdate)
import           Network.ABCI.Types.Messages.Types      (MessageType (..))
import qualified Proto.Types                            as PT
import qualified Proto.Types_Fields                     as PT

--------------------------------------------------------------------------------
-- Echo
--------------------------------------------------------------------------------

data Echo = Echo
  { echoMessage :: Text
  -- ^ The input string
  } deriving (Eq, Show, Generic)

makeLenses ''Echo

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

instance Default Echo where
  def = defMessage ^. _Unwrapped'

--------------------------------------------------------------------------------
-- Flush
--------------------------------------------------------------------------------

data Flush =
  Flush deriving (Eq, Show, Generic)

instance Wrapped Flush where
  type Unwrapped Flush = PT.ResponseFlush

  _Wrapped' = iso t f
    where
      t Flush =
        defMessage
      f _ =
        Flush

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
  , infoAppVersion       :: Word64
  -- ^ The application protocol version
  , infoLastBlockHeight  :: Int64
  -- ^  Latest block for which the app has called Commit
  , infoLastBlockAppHash :: ByteString
  -- ^  Latest result of Commit
  } deriving (Eq, Show, Generic)

makeLenses ''Info

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
        & PT.lastBlockAppHash .~ infoLastBlockAppHash
     f message =
       Info
         { infoData = message ^. PT.data'
         , infoVersion = message ^. PT.version
         , infoAppVersion = message ^. PT.appVersion
         , infoLastBlockHeight = message ^. PT.lastBlockHeight
         , infoLastBlockAppHash = message ^. PT.lastBlockAppHash
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

makeLenses ''SetOption

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

makeLenses ''InitChain

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

makeLenses ''Query

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
          & PT.key .~ queryKey
          & PT.value .~ queryValue
          & PT.maybe'proof .~ queryProof ^? _Just . _Wrapped'
          & PT.height .~ queryHeight
          & PT.codespace .~ queryCodespace
      f message =
        Query
          { queryCode = message ^. PT.code
          , queryLog = message ^. PT.log
          , queryInfo = message ^. PT.info
          , queryIndex = message ^. PT.index
          , queryKey = message ^. PT.key
          , queryValue = message ^. PT.value
          , queryProof = message ^? PT.maybe'proof . _Just . _Unwrapped'
          , queryHeight = message ^. PT.height
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

makeLenses ''BeginBlock

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

instance Default BeginBlock where
  def = defMessage ^. _Unwrapped'

--------------------------------------------------------------------------------
-- CheckTx
--------------------------------------------------------------------------------

data CheckTx = CheckTx
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
  , checkTxEvents    :: [Event]
  -- ^ Events
  , checkTxCodespace :: Text
  -- ^ Namespace for the Code.
  } deriving (Eq, Show, Generic)

makeLenses ''CheckTx

instance Wrapped CheckTx where
  type Unwrapped CheckTx = PT.ResponseCheckTx

  _Wrapped' = iso t f
    where
      t CheckTx{..} =
        defMessage
          & PT.code .~ checkTxCode
          & PT.data' .~ checkTxData
          & PT.log .~ checkTxLog
          & PT.info .~ checkTxInfo
          & PT.gasWanted .~ checkTxGasWanted
          & PT.gasUsed .~ checkTxGasUsed
          & PT.events .~ checkTxEvents ^.. traverse . _Wrapped'
          & PT.codespace .~ checkTxCodespace
      f message =
        CheckTx
          { checkTxCode = message ^. PT.code
          , checkTxData = message ^. PT.data'
          , checkTxLog = message ^. PT.log
          , checkTxInfo = message ^. PT.info
          , checkTxGasWanted = message ^. PT.gasWanted
          , checkTxGasUsed = message ^. PT.gasUsed
          , checkTxEvents = message ^.. PT.events . traverse . _Unwrapped'
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
  , deliverTxEvents    :: [Event]
  -- ^ Events
  , deliverTxCodespace :: Text
  -- ^ Namespace for the Code.
  } deriving (Eq, Show, Generic)

makeLenses ''DeliverTx

instance Wrapped DeliverTx where
  type Unwrapped DeliverTx = PT.ResponseDeliverTx

  _Wrapped' = iso t f
    where
      t DeliverTx{..} =
        defMessage
          & PT.code .~ deliverTxCode
          & PT.data' .~ deliverTxData
          & PT.log .~ deliverTxLog
          & PT.info .~ deliverTxInfo
          & PT.gasWanted .~ deliverTxGasWanted
          & PT.gasUsed .~ deliverTxGasUsed
          & PT.events .~ deliverTxEvents ^.. traverse . _Wrapped'
          & PT.codespace .~ deliverTxCodespace
      f responseDeliverTx =
        DeliverTx
          { deliverTxCode = responseDeliverTx ^. PT.code
          , deliverTxData = responseDeliverTx ^. PT.data'
          , deliverTxLog = responseDeliverTx ^. PT.log
          , deliverTxInfo = responseDeliverTx ^. PT.info
          , deliverTxGasWanted = responseDeliverTx ^. PT.gasWanted
          , deliverTxGasUsed = responseDeliverTx ^. PT.gasUsed
          , deliverTxEvents = responseDeliverTx ^.. PT.events . traverse . _Unwrapped'
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

makeLenses ''BeginBlock

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

instance Default EndBlock where
  def = defMessage ^. _Unwrapped'

--------------------------------------------------------------------------------
-- Commit
--------------------------------------------------------------------------------

data Commit = Commit
  { commitData :: ByteString
  -- ^ The Merkle root hash of the application state
  } deriving (Eq, Show, Generic)

makeLenses ''Commit

instance Wrapped Commit where
  type Unwrapped Commit = PT.ResponseCommit

  _Wrapped' = iso t f
    where
      t Commit{..} =
        defMessage
          & PT.data' .~ commitData
      f message =
        Commit
          { commitData = message ^. PT.data'
          }

instance Default Commit where
  def = defMessage ^. _Unwrapped'

--------------------------------------------------------------------------------
-- Exception
--------------------------------------------------------------------------------

data Exception = Exception
  { exceptionError :: Text
  } deriving (Eq, Show, Generic)

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

makePrisms ''Response

instance Default (Response 'MTEcho) where
  def = ResponseEcho def

instance Default (Response 'MTFlush) where
  def = ResponseFlush def

instance Default (Response 'MTInfo) where
  def = ResponseInfo def

instance Default (Response 'MTSetOption) where
  def = ResponseSetOption def

instance Default (Response 'MTInitChain) where
  def = ResponseInitChain def

instance Default (Response 'MTQuery) where
  def = ResponseQuery def

instance Default (Response 'MTBeginBlock) where
  def = ResponseBeginBlock def

instance Default (Response 'MTCheckTx) where
  def = ResponseCheckTx def

instance Default (Response 'MTDeliverTx) where
  def = ResponseDeliverTx def

instance Default (Response 'MTEndBlock) where
  def = ResponseEndBlock def

instance Default (Response 'MTCommit) where
  def = ResponseCommit def

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
