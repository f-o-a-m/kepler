module Network.ABCI.Types.Messages.Request
  ( Request(..)
  , withProto

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

  -- * ReExports
  , MessageType(..)
  ) where

import           Control.Lens                           (iso, traverse, (&),
                                                         (.~), (^.), (^..),
                                                         (^?), _Just)
import           Control.Lens.Wrapped                   (Wrapped (..),
                                                         _Unwrapped')
import           Data.Aeson                             (FromJSON (..),
                                                         ToJSON (..),
                                                         genericParseJSON,
                                                         genericToJSON)
import           Data.ByteArray.HexString
                                                                                   (HexString,
                                                                                   fromBytes,
                                                                                   toBytes)
import           Data.Int                               (Int64)
import           Data.ProtoLens.Message                 (Message (defMessage))
import           Data.Text                              (Text)
import           Data.Word                              (Word64)
import           GHC.Generics                           (Generic)
import           Network.ABCI.Types.Messages.FieldTypes (ConsensusParams (..),
                                                         Evidence (..),
                                                         Header (..),
                                                         LastCommitInfo (..),
                                                         Timestamp (..),
                                                         ValidatorUpdate (..))
import           Network.ABCI.Types.Messages.Types      (MessageType (..))
import Network.ABCI.Types.Messages.Common (defaultABCIOptions)
import qualified Proto.Types                            as PT
import qualified Proto.Types_Fields                     as PT
--------------------------------------------------------------------------------
-- Request
--------------------------------------------------------------------------------

data Request (m :: MessageType) :: * where
  RequestEcho :: Echo -> Request 'MTEcho
  RequestFlush :: Flush -> Request 'MTFlush
  RequestInfo :: Info -> Request 'MTInfo
  RequestSetOption :: SetOption -> Request 'MTSetOption
  RequestInitChain :: InitChain -> Request 'MTInitChain
  RequestQuery :: Query -> Request 'MTQuery
  RequestBeginBlock :: BeginBlock -> Request 'MTBeginBlock
  RequestCheckTx :: CheckTx -> Request 'MTCheckTx
  RequestDeliverTx :: DeliverTx -> Request 'MTDeliverTx
  RequestEndBlock :: EndBlock -> Request 'MTEndBlock
  RequestCommit :: Commit -> Request 'MTCommit

instance ToJSON (Request (t :: MessageType)) where
  toJSON (RequestEcho v)       = toJSON v
  toJSON (RequestFlush v)      = toJSON v
  toJSON (RequestInfo v)       = toJSON v
  toJSON (RequestSetOption v)  = toJSON v
  toJSON (RequestInitChain v)  = toJSON v
  toJSON (RequestQuery v)      = toJSON v
  toJSON (RequestBeginBlock v) = toJSON v
  toJSON (RequestCheckTx v)    = toJSON v
  toJSON (RequestDeliverTx v)  = toJSON v
  toJSON (RequestEndBlock v)   = toJSON v
  toJSON (RequestCommit v)     = toJSON v

instance FromJSON (Request (t :: MessageType)) where
  parseJSON = undefined

withProto
  :: (forall (t :: MessageType). Request t -> a)
  -> PT.Request'Value
  -> a
withProto f value = case value of
  PT.Request'Echo echo -> f $ RequestEcho $ echo ^. _Unwrapped'
  PT.Request'Flush flush -> f $ RequestFlush $ flush ^. _Unwrapped'
  PT.Request'Info info -> f $ RequestInfo $ info ^. _Unwrapped'
  PT.Request'SetOption setOption -> f $ RequestSetOption $ setOption ^. _Unwrapped'
  PT.Request'InitChain initChain -> f $ RequestInitChain $ initChain ^. _Unwrapped'
  PT.Request'Query query -> f $ RequestQuery $ query ^. _Unwrapped'
  PT.Request'BeginBlock beginBlock -> f $ RequestBeginBlock $ beginBlock ^. _Unwrapped'
  PT.Request'CheckTx checkTx -> f $ RequestCheckTx $ checkTx ^. _Unwrapped'
  PT.Request'DeliverTx deliverTx -> f $ RequestDeliverTx $ deliverTx ^. _Unwrapped'
  PT.Request'EndBlock endBlock -> f $ RequestEndBlock $ endBlock ^. _Unwrapped'
  PT.Request'Commit commit -> f $ RequestCommit $ commit ^. _Unwrapped'

--------------------------------------------------------------------------------
-- Echo
--------------------------------------------------------------------------------

data Echo = Echo
  { echoMessage :: Text
  -- ^ A string to echo back
  } deriving (Eq, Show, Generic)

instance ToJSON Echo where
  toJSON = genericToJSON $ defaultABCIOptions "echo"
instance FromJSON Echo where
  parseJSON = genericParseJSON $ defaultABCIOptions "echo"

instance Wrapped Echo where
  type Unwrapped Echo = PT.RequestEcho

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
  type Unwrapped Flush = PT.RequestFlush

  _Wrapped' = iso t f
    where
      t = const defMessage
      f = const Flush

--------------------------------------------------------------------------------
-- Info
--------------------------------------------------------------------------------

data Info = Info
  { infoVersion      :: Text
  -- ^ The Tendermint software semantic version
  , infoBlockVersion :: Word64
  -- ^ The Tendermint Block Protocol version
  , infoP2pVersion   :: Word64
  -- ^ The Tendermint P2P Protocol version
  } deriving (Eq, Show, Generic)

instance ToJSON Info where
  toJSON = genericToJSON $ defaultABCIOptions "info"
instance FromJSON Info where
  parseJSON = genericParseJSON $ defaultABCIOptions "info"

instance Wrapped Info where
  type Unwrapped Info = PT.RequestInfo

  _Wrapped' = iso t f
    where
      t Info{..} =
        defMessage
          & PT.version .~ infoVersion
          & PT.blockVersion .~ infoBlockVersion
          & PT.p2pVersion .~ infoP2pVersion
      f message =
        Info
          { infoVersion = message ^. PT.version
          , infoBlockVersion = message ^. PT.blockVersion
          , infoP2pVersion = message ^. PT.p2pVersion
          }

--------------------------------------------------------------------------------
-- SetOption
--------------------------------------------------------------------------------

data SetOption = SetOption
  { setOptionKey   :: Text
  -- ^ Key to set
  , setOptionValue :: Text
  -- ^ Value to set for key
  } deriving (Eq, Show, Generic)

instance ToJSON SetOption where
  toJSON = genericToJSON $ defaultABCIOptions "setOption"
instance FromJSON SetOption where
  parseJSON = genericParseJSON $ defaultABCIOptions "setOption"

instance Wrapped SetOption where
  type Unwrapped SetOption = PT.RequestSetOption

  _Wrapped' = iso t f
    where
      t SetOption{..} =
        defMessage
          & PT.key .~ setOptionKey
          & PT.value .~ setOptionValue
      f message =
        SetOption
          { setOptionKey = message ^. PT.key
          , setOptionValue = message ^. PT.value
          }

--------------------------------------------------------------------------------
-- InitChain
--------------------------------------------------------------------------------

data InitChain = InitChain
  { initChainTime            :: Maybe Timestamp
  -- ^ Genesis time
  , initChainChainId         :: Text
  -- ^ ID of the blockchain.
  , initChainConsensusParams :: Maybe ConsensusParams
  -- ^ Initial consensus-critical parameters.
  , initChainValidators      :: [ValidatorUpdate]
  -- ^ Initial genesis validators.
  , initChainAppState        :: HexString
  -- ^ Serialized initial application state. Amino-encoded JSON bytes.
  } deriving (Eq, Show, Generic)

instance ToJSON InitChain where
  toJSON = genericToJSON $ defaultABCIOptions "initChain"
instance FromJSON InitChain where
  parseJSON = genericParseJSON $ defaultABCIOptions "initChain"

instance Wrapped InitChain where
  type Unwrapped InitChain = PT.RequestInitChain

  _Wrapped' = iso t f
    where
      t InitChain{..} =
        defMessage
          & PT.maybe'time .~ initChainTime ^? _Just . _Wrapped'
          & PT.chainId .~ initChainChainId
          & PT.maybe'consensusParams .~ initChainConsensusParams ^? _Just . _Wrapped'
          & PT.validators .~ initChainValidators ^.. traverse . _Wrapped'
          & PT.appStateBytes .~ toBytes initChainAppState
      f message =
        InitChain
          { initChainTime = message ^? PT.maybe'time . _Just . _Unwrapped'
          , initChainChainId = message ^. PT.chainId
          , initChainConsensusParams = message ^? PT.maybe'consensusParams . _Just . _Unwrapped'
          , initChainValidators = message ^.. PT.validators . traverse . _Unwrapped'
          , initChainAppState = fromBytes $ message ^. PT.appStateBytes
          }

--------------------------------------------------------------------------------
-- Query
--------------------------------------------------------------------------------

data Query = Query
  { queryData   :: HexString
  -- ^  Raw query bytes. Can be used with or in lieu of Path.
  , queryPath   :: Text
  -- ^ Path of request, like an HTTP GET path. Can be used with or in liue of Data.
  , queryHeight :: Int64
  -- ^ The block height for which you want the query
  , queryProve  :: Bool
  -- ^ Return Merkle proof with response if possible
  } deriving (Eq, Show, Generic)

instance ToJSON Query where
  toJSON = genericToJSON $ defaultABCIOptions "query"
instance FromJSON Query where
  parseJSON = genericParseJSON $ defaultABCIOptions "query"

instance Wrapped Query where
  type Unwrapped Query = PT.RequestQuery

  _Wrapped' = iso t f
    where
      t Query{..} =
        defMessage
          & PT.data' .~ toBytes queryData
          & PT.path .~ queryPath
          & PT.height .~ queryHeight
          & PT.prove .~ queryProve
      f message =
        Query
          { queryData = fromBytes $ message ^. PT.data'
          , queryPath = message ^. PT.path
          , queryHeight = message ^. PT.height
          , queryProve = message ^. PT.prove
          }

--------------------------------------------------------------------------------
-- BeginBlock
--------------------------------------------------------------------------------

data BeginBlock = BeginBlock
  { beginBlockHash                :: HexString
  -- ^ The block's hash. This can be derived from the block header.
  , beginBlockHeader              :: Maybe Header
  -- ^ The block header.
  , beginBlockLastCommitInfo      :: Maybe LastCommitInfo
  -- ^ Info about the last commit, including the round, and the list of
  -- validators and which ones signed the last block.
  , beginBlockByzantineValidators :: [Evidence]
  -- ^ List of evidence of validators that acted maliciously.
  } deriving (Eq, Show, Generic)

instance ToJSON BeginBlock where
  toJSON = genericToJSON $ defaultABCIOptions "beginBlock"
instance FromJSON BeginBlock where
  parseJSON = genericParseJSON $ defaultABCIOptions "beginBlock"

instance Wrapped BeginBlock where
  type Unwrapped BeginBlock = PT.RequestBeginBlock

  _Wrapped' = iso t f
    where
      t BeginBlock{..} =
        defMessage
          & PT.hash .~ toBytes beginBlockHash
          & PT.maybe'header .~ beginBlockHeader ^? _Just . _Wrapped'
          & PT.maybe'lastCommitInfo .~ beginBlockLastCommitInfo ^? _Just . _Wrapped'
          & PT.byzantineValidators .~ beginBlockByzantineValidators ^.. traverse . _Wrapped'
      f message =
        BeginBlock
          { beginBlockHash = fromBytes $  message ^. PT.hash
          , beginBlockHeader = message ^? PT.maybe'header . _Just . _Unwrapped'
          , beginBlockLastCommitInfo = message ^? PT.maybe'lastCommitInfo . _Just . _Unwrapped'
          , beginBlockByzantineValidators = message ^.. PT.byzantineValidators . traverse . _Unwrapped'
          }

--------------------------------------------------------------------------------
-- CheckTx
--------------------------------------------------------------------------------

-- TODO: figure out what happened to Type CheckTxType field
data CheckTx = CheckTx
  { checkTxTx :: HexString
  -- ^ The request transaction bytes
  } deriving (Eq, Show, Generic)

instance ToJSON CheckTx where
  toJSON = genericToJSON $ defaultABCIOptions "checkTx"
instance FromJSON CheckTx where
  parseJSON = genericParseJSON $ defaultABCIOptions "checkTx"

instance Wrapped CheckTx where
  type Unwrapped CheckTx = PT.RequestCheckTx

  _Wrapped' = iso t f
    where
      t CheckTx{..} =
        defMessage
          & PT.tx .~ toBytes checkTxTx

      f message =
        CheckTx
          { checkTxTx = fromBytes $ message ^. PT.tx
          }

--------------------------------------------------------------------------------
-- DeliverTx
--------------------------------------------------------------------------------

data DeliverTx = DeliverTx
  { deliverTxTx :: HexString
  -- ^ The request transaction bytes.
  } deriving (Eq, Show, Generic)

instance ToJSON DeliverTx where
  toJSON = genericToJSON $ defaultABCIOptions "deliverTx"
instance FromJSON DeliverTx where
  parseJSON = genericParseJSON $ defaultABCIOptions "deliverTx"

instance Wrapped DeliverTx where
  type Unwrapped DeliverTx = PT.RequestDeliverTx

  _Wrapped' = iso t f
    where
     t DeliverTx{..} =
       defMessage
         & PT.tx .~ toBytes deliverTxTx

     f message =
       DeliverTx
         { deliverTxTx = fromBytes $ message ^. PT.tx
         }

--------------------------------------------------------------------------------
-- EndBlock
--------------------------------------------------------------------------------

data EndBlock = EndBlock
  { endBlockHeight :: Int64
  -- ^ Height of the block just executed.
  } deriving (Eq, Show, Generic)

instance ToJSON EndBlock where
  toJSON = genericToJSON $ defaultABCIOptions "endBlock"
instance FromJSON EndBlock where
  parseJSON = genericParseJSON $ defaultABCIOptions "endBlock"

instance Wrapped EndBlock where
  type Unwrapped EndBlock = PT.RequestEndBlock

  _Wrapped' = iso t f
    where
      t EndBlock{..} =
        defMessage
          & PT.height .~ endBlockHeight

      f message =
        EndBlock
          { endBlockHeight = message ^. PT.height
          }

--------------------------------------------------------------------------------
-- Commit
--------------------------------------------------------------------------------

data Commit =
  Commit deriving (Eq, Show, Generic)

instance ToJSON Commit where
  toJSON = genericToJSON $ defaultABCIOptions "commit"
instance FromJSON Commit where
  parseJSON = genericParseJSON $ defaultABCIOptions "commit"

instance Wrapped Commit where
  type Unwrapped Commit = PT.RequestCommit

  _Wrapped' = iso t f
    where
      t Commit =
        defMessage

      f _ =
        Commit
