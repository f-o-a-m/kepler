module Network.ABCI.Types.Messages.Request
  ( Request(..)

  -- * Request Message Types
  , Echo(..)
  , Flush(..)
  , Info(..)
  , Query(..)
  , BeginBlock(..)
  , CheckTx(..)
  , DeliverTx(..)
  , EndBlock(..)
  , Commit(..)
  ) where

import           Control.Lens                           (Iso', iso, traverse,
                                                         (&), (.~), (^.), (^..),
                                                         (^?), _Just, from)
import           Control.Lens.Wrapped                   (Wrapped(..))
import           Data.ByteString                        (ByteString)
import           Data.Int                               (Int64)
import           Data.ProtoLens.Message                 (Message (defMessage))
import           Data.Text                              (Text)
import           Data.Word                              (Word64)
import           GHC.Generics                           (Generic)
import           Network.ABCI.Types.Messages.FieldTypes (ConsensusParams(..),
                                                         Evidence(..), Header(..),
                                                         LastCommitInfo(..),
                                                         Timestamp(..),
                                                         ValidatorUpdate(..),
                                                         consensusParams,
                                                         evidence, header,
                                                         lastCommitInfo,
                                                         timestamp,
                                                         validatorUpdate)
import           Network.ABCI.Types.Messages.Types      (MessageType(..))
import qualified Proto.Types                            as PT
import qualified Proto.Types_Fields                     as PT


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

--------------------------------------------------------------------------------
-- Echo
--------------------------------------------------------------------------------

data Echo =
  Echo
    { echoMessage :: Text
    -- ^ A string to echo back
    } deriving (Eq, Show, Generic)

instance Wrapped Echo where
  type Unwrapped Echo = PT.RequestEcho

  _Wrapped' = iso t f
    where
      t Echo{..} =
        defMessage & PT.message .~ echoMessage
      f message =
        Echo
          { echoMessage = message ^. PT.message
          }

--------------------------------------------------------------------------------
-- Flush
--------------------------------------------------------------------------------

data Flush =
  Flush deriving (Eq, Show, Generic)

instance Wrapped Flush where
  type Unwrapped Flush = PT.RequestFlush

  _Wrapped' = iso t f
    where
      t = const defMessage
      f = const Flush

--------------------------------------------------------------------------------
-- Info
--------------------------------------------------------------------------------

data Info =
  Info
    { infoVersion      :: Text
    -- ^ The Tendermint software semantic version
    , infoBlockVersion :: Word64
    -- ^ The Tendermint Block Protocol version
    , infoP2pVersion   :: Word64
    -- ^ The Tendermint P2P Protocol version
    } deriving (Eq, Show, Generic)

instance Wrapped Info where
  type Unwrapped Info = PT.RequestInfo

  _Wrapped' = iso t f
    where
      t Info{..} =
        defMessage & PT.version .~ infoVersion
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

data SetOption =
  SetOption
    { setOptionKey   :: Text
    -- ^ Key to set
    , setOptionValue :: Text
    -- ^ Value to set for key
    } deriving (Eq, Show, Generic)

instance Wrapped SetOption where
  type Unwrapped SetOption = PT.RequestSetOption

  _Wrapped' = iso t f
    where
      t SetOption{..} =
        defMessage & PT.key .~ setOptionKey
                   & PT.value .~ setOptionValue
      f message =
        SetOption
          { setOptionKey = message ^. PT.key
          , setOptionValue = message ^. PT.value
          }

--------------------------------------------------------------------------------
-- InitChain
--------------------------------------------------------------------------------

data InitChain =
  InitChain
    { initChainTime            :: Maybe Timestamp
    -- ^ Genesis time
    , initChainChainId         :: Text
    -- ^ ID of the blockchain.
    , initChainConsensusParams :: Maybe ConsensusParams
    -- ^ Initial consensus-critical parameters.
    , initChainValidators      :: [ValidatorUpdate]
    -- ^ Initial genesis validators.
    , initChainAppState        :: ByteString
    -- ^ Serialized initial application state. Amino-encoded JSON bytes.
    } deriving (Eq, Show, Generic)

instance Wrapped InitChain where
  type Unwrapped InitChain = PT.RequestInitChain

  _Wrapped' = iso t f
    where
      t InitChain{..} =
        defMessage & PT.maybe'time .~ initChainTime ^? _Just . timestamp
                   & PT.chainId .~ initChainChainId
                   & PT.maybe'consensusParams .~ initChainConsensusParams ^? _Just . consensusParams
                   & PT.validators .~ initChainValidators ^.. traverse . validatorUpdate
                   & PT.appStateBytes .~ initChainAppState
      f message =
        InitChain
          { initChainTime = message ^? PT.maybe'time . _Just . from timestamp
          , initChainChainId = message ^. PT.chainId
          , initChainConsensusParams = message ^? PT.maybe'consensusParams . _Just . from consensusParams
          , initChainValidators = message ^.. PT.validators . traverse . from validatorUpdate
          , initChainAppState = message ^. PT.appStateBytes
          }

--------------------------------------------------------------------------------
-- Query
--------------------------------------------------------------------------------

data Query =
  Query
    { queryData   :: ByteString
    -- ^  Raw query bytes. Can be used with or in lieu of Path.
    , queryPath   :: Text
    -- ^ Path of request, like an HTTP GET path. Can be used with or in liue of Data.
    , queryHeight :: Int64
    -- ^ The block height for which you want the query
    , queryProve  :: Bool
    -- ^ Return Merkle proof with response if possible
    } deriving (Eq, Show, Generic)

instance Wrapped Query where
  type Unwrapped Query = PT.RequestQuery

  _Wrapped' = iso t f
    where
      t Query{..} =
        defMessage & PT.data' .~ queryData
                   & PT.path .~ queryPath
                   & PT.height .~ queryHeight
                   & PT.prove .~ queryProve
      f message =
        Query
          { queryData = message ^. PT.data'
          , queryPath = message ^. PT.path
          , queryHeight = message ^. PT.height
          , queryProve = message ^. PT.prove
          }

--------------------------------------------------------------------------------
-- BeginBlock
--------------------------------------------------------------------------------

data BeginBlock =
  BeginBlock
    { beginBlockHash                :: ByteString
    -- ^ The block's hash. This can be derived from the block header.
    , beginBlockHeader              :: Maybe Header
    -- ^ The block header.
    , beginBlockLastCommitInfo      :: Maybe LastCommitInfo
    -- ^ Info about the last commit, including the round, and the list of
    -- validators and which ones signed the last block.
    , beginBlockByzantineValidators :: [Evidence]
    -- ^ List of evidence of validators that acted maliciously.
    } deriving (Eq, Show, Generic)

instance Wrapped BeginBlock where
  type Unwrapped BeginBlock = PT.RequestBeginBlock

  _Wrapped' = iso t f
    where
      t BeginBlock{..} =
        defMessage & PT.hash .~ beginBlockHash
                   & PT.maybe'header .~ beginBlockHeader ^? _Just . header
                   & PT.maybe'lastCommitInfo .~ beginBlockLastCommitInfo ^? _Just . lastCommitInfo
                   & PT.byzantineValidators .~ beginBlockByzantineValidators ^.. traverse . evidence
      f message =
        BeginBlock
          { beginBlockHash = message ^. PT.hash
          , beginBlockHeader = message ^? PT.maybe'header . _Just . from header
          , beginBlockLastCommitInfo = message ^? PT.maybe'lastCommitInfo . _Just . from lastCommitInfo
          , beginBlockByzantineValidators = message ^.. PT.byzantineValidators . traverse . from evidence
          }

--------------------------------------------------------------------------------
-- CheckTx
--------------------------------------------------------------------------------

-- TODO: figure out what happened to Type CheckTxType field
data CheckTx =
  CheckTx
    { checkTxTx :: ByteString
    -- ^ The request transaction bytes
    } deriving (Eq, Show, Generic)

instance Wrapped CheckTx where
  type Unwrapped CheckTx = PT.RequestCheckTx

  _Wrapped' = iso t f
    where
      t CheckTx{..} =
        defMessage
          & PT.tx .~ checkTxTx

      f message =
        CheckTx
          { checkTxTx = message ^. PT.tx
          }

--------------------------------------------------------------------------------
-- DeliverTx
--------------------------------------------------------------------------------

data DeliverTx =
  DeliverTx
    { deliverTxTx :: ByteString
    -- ^ The request transaction bytes.
    } deriving (Eq, Show, Generic)

instance Wrapped DeliverTx where
  type Unwrapped DeliverTx = PT.RequestDeliverTx

  _Wrapped' = iso t f
    where
     t DeliverTx{..} =
       defMessage
         & PT.tx .~ deliverTxTx

     f message =
       DeliverTx
         { deliverTxTx = message ^. PT.tx
         }

--------------------------------------------------------------------------------
-- EndBlock
--------------------------------------------------------------------------------

data EndBlock =
  EndBlock
    { endBlockHeight :: Int64
    -- ^ Height of the block just executed.
    } deriving (Eq, Show, Generic)

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

commit :: Iso' Commit PT.RequestCommit
commit = iso t f
  where
    t Commit =
      defMessage

    f requestCommit =
      Commit
