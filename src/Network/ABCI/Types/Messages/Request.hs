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
import           Data.ByteString                        (ByteString)
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
import qualified Proto.Types                            as PT
import qualified Proto.Types_Fields                     as PT
import           Debug.Trace                            (traceShow)
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

-- | Translates the unsafe auto-generated 'Proto.Request' to a type-safe
--   'Request GADT so users can safely pattern-match on it
--   (ie: the compiler will warn if any case is not covered)
--
--   Note that we need to use a rank-n-types continuation since the
--   'Request' GADT carries a phantom-type 'MsgType' "tag" and Haskell
--   does not allow a polymorphic return type on a "normal" function
--   (only those belonging to a type classes)
withProto
  :: PT.Request
  -> (forall (t :: MessageType). Maybe (Request t) -> a)
  -> a
withProto r f
  | Just echo       <- r ^. PT.maybe'echo       = f (Just (RequestEcho $ echo ^. _Unwrapped'))
  | Just flush      <- r ^. PT.maybe'flush      = f (Just (RequestFlush $ flush ^. _Unwrapped'))
  | Just info       <- r ^. PT.maybe'info       = f (Just (RequestInfo $ info ^. _Unwrapped'))
  | Just setOption  <- r ^. PT.maybe'setOption  = f (Just (RequestSetOption $ setOption ^. _Unwrapped'))
  | Just initChain  <- r ^. PT.maybe'initChain  = f (Just (RequestInitChain $ initChain ^. _Unwrapped'))
  | Just query      <- r ^. PT.maybe'query      = f (Just (RequestQuery $ query ^. _Unwrapped'))
  | Just beginBlock <- r ^. PT.maybe'beginBlock = f (Just (RequestBeginBlock $ beginBlock ^. _Unwrapped'))
  | Just requestTx  <- r ^. PT.maybe'checkTx    = f (Just (RequestCheckTx $ requestTx ^. _Unwrapped'))
  | Just deliverTx  <- r ^. PT.maybe'deliverTx  = f (Just (RequestDeliverTx $ deliverTx ^. _Unwrapped'))
  | Just endBlock   <- r ^. PT.maybe'endBlock   = f (Just (RequestEndBlock $ endBlock ^. _Unwrapped'))
  | Just commit     <- r ^. PT.maybe'commit     = f (Just (RequestCommit $ commit ^. _Unwrapped'))
  | otherwise                                   = traceShow r $ f Nothing

--------------------------------------------------------------------------------
-- Echo
--------------------------------------------------------------------------------

data Echo = Echo
  { echoMessage :: Text
  -- ^ A string to echo back
  } deriving (Eq, Show, Generic)

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
  , initChainAppState        :: ByteString
  -- ^ Serialized initial application state. Amino-encoded JSON bytes.
  } deriving (Eq, Show, Generic)

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
          & PT.appStateBytes .~ initChainAppState
      f message =
        InitChain
          { initChainTime = message ^? PT.maybe'time . _Just . _Unwrapped'
          , initChainChainId = message ^. PT.chainId
          , initChainConsensusParams = message ^? PT.maybe'consensusParams . _Just . _Unwrapped'
          , initChainValidators = message ^.. PT.validators . traverse . _Unwrapped'
          , initChainAppState = message ^. PT.appStateBytes
          }

--------------------------------------------------------------------------------
-- Query
--------------------------------------------------------------------------------

data Query = Query
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
        defMessage
          & PT.data' .~ queryData
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

data BeginBlock = BeginBlock
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
        defMessage
          & PT.hash .~ beginBlockHash
          & PT.maybe'header .~ beginBlockHeader ^? _Just . _Wrapped'
          & PT.maybe'lastCommitInfo .~ beginBlockLastCommitInfo ^? _Just . _Wrapped'
          & PT.byzantineValidators .~ beginBlockByzantineValidators ^.. traverse . _Wrapped'
      f message =
        BeginBlock
          { beginBlockHash = message ^. PT.hash
          , beginBlockHeader = message ^? PT.maybe'header . _Just . _Unwrapped'
          , beginBlockLastCommitInfo = message ^? PT.maybe'lastCommitInfo . _Just . _Unwrapped'
          , beginBlockByzantineValidators = message ^.. PT.byzantineValidators . traverse . _Unwrapped'
          }

--------------------------------------------------------------------------------
-- CheckTx
--------------------------------------------------------------------------------

-- TODO: figure out what happened to Type CheckTxType field
data CheckTx = CheckTx
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

data DeliverTx = DeliverTx
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

data EndBlock = EndBlock
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

instance Wrapped Commit where
  type Unwrapped Commit = PT.RequestCommit

  _Wrapped' = iso t f
    where
      t Commit =
        defMessage

      f _ =
        Commit
