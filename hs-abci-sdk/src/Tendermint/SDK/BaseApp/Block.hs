module Tendermint.SDK.BaseApp.Block
  ( BlockEffs
  , evalBeginBlockHandler
  , evalEndBlockHandler
  , EndBlockResult (..)
  , defaultBeginBlocker
  , defaultEndBlocker
  ) where

import           Control.Monad.IO.Class                    (liftIO)
import qualified Data.Aeson                                as A
import           Data.ByteString.Lazy                      (toStrict)
import           Data.IORef                                (newIORef)
import           Data.Proxy                                (Proxy (Proxy))
import           GHC.Generics                              (Generic)
import           Network.ABCI.Types.Messages.FieldTypes    (ConsensusParams,
                                                            ValidatorUpdate)
import qualified Network.ABCI.Types.Messages.Request       as Req
import qualified Network.ABCI.Types.Messages.Response      as Resp
import           Polysemy                                  (Embed, Members, Sem)
import           Polysemy.Tagged                           (Tagged (..))
import qualified Tendermint.SDK.BaseApp.Store              as Store
import qualified Tendermint.SDK.BaseApp.Transaction.Cache  as Cache
import           Tendermint.SDK.BaseApp.Transaction.Effect (TxEffs, runTx)
import           Tendermint.SDK.BaseApp.Transaction.Types  (TransactionContext (..))
import           Tendermint.SDK.Codec                      (HasCodec (..))
import           Tendermint.SDK.Types.Effects              ((:&))
import           Tendermint.SDK.Types.TxResult             (TxResult (_txResultEvents))



data BlockContext = BlockContext TransactionContext

newBlockContext :: IO BlockContext
newBlockContext = do
  initialCache <- newIORef Cache.emptyCache
  gasRemaining <- newIORef 0
  es <- newIORef []
  pure . BlockContext $
    TransactionContext
      { gasRemaining
      , txRequiresGas = False
      , storeCache = initialCache
      , events = es
      }

type BlockEffs = TxEffs

----------------------

evalBeginBlockHandler
  :: Members [Embed IO, Tagged 'Store.Consensus Store.ReadStore, Tagged 'Store.Consensus Store.WriteStore] r
  => Sem (BlockEffs :& r) ()
  -> Sem r Resp.BeginBlock
evalBeginBlockHandler action = do
  (BlockContext txCtx)  <- liftIO newBlockContext
  (_, txres, cache)  <- runTx (Proxy @'Store.Consensus) txCtx action
  case cache of
    Just c -> do
      Cache.writeCache c
      pure $ Resp.BeginBlock (_txResultEvents txres)
    _ -> pure $ Resp.BeginBlock []


defaultBeginBlocker :: Req.BeginBlock -> Sem r ()
defaultBeginBlocker = const $ pure ()

----------------------

data EndBlockResult = EndBlockResult [ValidatorUpdate] (Maybe ConsensusParams)
    deriving (Generic)

instance A.ToJSON EndBlockResult
instance A.FromJSON EndBlockResult
instance HasCodec EndBlockResult where
  encode = toStrict . A.encode
  decode s = maybe (Left "EndBlockResult failure") Right (A.decodeStrict s)


evalEndBlockHandler
  :: Members [Embed IO, Tagged 'Store.Consensus Store.ReadStore, Tagged 'Store.Consensus Store.WriteStore] r
  => Sem (BlockEffs :& r) EndBlockResult
  -> Sem r Resp.EndBlock
evalEndBlockHandler action = do
  (BlockContext txCtx) <- liftIO newBlockContext
  (res, txres, cache) <- runTx (Proxy @'Store.Consensus) txCtx action
  case (res, cache) of
    (Just (EndBlockResult updates params), Just c) -> do
      Cache.writeCache c
      pure $ Resp.EndBlock updates params (_txResultEvents txres)
    _ -> pure $ Resp.EndBlock [] Nothing []

defaultEndBlocker :: Req.EndBlock -> Sem r EndBlockResult
defaultEndBlocker = const $ pure (EndBlockResult [] Nothing)
