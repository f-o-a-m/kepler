module Tendermint.SDK.BaseApp.Block
  ( BlockEffs
  , evalBeginBlockHandler
  , evalEndBlockHandler
  , EndBlockResult (..)
  , defaultBeginBlocker
  , defaultEndBlocker
  ) where

import           Control.Lens                              ((^.))
import           Control.Monad.IO.Class                    (liftIO)
import           Data.IORef                                (newIORef)
import           Data.Proxy                                (Proxy (Proxy))
import           Network.ABCI.Types.Messages.FieldTypes    (ConsensusParams,
                                                            ValidatorUpdate)
import qualified Network.ABCI.Types.Messages.Request       as Req
import qualified Network.ABCI.Types.Messages.Response      as Resp
import           Polysemy                                  (Embed, Members, Sem)
import           Polysemy.Tagged                           (Tagged (..))
import           Tendermint.SDK.BaseApp.Errors             (AppError,
                                                            txResultAppError)
import qualified Tendermint.SDK.BaseApp.Store              as Store
import qualified Tendermint.SDK.BaseApp.Transaction.Cache  as Cache
import           Tendermint.SDK.BaseApp.Transaction.Effect (TxEffs, runTx)
import           Tendermint.SDK.BaseApp.Transaction.Types  (TransactionContext (..))
import           Tendermint.SDK.Codec                      (HasCodec (..))
import           Tendermint.SDK.Types.Effects              ((:&))
import           Tendermint.SDK.Types.TxResult             (txResultEvents)


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
  -> Sem r (Either AppError Resp.BeginBlock)
evalBeginBlockHandler action = do
  (BlockContext txCtx)  <- liftIO newBlockContext
  (res, txres)  <- runTx (Proxy @'Store.Consensus) txCtx action
  case res of
    Just (_, c) -> do
      Cache.writeCache c
      pure $ Right $ Resp.BeginBlock (txres ^. txResultEvents)
    Nothing -> pure $ Left (txres ^. txResultAppError)


defaultBeginBlocker :: Req.BeginBlock -> Sem r ()
defaultBeginBlocker = const $ pure ()

----------------------

data EndBlockResult = EndBlockResult [ValidatorUpdate] (Maybe ConsensusParams)

instance HasCodec EndBlockResult where
  encode _ = ""
  decode _ = Left ""


evalEndBlockHandler
  :: Members [Embed IO, Tagged 'Store.Consensus Store.ReadStore, Tagged 'Store.Consensus Store.WriteStore] r
  => Sem (BlockEffs :& r) EndBlockResult
  -> Sem r (Either AppError Resp.EndBlock)
evalEndBlockHandler action = do
  (BlockContext txCtx) <- liftIO newBlockContext
  (res, txres) <- runTx (Proxy @'Store.Consensus) txCtx action
  case res of
    Just (EndBlockResult updates params, c) -> do
      Cache.writeCache c
      pure $ Right $ Resp.EndBlock updates params (txres ^. txResultEvents)
    Nothing -> pure $ Left (txres ^. txResultAppError)



defaultEndBlocker :: Req.EndBlock -> Sem r EndBlockResult
defaultEndBlocker = const $ pure (EndBlockResult [] Nothing)
