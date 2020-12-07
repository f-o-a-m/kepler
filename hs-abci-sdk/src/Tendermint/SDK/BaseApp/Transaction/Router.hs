{-# LANGUAGE UndecidableInstances #-}
module Tendermint.SDK.BaseApp.Transaction.Router
  ( HasTxRouter(..)
  , emptyTxServer
  ) where

import           Control.Monad.IO.Class                         (liftIO)
import           Data.ByteString                                (ByteString)
import           Data.Kind                                      (Type)
import           Data.Monoid
import           Data.Proxy
import           Data.String.Conversions                        (cs)
import           GHC.TypeLits                                   (KnownSymbol,
                                                                 symbolVal)
import           Polysemy                                       (EffectRow,
                                                                 Embed, Members,
                                                                 Sem)
import           Polysemy.Tagged                                (Tagged)
import           Servant.API
import qualified Tendermint.SDK.BaseApp.Router                  as R
import           Tendermint.SDK.BaseApp.Store                   (ReadStore,
                                                                 Scope)
import           Tendermint.SDK.BaseApp.Transaction.AnteHandler (AnteHandler)
import           Tendermint.SDK.BaseApp.Transaction.Cache       (Cache)
import           Tendermint.SDK.BaseApp.Transaction.Effect      (TxEffs, runTx)
import           Tendermint.SDK.BaseApp.Transaction.Types
import           Tendermint.SDK.Codec                           (HasCodec (..))
import           Tendermint.SDK.Types.Effects                   ((:&))
import           Tendermint.SDK.Types.Message                   (HasMessageType (..),
                                                                 Msg (..))
import           Tendermint.SDK.Types.TxResult                  (TxResult)
--------------------------------------------------------------------------------

class HasTxRouter layout (r :: EffectRow) (scope :: Scope) where
  type RouteTx layout (s :: EffectRow) :: Type
  routeTx
        :: Proxy layout
        -> Proxy r
        -> Proxy scope
        -> R.Delayed (Sem r) env (RoutingTx ByteString) (RouteTx layout (TxEffs :& r))
        -> R.Router env r (RoutingTx ByteString) (TxResult, Maybe Cache)

  applyAnteHandler
    :: Proxy layout
    -> Proxy r
    -> Proxy scope
    -> AnteHandler r
    -> RouteTx layout r
    -> RouteTx layout r

  hoistTxRouter
    :: Proxy layout
    -> Proxy r
    -> Proxy scope
    -> (forall a. Sem s a -> Sem s' a)
    -> RouteTx layout s
    -> RouteTx layout s'

instance (HasTxRouter a r scope, HasTxRouter b r scope) => HasTxRouter (a :<|> b) r scope where
  type RouteTx (a :<|> b) s = RouteTx a s :<|> RouteTx b s

  routeTx _ pr ps server =
    R.choice (routeTx (Proxy @a) pr ps ((\ (a :<|> _) -> a) <$> server))
             (routeTx (Proxy @b) pr ps ((\ (_ :<|> b) -> b) <$> server))

  applyAnteHandler _ pr ps ah (a :<|> b) =
    applyAnteHandler (Proxy @a) pr ps ah a :<|>
    applyAnteHandler (Proxy @b) pr ps ah b

  hoistTxRouter _ pr nat ps (a :<|> b) =
    hoistTxRouter (Proxy @a) pr nat ps a :<|> hoistTxRouter (Proxy @b) pr nat ps b

instance (HasTxRouter sublayout r scope, KnownSymbol path) => HasTxRouter (path :> sublayout) r scope where

  type RouteTx (path :> sublayout) s = RouteTx sublayout s

  routeTx _ pr ps subserver =
    R.pathRouter (cs (symbolVal proxyPath)) (routeTx (Proxy @sublayout) pr ps subserver)
    where proxyPath = Proxy @path

  applyAnteHandler _ pr ps ah = applyAnteHandler (Proxy @sublayout) pr ps ah

  hoistTxRouter _ pr ps nat = hoistTxRouter (Proxy @sublayout) pr ps nat

methodRouter
  :: HasCodec a
  => Members [Embed IO, Tagged scope ReadStore] r
  => Proxy scope
  -> R.Delayed (Sem r) env (RoutingTx msg) (Sem (TxEffs :& r) a)
  -> R.Router env r (RoutingTx msg) (TxResult, Maybe Cache)
methodRouter ps action =
  let route' env tx = do
        ctx <- liftIO $ newTransactionContext True tx
        let action' = fmap (\(_,res,c) -> (res,c)) . runTx ps ctx <$> action
        R.runAction action' env tx (pure . R.Route)
  in R.leafRouter route'

instance ( HasMessageType msg, HasCodec msg
         , Members [Tagged scope ReadStore, Embed IO] r
         , HasCodec a
         ) => HasTxRouter (TypedMessage msg :~> Return a) r scope where

  type RouteTx (TypedMessage msg :~> Return a) r = RoutingTx msg -> Sem r a

  routeTx _ _ ps subserver =
    let f (RoutingTx tx@Tx{txMsg}) =
          if msgType txMsg == mt
            then case decode $ msgData txMsg of
              Left e -> R.delayedFail $
                R.InvalidRequest ("Failed to parse message of type " <> mt <> ": " <> e <> ".")
              Right a -> pure . RoutingTx $ tx {txMsg = txMsg {msgData = a}}
            else R.delayedFail R.PathNotFound
    in methodRouter ps $ R.addBody subserver $ R.withRequest f
      where mt = messageType (Proxy :: Proxy msg)

  applyAnteHandler _ _ _ ah f = appEndo ah f

  hoistTxRouter _ _ _ nat = (.) nat

emptyTxServer :: RouteTx EmptyTxServer r
emptyTxServer = EmptyTxServer

instance HasTxRouter EmptyTxServer r scope where
  type RouteTx EmptyTxServer r = EmptyTxServer
  routeTx _ _ _ _ = R.StaticRouter mempty mempty

  applyAnteHandler _ _ _ _ = id

  hoistTxRouter _ _ _ _ = id
