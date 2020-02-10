{-# LANGUAGE UndecidableInstances #-}
module Tendermint.SDK.BaseApp.Transaction.Router
  ( HasTxRouter(..)
  , emptyTxServer
  ) where

import           Control.Monad.IO.Class                    (liftIO)
import           Data.ByteString                           (ByteString)
import           Data.Proxy
import           Data.String.Conversions                   (cs)
import           GHC.TypeLits                              (KnownSymbol,
                                                            symbolVal)
import           Polysemy                                  (EffectRow, Embed,
                                                            Members, Sem)
import           Servant.API
import qualified Tendermint.SDK.BaseApp.Router             as R
import           Tendermint.SDK.BaseApp.Store              (ReadStore)
import           Tendermint.SDK.BaseApp.Transaction.Cache  (Cache)
import           Tendermint.SDK.BaseApp.Transaction.Effect (TxEffs, runTx)
import           Tendermint.SDK.BaseApp.Transaction.Types
import           Tendermint.SDK.Codec                      (HasCodec (..))
import           Tendermint.SDK.Types.Effects              ((:&))
import           Tendermint.SDK.Types.Message              (HasMessageType (..),
                                                            Msg (..))
import           Tendermint.SDK.Types.TxResult             (TxResult)

--------------------------------------------------------------------------------

class HasTxRouter layout (r :: EffectRow) where
  type RouteTx layout (s :: EffectRow) :: *
  routeTx
        :: Proxy layout
        -> Proxy r
        -> R.Delayed (Sem r) env (RoutingTx ByteString) (RouteTx layout (TxEffs :& r))
        -> R.Router env r (RoutingTx ByteString) (TxResult, Maybe Cache)

--  applyAnteHandler
--    :: Proxy layout
--    => Proxy r
--
  hoistTxRouter
    :: Proxy layout
    -> Proxy r
    -> (forall a. Sem s a -> Sem s' a)
    -> RouteTx layout s
    -> RouteTx layout s'

instance (HasTxRouter a r, HasTxRouter b r) => HasTxRouter (a :<|> b) r where
  type RouteTx (a :<|> b) s = RouteTx a s :<|> RouteTx b s

  routeTx _ pr server =
    R.choice (routeTx (Proxy @a) pr ((\ (a :<|> _) -> a) <$> server))
             (routeTx (Proxy @b) pr ((\ (_ :<|> b) -> b) <$> server))

  hoistTxRouter _ pr nat (a :<|> b) =
    hoistTxRouter (Proxy @a) pr nat a :<|> hoistTxRouter (Proxy @b) pr nat b

instance (HasTxRouter sublayout r, KnownSymbol path) => HasTxRouter (path :> sublayout) r where

  type RouteTx (path :> sublayout) s = RouteTx sublayout s

  routeTx _ pr subserver =
    R.pathRouter (cs (symbolVal proxyPath)) (routeTx (Proxy @sublayout) pr subserver)
    where proxyPath = Proxy @path

  hoistTxRouter _ pr nat = hoistTxRouter (Proxy @sublayout) pr nat

methodRouter
  :: HasCodec a
  => Members [Embed IO, ReadStore] r
  => R.Delayed (Sem r) env (RoutingTx msg) (Sem (TxEffs :& r) a)
  -> R.Router env r (RoutingTx msg) (TxResult, Maybe Cache)
methodRouter action =
  let route' env tx = do
        ctx <- liftIO $ newTransactionContext tx
        let action' = runTx ctx <$> action
        R.runAction action' env tx (pure . R.Route)
  in R.leafRouter route'

instance ( HasMessageType msg, HasCodec msg
         , Members [ReadStore, Embed IO] r
         , HasCodec a
         ) => HasTxRouter (TypedMessage msg :~> Return a) r where

  type RouteTx (TypedMessage msg :~> Return a) r = RoutingTx msg -> Sem r a

  routeTx _ _ subserver =
    let f (RoutingTx tx@Tx{txMsg}) =
          if msgType txMsg == mt
            then case decode $ msgData txMsg of
              Left e -> R.delayedFail $
                R.InvalidRequest ("Failed to parse message of type " <> mt <> ": " <> e <> ".")
              Right a -> pure . RoutingTx $ tx {txMsg = txMsg {msgData = a}}
            else R.delayedFail R.PathNotFound
    in methodRouter $ R.addBody subserver $ R.withRequest f
      where mt = messageType (Proxy :: Proxy msg)

  hoistTxRouter _ _ nat = (.) nat

emptyTxServer :: RouteTx EmptyTxServer r
emptyTxServer = EmptyTxServer

instance HasTxRouter EmptyTxServer r where
  type RouteTx EmptyTxServer r = EmptyTxServer
  routeTx _ _ _ = R.StaticRouter mempty mempty

  hoistTxRouter _ _ _ = id
