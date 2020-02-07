{-# LANGUAGE UndecidableInstances #-}
module Tendermint.SDK.BaseApp.Transaction.Router
  ( HasTxRouter(..)
  , emptyTxServer
  ) where

import           Data.ByteString                             (ByteString)
import           Data.Proxy
import           Data.String.Conversions                     (cs)
import           GHC.TypeLits                                (KnownSymbol,
                                                              symbolVal)
import           Polysemy                                    (Sem, Members, Embed, EffectRow)
import Polysemy.Reader (Reader, ask)
import Tendermint.SDK.BaseApp.Store (ReadStore)
import           Servant.API
import qualified Tendermint.SDK.BaseApp.Router               as R
import           Tendermint.SDK.BaseApp.Transaction.Modifier
import           Tendermint.SDK.BaseApp.Transaction.Effect
import           Tendermint.SDK.BaseApp.Transaction.Types
import           Tendermint.SDK.Types.TxResult (TxResult)
import           Tendermint.SDK.Types.Effects ((:&))
import           Tendermint.SDK.Codec                        (HasCodec (..))
import           Tendermint.SDK.Types.Message                (HasMessageType (..),
                                                              Msg (..))

--------------------------------------------------------------------------------

class HasTxRouter layout (r :: EffectRow) (c :: RouteContext) where
  type RouteTx layout (s :: EffectRow) c :: *
  routeTx
        :: Proxy layout
        -> Proxy r
        -> Proxy c
        -> R.Delayed (Sem (Reader TransactionContext ': r)) env (RoutingTx ByteString) (RouteTx layout (TxEffs :& r) c)
        -> R.Router env r (RoutingTx ByteString) TxResult

  hoistTxRouter
    :: Proxy layout
    -> Proxy r
    -> Proxy c
    -> (forall a. Sem s a -> Sem s' a)
    -> RouteTx layout s c
    -> RouteTx layout s' c

instance (HasTxRouter a r c, HasTxRouter b r c) => HasTxRouter (a :<|> b) r c where
  type RouteTx (a :<|> b) s c = RouteTx a s c :<|> RouteTx b s c

  routeTx _ pr pc ctx server =
    R.choice (routeTx (Proxy @a) pr pc ctx ((\ (a :<|> _) -> a) <$> server))
             (routeTx (Proxy @b) pr pc ctx ((\ (_ :<|> b) -> b) <$> server))

  hoistTxRouter _ pr pc nat (a :<|> b) =
    hoistTxRouter (Proxy @a) pr pc nat a :<|> hoistTxRouter (Proxy @b) pr pc nat b

instance (HasTxRouter sublayout r c, KnownSymbol path) => HasTxRouter (path :> sublayout) r c where

  type RouteTx (path :> sublayout) s c = RouteTx sublayout s c

  routeTx _ pr pc ctx subserver =
    R.pathRouter (cs (symbolVal proxyPath)) (routeTx (Proxy @sublayout) pr pc ctx subserver)
    where proxyPath = Proxy @path

  hoistTxRouter _ pr pc nat = hoistTxRouter (Proxy @sublayout) pr pc nat

methodRouter
  :: HasCodec a
  => Members [Embed IO, ReadStore] r
  => TransactionContext
  -> R.Delayed (Sem r) env (RoutingTx msg) (Sem (TxEffs :& r) a)
  -> R.Router env r (RoutingTx msg) TxResult
methodRouter ctx action =
  let route' env tx =
        let action' = runTx ctx <$> action
        in R.runAction action' env tx (pure . R.Route)
  in R.leafRouter route'

instance ( HasMessageType msg, HasCodec msg
         , Members [ReadStore, Embed IO] r
         , HasCodec (OnCheckReturn c oc a)
         ) => HasTxRouter (TypedMessage msg :~> Return' oc a) r c where

  type RouteTx (TypedMessage msg :~> Return' oc a) s c = RoutingTx msg -> Sem s (OnCheckReturn c oc a)

  routeTx _ _ _ ctx subserver =
    let f (RoutingTx tx@Tx{txMsg}) =
          if msgType txMsg == mt
            then case decode $ msgData txMsg of
              Left e -> R.delayedFail $
                R.InvalidRequest ("Failed to parse message of type " <> mt <> ": " <> e <> ".")
              Right a -> pure . RoutingTx $ tx {txMsg = txMsg {msgData = a}}
            else R.delayedFail R.PathNotFound
    in methodRouter ctx $ R.addBody subserver $ R.withRequest f
      where mt = messageType (Proxy :: Proxy msg)

  hoistTxRouter _ _ _ nat = (.) nat

emptyTxServer :: RouteTx EmptyTxServer r c
emptyTxServer = EmptyTxServer

instance HasTxRouter EmptyTxServer r c where
  type RouteTx EmptyTxServer r c = EmptyTxServer
  routeTx _ _ _ _ _ = R.StaticRouter mempty mempty

  hoistTxRouter _ _ _ _ = id
