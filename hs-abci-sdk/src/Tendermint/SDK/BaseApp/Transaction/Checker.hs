module Tendermint.SDK.BaseApp.Transaction.Checker
  ( DefaultCheckTx(..)
  , VoidReturn
  ) where

import           Data.Kind                                (Type)
import           Data.Proxy
import qualified Data.Validation                          as V
import           Polysemy                                 (EffectRow, Member,
                                                           Sem)
import           Polysemy.Error                           (Error)
import           Servant.API                              ((:<|>) (..), (:>))
import           Tendermint.SDK.BaseApp.Errors            (AppError,
                                                           SDKError (..),
                                                           throwSDKError)
import           Tendermint.SDK.BaseApp.Transaction.Types
import           Tendermint.SDK.Types.Message             (ValidateMessage (..), formatMessageSemanticError)

defaultCheckTxHandler
  :: Member (Error AppError) r
  => ValidateMessage msg
  => RoutingTx msg
  -> Sem r ()
defaultCheckTxHandler(RoutingTx Tx{txMsg}) =
  case validateMessage txMsg of
    V.Failure err ->
      throwSDKError . MessageValidation . map formatMessageSemanticError $ err
    V.Success _ -> pure ()

type family VoidReturn (api :: Type) :: Type where
  VoidReturn (a :<|> b) = VoidReturn a :<|> VoidReturn b
  VoidReturn (path :> a) = path :> VoidReturn a
  VoidReturn (TypedMessage msg :~> Return a) = TypedMessage msg :~> Return ()

class DefaultCheckTx api (r :: EffectRow) where
    type DefaultCheckTxT api r :: Type
    defaultCheckTx :: Proxy api -> Proxy r -> DefaultCheckTxT api r

instance (DefaultCheckTx a r, DefaultCheckTx b r) => DefaultCheckTx (a :<|> b) r where
    type DefaultCheckTxT (a :<|> b) r = DefaultCheckTxT a r :<|> DefaultCheckTxT b r

    defaultCheckTx _ pr =
        defaultCheckTx (Proxy :: Proxy a) pr :<|> defaultCheckTx (Proxy :: Proxy b) pr

instance DefaultCheckTx rest r => DefaultCheckTx (path :> rest) r where
    type DefaultCheckTxT (path :> rest) r = DefaultCheckTxT rest r

    defaultCheckTx _ = defaultCheckTx (Proxy :: Proxy rest)

instance (Member (Error AppError) r, ValidateMessage msg) =>  DefaultCheckTx (TypedMessage msg :~> Return a) r where
    type DefaultCheckTxT (TypedMessage msg :~> Return a) r = RoutingTx msg -> Sem r ()

    defaultCheckTx _ _ = defaultCheckTxHandler

instance DefaultCheckTx EmptyTxServer r where
    type DefaultCheckTxT EmptyTxServer r = EmptyTxServer

    defaultCheckTx _ _ = EmptyTxServer
