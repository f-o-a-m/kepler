module Tendermint.SDK.BaseApp.Transaction.Checker
  ( DefaultCheckTx(..)
  ) where

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
  => PreRoutedTx msg
  -> Sem r ()
defaultCheckTxHandler(PreRoutedTx Tx{txMsg}) =
  case validateMessage txMsg of
    V.Failure err ->
      throwSDKError . MessageValidation . map formatMessageSemanticError $ err
    V.Success _ -> pure ()

class DefaultCheckTx api (r :: EffectRow) where
    type DefaultCheckTxT api r :: *
    defaultCheckTxServer :: Proxy api -> Proxy r -> DefaultCheckTxT api r

instance (DefaultCheckTx a r, DefaultCheckTx b r) => DefaultCheckTx (a :<|> b) r where
    type DefaultCheckTxT (a :<|> b) r = DefaultCheckTxT a r :<|> DefaultCheckTxT b r

    defaultCheckTxServer _ pr =
        defaultCheckTxServer (Proxy :: Proxy a) pr :<|> defaultCheckTxServer (Proxy :: Proxy b) pr

instance DefaultCheckTx rest r => DefaultCheckTx (path :> rest) r where
    type DefaultCheckTxT (path :> rest) r = DefaultCheckTxT rest r

    defaultCheckTxServer _ = defaultCheckTxServer (Proxy :: Proxy rest)

instance (Member (Error AppError) r, ValidateMessage msg) =>  DefaultCheckTx (TypedMessage t msg :~> Return a) r where
    type DefaultCheckTxT (TypedMessage t msg :~> Return a) r = PreRoutedTx msg -> Sem r ()

    defaultCheckTxServer _ _ = defaultCheckTxHandler
