{-# LANGUAGE TemplateHaskell #-}
module Tendermint.SDK.BaseApp.Gas
  ( GasMeter(..)
  , withGas
  , eval
  ) where

import           Control.Monad.IO.Class        (MonadIO (..))
import           Data.Int                      (Int64)
import qualified Data.IORef                    as Ref
import           Polysemy                      (Embed, Members, Sem, interpretH,
                                                makeSem, raise, runT)
import           Polysemy.Error                (Error)
import           Tendermint.SDK.BaseApp.Errors (AppError,
                                                SDKError (OutOfGasException),
                                                throwSDKError)

data GasMeter m a where
    WithGas :: forall m a. Int64 -> m a -> GasMeter m a

makeSem ''GasMeter

eval
  :: Members [Error AppError, Embed IO] r
  => Ref.IORef Int64
  -> Sem (GasMeter ': r) a
  -> Sem r a
eval meter = interpretH (\case
  WithGas gasCost action -> do
    remainingGas <- liftIO $ Ref.readIORef meter
    let balanceAfterAction = remainingGas - gasCost
    if balanceAfterAction < 0
      then throwSDKError OutOfGasException
      else do
        liftIO $ Ref.writeIORef meter balanceAfterAction
        a <- runT action
        raise $ eval meter a
  )

