module Tendermint.SDK.Application.AnteHandler
  ( module Tendermint.SDK.Application.AnteHandler
  --  Re-Exports
  , AnteHandler(..)
  ) where

import           Control.Monad                      (unless)
import qualified Debug.Trace                        as Trace
import           Polysemy
import           Polysemy.Error                     (Error)
import           Tendermint.SDK.BaseApp.Errors      (AppError, SDKError (..),
                                                     throwSDKError)
import           Tendermint.SDK.BaseApp.Transaction (AnteHandler (..),
                                                     RoutingTx (..))
import qualified Tendermint.SDK.Modules.Auth        as A
import           Tendermint.SDK.Types.Message       (Msg (..))
import           Tendermint.SDK.Types.Transaction   (Tx (..))


nonceAnteHandler
  :: Members A.AuthEffs r
  => Member (Error AppError) r
  => AnteHandler r
nonceAnteHandler = AnteHandler $
  \txApplication tx@(RoutingTx Tx{..}) -> do
    Trace.traceM "Running nonceAntehandler"
    let Msg{msgAuthor} = txMsg
    mAcnt <- A.getAccount msgAuthor
    account <- case mAcnt of
      Just a@A.Account{accountNonce} -> do
        unless (accountNonce <= txNonce) $
          throwSDKError (NonceException accountNonce txNonce)
        pure a
      Nothing -> do
        unless (txNonce == 0) $
          throwSDKError (NonceException 0 txNonce)
        A.createAccount msgAuthor
    Trace.traceM "Running handler"
    result <- txApplication tx
    Trace.traceM "Handler ran"
    A.putAccount msgAuthor $
      account { A.accountNonce = A.accountNonce account + 1}
    pure result

baseAppAnteHandler
  :: Members A.AuthEffs r
  => Member (Error AppError) r
  => AnteHandler r
baseAppAnteHandler = mconcat $
  [ nonceAnteHandler
  ]
