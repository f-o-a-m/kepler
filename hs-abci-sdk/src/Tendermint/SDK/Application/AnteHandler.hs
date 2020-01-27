module Tendermint.SDK.Application.AnteHandler
  ( AnteHandler(..)
  , applyAnteHandler
  , baseAppAnteHandler
  ) where

import           Control.Monad                      (unless)
import           Polysemy
import           Polysemy.Error                     (Error)
import           Tendermint.SDK.BaseApp.Errors      (AppError, SDKError (..),
                                                     throwSDKError)
import           Tendermint.SDK.BaseApp.Transaction (RoutingTx (..),
                                                     TransactionApplication)
import qualified Tendermint.SDK.Modules.Auth        as A
import           Tendermint.SDK.Types.Message       (Msg (..))
import           Tendermint.SDK.Types.Transaction   (Tx (..))

data AnteHandler r = AnteHandler
  ( TransactionApplication (Sem r) -> TransactionApplication (Sem r))

instance Semigroup (AnteHandler r) where
  (<>) (AnteHandler h1) (AnteHandler h2) =
      AnteHandler $ h1 . h2

instance Monoid (AnteHandler r) where
  mempty = AnteHandler id

applyAnteHandler
  :: AnteHandler r
  -> TransactionApplication (Sem r)
  -> TransactionApplication (Sem r)
applyAnteHandler (AnteHandler ah) = ($) ah

nonceAnteHandler
  :: Members A.AuthEffs r
  => Member (Error AppError) r
  => AnteHandler r
nonceAnteHandler = AnteHandler $
  \txApplication tx@(RoutingTx Tx{..}) -> do
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
    result <- txApplication tx
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
