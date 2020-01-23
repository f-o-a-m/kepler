module Tendermint.SDK.Application.AnteHandler
  ( AnteHandler(..)
  , applyAnteHandler
  , baseAppAnteHandler
  ) where

import           Control.Monad                     (unless)
import           Polysemy
import           Polysemy.Error                    (Error)
import qualified Tendermint.SDK.Application.Module as M
import           Tendermint.SDK.BaseApp.Errors     (AppError, SDKError (..),
                                                    throwSDKError)
import qualified Tendermint.SDK.Modules.Auth       as A
import           Tendermint.SDK.Types.Message      (Msg (..))
import           Tendermint.SDK.Types.Transaction  (PreRoutedTx (..), Tx (..))

data AnteHandler r where
  AnteHandler :: (forall msg. M.Router r msg -> M.Router r msg) -> AnteHandler r

instance Semigroup (AnteHandler r) where
  (<>) (AnteHandler h1) (AnteHandler h2) =
      AnteHandler $ h1 . h2

instance Monoid (AnteHandler r) where
  mempty = AnteHandler id

applyAnteHandler :: AnteHandler r -> M.Router r msg -> M.Router r msg
applyAnteHandler (AnteHandler ah) = ($) ah

nonceAnteHandler
  :: Members A.AuthEffs r
  => Member (Error AppError) r
  => AnteHandler r
nonceAnteHandler = AnteHandler $ \(M.Router router) ->
    M.Router $ \tx@(PreRoutedTx Tx{..}) -> do
      let Msg{msgAuthor} = txMsg
      result <- router tx
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
