module Tendermint.SDK.Application.AnteHandler
  ( AnteHandler(..)
  , applyAnteHandler
  , baseAppAnteHandler
  ) where

import           Control.Monad                     (unless, void)
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
      AnteHandler $ h2 . h1

instance Monoid (AnteHandler r) where
  mempty = AnteHandler id

applyAnteHandler :: AnteHandler r -> M.Router r msg -> M.Router r msg
applyAnteHandler (AnteHandler ah) = ($) ah

createAccountAnteHandler
  :: Members A.AuthEffs r
  => AnteHandler r
createAccountAnteHandler = AnteHandler $ \(M.Router router) ->
  M.Router $ \tx@(PreRoutedTx Tx{..}) -> do
    let Msg{msgAuthor} = txMsg
    mAcnt <- A.getAccount msgAuthor
    case mAcnt of
      Nothing -> void $ A.createAccount msgAuthor
      _       -> pure ()
    router tx >>= pure

nonceAnteHandler
  :: Members A.AuthEffs r
  => Member (Error AppError) r
  => AnteHandler r
nonceAnteHandler = AnteHandler $ \(M.Router router) ->
    M.Router $ \tx@(PreRoutedTx Tx{..}) -> do
      let Msg{msgAuthor} = txMsg
      preMAcnt <- A.getAccount msgAuthor
      case preMAcnt of
        Just A.Account{accountNonce} -> do
          let expectedNonce = accountNonce + 1
          unless (txNonce == expectedNonce) $
            throwSDKError (NonceException expectedNonce txNonce)
        Nothing -> do
          unless (txNonce == 1) $
            throwSDKError (NonceException 1 txNonce)
      result <- router tx
      postMAcnt <- A.getAccount msgAuthor
      case postMAcnt of
        Just acnt@A.Account{accountNonce} -> do
          A.putAccount msgAuthor $
            acnt { A.accountNonce = accountNonce + 1}
        -- @NOTE: no-op when no nonce is availble to update
        Nothing -> pure ()
      pure result

baseAppAnteHandler
  :: Members A.AuthEffs r
  => Member (Error AppError) r
  => AnteHandler r
baseAppAnteHandler = mconcat $
  [ createAccountAnteHandler
  , nonceAnteHandler
  ]
