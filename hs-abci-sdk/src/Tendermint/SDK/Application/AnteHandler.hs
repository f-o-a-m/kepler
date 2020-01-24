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
import Debug.Trace (traceShow, trace)

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
    M.Router $ \tx@(PreRoutedTx Tx{..}) -> traceShow ("ANTEHANDLER" :: String) $ do
      let Msg{msgAuthor} = txMsg
      preMAcnt <- trace ("Getting account to check nonce...") $ A.getAccount msgAuthor
      case preMAcnt of
        Just A.Account{accountNonce} -> do
          trace ("Found account. Comparing nonces: Account " ++ show accountNonce ++ " Tx " ++ show txNonce) $
            unless (accountNonce == txNonce - 1) $ throwSDKError (NonceException (accountNonce + 1) txNonce)
        -- @NOTE: unitialized account -> txNonce == 1  (i.e., this is the first transaction)
        Nothing -> do
          trace ("No account found. Checking tx nonce is 1:  " ++ show txNonce) $ unless (txNonce == 1) $
            throwSDKError (NonceException 1 txNonce)
      result <- router tx
      postMAcnt <- trace ("Getting account to update nonce...") $ A.getAccount msgAuthor
      case postMAcnt of
        Just acnt@A.Account{accountNonce} -> do
          trace ("Found account. Updating nonce to " ++ show (accountNonce + 1)) $ A.putAccount msgAuthor $
            acnt { A.accountNonce = accountNonce + 1}
        -- @NOTE: no-op when no nonce is availble to update
        Nothing -> trace ("No account found. No nonce updated...") $ pure ()
      pure result

baseAppAnteHandler
  :: Members A.AuthEffs r
  => Member (Error AppError) r
  => AnteHandler r
baseAppAnteHandler = mconcat $
  [ nonceAnteHandler
  ]
