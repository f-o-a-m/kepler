module Tendermint.SDK.BaseApp.Transaction.AnteHandler
  ( AnteHandler(..)
  ) where

import           Polysemy (Sem)
import           Tendermint.SDK.BaseApp.Transaction.Types (RoutingTx)

newtype AnteHandler r = AnteHandler 
  (forall msg a. (RoutingTx msg -> Sem r a) -> (RoutingTx msg -> Sem r a))

instance Semigroup (AnteHandler r) where
  (<>) (AnteHandler h1) (AnteHandler h2) =
      AnteHandler $ h1 . h2

instance Monoid (AnteHandler r) where
  mempty = AnteHandler id

{-
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
-}