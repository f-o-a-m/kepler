module Tendermint.SDK.Application.Handlers
  ( Handler
  , HandlersContext(..)
  , makeApp
  ) where

import           Control.Lens                           (from, to, (&), (.~),
                                                         (^.))
import           Crypto.Hash                            (Digest)
import           Crypto.Hash.Algorithms                 (SHA256)
import qualified Data.ByteArray.Base64String            as Base64
import           Data.Default.Class                     (Default (..))
import           Data.Proxy
import           Network.ABCI.Server.App                (App (..),
                                                         MessageType (..),
                                                         Request (..),
                                                         Response (..))
import qualified Network.ABCI.Types.Messages.Request    as Req
import qualified Network.ABCI.Types.Messages.Response   as Resp
import           Polysemy
import           Polysemy.Error                         (catch)
import           Tendermint.SDK.Application.AnteHandler (AnteHandler)
                       --                                  applyAnteHandler)
import qualified Tendermint.SDK.Application.Module      as M
import qualified Tendermint.SDK.BaseApp         as BA
import           Tendermint.SDK.BaseApp.Errors          (AppError,
                                                         SDKError (..),
                                                         queryAppError,
                                                         throwSDKError,
                                                         txResultAppError)
import qualified Tendermint.SDK.BaseApp.Query           as Q
--import           Tendermint.SDK.BaseApp.Store           (ConnectionScope (..))
--import qualified Tendermint.SDK.BaseApp.Store           as Store
import           Tendermint.SDK.BaseApp.Transaction     as T
import           Tendermint.SDK.Crypto                  (RecoverableSignatureSchema,
                                                         SignatureSchema (..))
import           Tendermint.SDK.Types.Transaction       (parseTx)
import           Tendermint.SDK.Types.TxResult          (checkTxTxResult,
                                                         deliverTxTxResult)
--import qualified Tendermint.SDK.BaseApp.Store.IAVLStore as IAVL


type Handler mt r = Request mt -> Sem r (Response mt)

data Handlers r = Handlers
  { info       :: Handler 'MTInfo r
  , setOption  :: Handler 'MTSetOption r
  , initChain  :: Handler 'MTInitChain r
  , query      :: Handler 'MTQuery r
  , checkTx    :: Handler 'MTCheckTx r
  , beginBlock :: Handler 'MTBeginBlock r
  , deliverTx  :: Handler 'MTDeliverTx r
  , endBlock   :: Handler 'MTEndBlock r
  , commit     :: Handler 'MTCommit r
  }

defaultHandlers :: forall r. Handlers r
defaultHandlers = Handlers
  { info = defaultHandler
  , setOption = defaultHandler
  , initChain = defaultHandler
  , query = defaultHandler
  , checkTx = defaultHandler
  , beginBlock = defaultHandler
  , deliverTx = defaultHandler
  , endBlock = defaultHandler
  , commit = defaultHandler
  }
  where
    defaultHandler
      :: Default a
      => Applicative m
      => b
      -> m a
    defaultHandler = const $ pure def

data HandlersContext alg ms r = HandlersContext
  { signatureAlgP :: Proxy alg
  , modules       :: M.ModuleList ms r
  , anteHandler   :: AnteHandler r
  }

-- Common function between checkTx and deliverTx
makeHandlers
  :: forall alg ms r s.
     RecoverableSignatureSchema alg
  => Message alg ~ Digest SHA256
  => Members BA.BaseAppEffs s
  => M.ToApplication ms (M.Effs ms s)
  => T.HasTxRouter (M.ApplicationC ms) (M.Effs ms s)
  => T.HasTxRouter (M.ApplicationC ms) s
  => T.HasTxRouter (M.ApplicationD ms) (M.Effs ms s)
  => T.HasTxRouter (M.ApplicationD ms) s
  => Q.HasQueryRouter (M.ApplicationQ ms) (M.Effs ms s)
  => Q.HasQueryRouter (M.ApplicationQ ms) s
  => M.Eval ms s
  => M.Effs ms s ~ r
  => HandlersContext alg ms r
  -> Handlers s
makeHandlers (HandlersContext{..} :: HandlersContext alg ms r) =
  let

      rProxy :: Proxy s
      rProxy = Proxy

      app :: M.Application (M.ApplicationC ms) (M.ApplicationD ms) (M.ApplicationQ ms) (T.TxEffs BA.:& s) (Q.QueryEffs BA.:& s)
      app = M.makeApplication rProxy modules

      txParser bs = case parseTx signatureAlgP bs of
        Left err -> throwSDKError $ ParseError err
        Right tx -> pure $ T.RoutingTx tx

      checkServer :: T.TransactionApplication (Sem s)
      checkServer = -- applyAnteHandler anteHandler $
        T.serveTxApplication (Proxy @(M.ApplicationC ms)) rProxy $ M.applicationTxChecker app

      deliverServer :: T.TransactionApplication (Sem s)
      deliverServer = -- applyAnteHandler anteHandler $
        T.serveTxApplication (Proxy @(M.ApplicationD ms)) rProxy $ M.applicationTxDeliverer app

      queryServer :: Q.QueryApplication (Sem s)
      queryServer = Q.serveQueryApplication (Proxy @(M.ApplicationQ ms)) rProxy $ M.applicationQuerier app

      query (RequestQuery q) = 
        --Store.applyScope $
        catch
          (do
            queryResp <- queryServer q
            pure $ ResponseQuery queryResp
          )
          (\(err :: AppError) ->
            return . ResponseQuery $ def & queryAppError .~ err
          )

      --beginBlock _ = Store.applyScope (def <$ Store.beginBlock)

      checkTx (RequestCheckTx _checkTx) =  do
        res <- catch
          ( let txBytes =  _checkTx ^. Req._checkTxTx . to Base64.toBytes
            in do
               (res, _) <- txParser txBytes >>= checkServer
               pure res
          )
          (\(err :: AppError) ->
            return $ def & txResultAppError .~ err
          )
        return . ResponseCheckTx $ res ^. from checkTxTxResult

      deliverTx (RequestDeliverTx _deliverTx) = do
        res <- catch @AppError
          ( let txBytes = _deliverTx ^. Req._deliverTxTx . to Base64.toBytes
            in do
               (res, _) <- txParser txBytes >>= deliverServer
               pure res
          )
          (\(err :: AppError) ->
            return $ def & txResultAppError .~ err
          )
        return . ResponseDeliverTx $ res ^. from deliverTxTxResult

      --commit :: Handler 'MTCommit (BA.BaseAppEffs BA.:& core)
      --commit _ = Store.applyScope $ do
      --  Store.commitBlock
      --  Store.mergeScopes
      --  rootHash <- Store.storeRoot
      --  return . ResponseCommit $ def
      --    & Resp._commitData .~ Base64.fromBytes rootHash

  in defaultHandlers
       { query = query
       , checkTx = checkTx
       --, beginBlock = beginBlock
       , deliverTx = deliverTx
       --, commit = commit
       }

makeApp
  :: forall alg ms r s.
    
     RecoverableSignatureSchema alg
  => Message alg ~ Digest SHA256

  => Members BA.BaseAppEffs s

  => M.ToApplication ms (M.Effs ms s)
  => T.HasTxRouter (M.ApplicationC ms) (M.Effs ms s)
  => T.HasTxRouter (M.ApplicationC ms) s
  => T.HasTxRouter (M.ApplicationD ms) (M.Effs ms s)
  => T.HasTxRouter (M.ApplicationD ms) s
  => Q.HasQueryRouter (M.ApplicationQ ms) (M.Effs ms s)
  => Q.HasQueryRouter (M.ApplicationQ ms) s
  => M.Eval ms s
  => M.Effs ms s ~ r


  => HandlersContext alg ms r
  -> App (Sem s)
makeApp handlersContext =
  let Handlers{..} = makeHandlers handlersContext :: Handlers s
  in App $ \case
       RequestEcho echo ->
         pure . ResponseEcho $ def
           & Resp._echoMessage .~ echo ^. Req._echoMessage
       RequestFlush _ -> pure def
       msg@(RequestInfo _) -> info msg
       msg@(RequestSetOption _) -> setOption msg
       msg@(RequestInitChain _) -> initChain msg
       msg@(RequestQuery _) -> query msg
       msg@(RequestBeginBlock _) -> beginBlock msg
       msg@(RequestCheckTx _) ->  checkTx msg
       msg@(RequestDeliverTx _) -> deliverTx msg
       msg@(RequestEndBlock _) -> endBlock msg
       msg@(RequestCommit _) -> commit msg
