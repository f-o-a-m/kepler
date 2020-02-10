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
import           Polysemy.Error                         (Error, catch)
import           Tendermint.SDK.Application.AnteHandler (AnteHandler)
                       --                                  applyAnteHandler)
import qualified Tendermint.SDK.Application.Module      as M
import qualified Tendermint.SDK.BaseApp         as BA
import           Tendermint.SDK.BaseApp.CoreEff         (CoreEffs)
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

data Handlers core = Handlers
  { info       :: Handler 'MTInfo (BA.BaseAppEffs BA.:& core)
  , setOption  :: Handler 'MTSetOption (BA.BaseAppEffs BA.:& core)
  , initChain  :: Handler 'MTInitChain (BA.BaseAppEffs BA.:& core)
  , query      :: Handler 'MTQuery  (BA.BaseAppEffs BA.:& core)
  , checkTx    :: Handler 'MTCheckTx (BA.BaseAppEffs BA.:& core)
  , beginBlock :: Handler 'MTBeginBlock (BA.BaseAppEffs BA.:& core)
  , deliverTx  :: Handler 'MTDeliverTx (BA.BaseAppEffs BA.:& core)
  , endBlock   :: Handler 'MTEndBlock (BA.BaseAppEffs BA.:& core)
  , commit     :: Handler 'MTCommit (BA.BaseAppEffs BA.:& core)
  }

defaultHandlers :: forall core. Handlers core
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

data HandlersContext alg ms r core = HandlersContext
  { signatureAlgP :: Proxy alg
  , modules       :: M.ModuleList ms r
  , compileToCore :: forall a. Sem (BA.BaseAppEffs BA.:& core) a -> Sem core a
  , anteHandler   :: AnteHandler r
  }

-- Common function between checkTx and deliverTx
makeHandlers
  :: forall alg ms r core.
     Member (Error AppError) r
  => RecoverableSignatureSchema alg
  => Message alg ~ Digest SHA256
  => M.ToApplication ms (M.Effs ms core)
  => T.HasTxRouter (M.ApplicationC ms) (M.Effs ms core)
  => T.HasTxRouter (M.ApplicationC ms) (BA.BaseAppEffs BA.:& core)
  => T.HasTxRouter (M.ApplicationD ms) (M.Effs ms core)
  => T.HasTxRouter (M.ApplicationD ms) (BA.BaseAppEffs BA.:& core)
  => Q.HasQueryRouter (M.ApplicationQ ms) (M.Effs ms core)
  => Q.HasQueryRouter (M.ApplicationQ ms) (BA.BaseAppEffs BA.:& core)
  => M.Eval ms core
  => M.Effs ms core ~ r
  => Members CoreEffs core
  => HandlersContext alg ms r core
  -> Handlers core
makeHandlers (HandlersContext{..} :: HandlersContext alg ms r core) =
  let

      app :: M.Application (M.ApplicationC ms) (M.ApplicationD ms) (M.ApplicationQ ms) 
               (T.TxEffs BA.:& BA.BaseAppEffs BA.:& core) (Q.QueryEffs BA.:& BA.BaseAppEffs BA.:& core)
      app = M.makeApplication (Proxy :: Proxy core) modules

      rProxy :: Proxy (BA.BaseAppEffs BA.:& core)
      rProxy = Proxy

      txParser bs = case parseTx signatureAlgP bs of
        Left err -> throwSDKError $ ParseError err
        Right tx -> pure $ T.RoutingTx tx

      checkServer :: T.TransactionApplication (Sem (BA.BaseAppEffs BA.:& core))
      checkServer = -- applyAnteHandler anteHandler $
        T.serveTxApplication (Proxy @(M.ApplicationC ms)) rProxy $ M.applicationTxChecker app

      deliverServer :: T.TransactionApplication (Sem (BA.BaseAppEffs BA.:& core))
      deliverServer = -- applyAnteHandler anteHandler $
        T.serveTxApplication (Proxy @(M.ApplicationD ms)) rProxy $ M.applicationTxDeliverer app

      queryServer :: Q.QueryApplication (Sem (BA.BaseAppEffs BA.:& core))
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
  :: forall alg ms r core.
     Members [Error AppError, Embed IO] r
  => RecoverableSignatureSchema alg
  => Message alg ~ Digest SHA256

  => M.ToApplication ms (M.Effs ms core)
  => T.HasTxRouter (M.ApplicationC ms) (M.Effs ms core)
  => T.HasTxRouter (M.ApplicationC ms) (BA.BaseAppEffs BA.:& core)
  => T.HasTxRouter (M.ApplicationD ms) (M.Effs ms core)
  => T.HasTxRouter (M.ApplicationD ms) (BA.BaseAppEffs BA.:& core)
  => Q.HasQueryRouter (M.ApplicationQ ms) (M.Effs ms core)
  => Q.HasQueryRouter (M.ApplicationQ ms) (BA.BaseAppEffs BA.:& core)

  => M.Eval ms core
  => M.Effs ms core ~ r

  => Members CoreEffs core

  => HandlersContext alg ms r core
  -> App (Sem core)
makeApp handlersContext@HandlersContext{compileToCore} =
  let Handlers{..} = makeHandlers handlersContext
  in App $ \case
       RequestEcho echo ->
         pure . ResponseEcho $ def
           & Resp._echoMessage .~ echo ^. Req._echoMessage
       RequestFlush _ -> pure def
       msg@(RequestInfo _) -> compileToCore $  info msg
       msg@(RequestSetOption _) -> compileToCore $ setOption msg
       msg@(RequestInitChain _) ->  compileToCore $ initChain msg
       msg@(RequestQuery _) -> compileToCore $ query msg
       msg@(RequestBeginBlock _) -> compileToCore $ beginBlock msg
       msg@(RequestCheckTx _) -> compileToCore $ checkTx msg
       msg@(RequestDeliverTx _) -> compileToCore $ deliverTx msg
       msg@(RequestEndBlock _) -> compileToCore $ endBlock msg
       msg@(RequestCommit _) ->  compileToCore $ commit msg
