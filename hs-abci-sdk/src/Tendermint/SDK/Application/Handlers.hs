module Tendermint.SDK.Application.Handlers
  ( Handler
  , HandlersContext(..)
  , makeApp
  ) where

import           Control.Lens                             (from, to, (&), (.~),
                                                           (^.))
import           Crypto.Hash                              (Digest)
import           Crypto.Hash.Algorithms                   (SHA256)
import qualified Data.ByteArray.Base64String              as Base64
import           Data.Default.Class                       (Default (..))
import           Data.Proxy
import           Network.ABCI.Server.App                  (App (..),
                                                           MessageType (..),
                                                           Request (..),
                                                           Response (..),
                                                           transformApp)
import qualified Network.ABCI.Types.Messages.Request      as Req
import qualified Network.ABCI.Types.Messages.Response     as Resp
import           Polysemy
import           Polysemy.Error                           (catch)
import qualified Tendermint.SDK.Application.Module        as M
import qualified Tendermint.SDK.BaseApp                   as BA
import           Tendermint.SDK.BaseApp.Errors            (SDKError (..),
                                                           queryAppError,
                                                           throwSDKError,
                                                           txResultAppError)
import qualified Tendermint.SDK.BaseApp.Query             as Q
import qualified Tendermint.SDK.BaseApp.Store             as Store
import           Tendermint.SDK.BaseApp.Transaction       as T
import           Tendermint.SDK.BaseApp.Transaction.Cache (writeCache)
import           Tendermint.SDK.Crypto                    (RecoverableSignatureSchema,
                                                           SignatureSchema (..))
import           Tendermint.SDK.Types.Transaction         (parseTx)
import           Tendermint.SDK.Types.TxResult            (checkTxTxResult,
                                                           deliverTxTxResult)

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


data HandlersContext alg ms core = HandlersContext
  { signatureAlgP :: Proxy alg
  , modules       :: M.ModuleList ms (M.Effs ms core)
  , beginBlockers :: [Req.BeginBlock -> Sem (BA.BaseAppEffs core) Resp.BeginBlock]
  , endBlockers   :: [Req.EndBlock -> Sem (BA.BaseAppEffs core) Resp.EndBlock]
  , anteHandler   :: BA.AnteHandler (M.Effs ms core)
  , compileToCore :: forall a. Sem (BA.BaseAppEffs core) a -> Sem core a
  }

-- Common function between checkTx and deliverTx
makeHandlers
  :: forall alg ms core.
     RecoverableSignatureSchema alg
  => Message alg ~ Digest SHA256
  => M.ToApplication ms (M.Effs ms core)
  => T.HasTxRouter (M.ApplicationC ms) (M.Effs ms core) 'Store.QueryAndMempool
  => T.HasTxRouter (M.ApplicationC ms) (BA.BaseAppEffs core) 'Store.QueryAndMempool
  => T.HasTxRouter (M.ApplicationD ms) (M.Effs ms core) 'Store.Consensus
  => T.HasTxRouter (M.ApplicationD ms) (BA.BaseAppEffs core) 'Store.Consensus
  => Q.HasQueryRouter (M.ApplicationQ ms) (M.Effs ms core)
  => Q.HasQueryRouter (M.ApplicationQ ms) (BA.BaseAppEffs core)
  => M.Eval ms core
 -- => M.Effs ms core ~ (BA.AppEffs (M.ModulesEffs ms) core)
  => HandlersContext alg ms core
  -> Handlers (BA.BaseAppEffs core)
makeHandlers (HandlersContext{..} :: HandlersContext alg ms core) =
  let

      cProxy :: Proxy core
      cProxy = Proxy

      rProxy :: Proxy (BA.BaseAppEffs core)
      rProxy = Proxy

      app :: M.Application (M.ApplicationC ms) (M.ApplicationD ms) (M.ApplicationQ ms)
               (T.TxEffs BA.:& BA.BaseAppEffs core) (Q.QueryEffs BA.:& BA.BaseAppEffs core)
      app = M.makeApplication cProxy anteHandler modules

      txParser bs = case parseTx signatureAlgP bs of
        Left err -> throwSDKError $ ParseError err
        Right tx -> pure $ T.RoutingTx tx

      checkServer :: T.TransactionApplication (Sem (BA.BaseAppEffs core))
      checkServer =
        T.serveTxApplication (Proxy @(M.ApplicationC ms)) rProxy (Proxy @'Store.QueryAndMempool) $ M.applicationTxChecker app

      deliverServer :: T.TransactionApplication (Sem (BA.BaseAppEffs core))
      deliverServer =
        T.serveTxApplication (Proxy @(M.ApplicationD ms)) rProxy (Proxy @'Store.Consensus) $ M.applicationTxDeliverer app

      queryServer :: Q.QueryApplication (Sem (BA.BaseAppEffs core))
      queryServer = Q.serveQueryApplication (Proxy @(M.ApplicationQ ms)) rProxy $ M.applicationQuerier app

      query (RequestQuery q) =
        --Store.applyScope $
        catch
          (do
            queryResp <- queryServer q
            pure $ ResponseQuery queryResp
          )
          (\(err :: BA.AppError) ->
            return . ResponseQuery $ def & queryAppError .~ err
          )

      checkTx (RequestCheckTx _checkTx) =  do
        res <- catch
          ( let txBytes =  _checkTx ^. Req._checkTxTx . to Base64.toBytes
            in do
               (res, _) <- txParser txBytes >>= checkServer
               pure res
          )
          (\(err :: BA.AppError) ->
            return $ def & txResultAppError .~ err
          )
        return . ResponseCheckTx $ res ^. from checkTxTxResult

      deliverTx (RequestDeliverTx _deliverTx) = do
        res <- catch @BA.AppError
          ( let txBytes = _deliverTx ^. Req._deliverTxTx . to Base64.toBytes
            in do
               (res, cache) <- txParser txBytes >>= deliverServer
               maybe (pure ()) writeCache cache
               pure res
          )
          (\(err :: BA.AppError) ->
            return $ def & txResultAppError .~ err
          )
        return . ResponseDeliverTx $ res ^. from deliverTxTxResult

      mergeBeginBlock (Resp.BeginBlock a) (Resp.BeginBlock b) = Resp.BeginBlock (a++b)
      beginBlock (RequestBeginBlock bb) = do
        res <- mapM (\f -> f bb) beginBlockers
        pure $ ResponseBeginBlock (foldl mergeBeginBlock (Resp.BeginBlock []) res)

      mergeEndBlock (Resp.EndBlock updatesA paramsA eventsA) (Resp.EndBlock updatesB paramsB eventsB) =
        Resp.EndBlock (updatesA ++ updatesB) (mergeParams paramsA paramsB) (eventsA ++ eventsB)
        where
          mergeParams Nothing y = y
          mergeParams x _       = x
      endBlock (RequestEndBlock eb) = do
        res <- mapM (\f -> f eb) endBlockers
        pure $ ResponseEndBlock (foldl mergeEndBlock (Resp.EndBlock [] Nothing []) res)

      commit :: Handler 'MTCommit (BA.BaseAppEffs core)
      commit _ = do
        _ <- Store.commit
        rootHash <- Store.commitBlock
        return . ResponseCommit $ def
          & Resp._commitData .~ Base64.fromBytes rootHash

  in defaultHandlers
       { query = query
       , checkTx = checkTx
       , beginBlock = beginBlock
       , deliverTx = deliverTx
       , endBlock = endBlock
       , commit = commit
       }

makeApp
  :: forall alg ms core.

     RecoverableSignatureSchema alg
  => Message alg ~ Digest SHA256
  => M.ToApplication ms (M.Effs ms core)
  => T.HasTxRouter (M.ApplicationC ms) (M.Effs ms core) 'Store.QueryAndMempool
  => T.HasTxRouter (M.ApplicationC ms) (BA.BaseAppEffs core) 'Store.QueryAndMempool
  => T.HasTxRouter (M.ApplicationD ms) (M.Effs ms core) 'Store.Consensus
  => T.HasTxRouter (M.ApplicationD ms) (BA.BaseAppEffs core) 'Store.Consensus
  => Q.HasQueryRouter (M.ApplicationQ ms) (M.Effs ms core)
  => Q.HasQueryRouter (M.ApplicationQ ms) (BA.BaseAppEffs core)
  => M.Eval ms core
  -- => M.Effs ms (BA.BaseAppEffs core) ~ (BA.AppEffs (M.ModulesEffs ms) core)
  => HandlersContext alg ms core
  -> App (Sem core)
makeApp handlersContext@HandlersContext{compileToCore} =
  let Handlers{..} = makeHandlers handlersContext :: Handlers (BA.BaseAppEffs core)
  in transformApp compileToCore $ App $ \case
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
