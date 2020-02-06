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
import           Data.Singletons
import           Network.ABCI.Server.App                (App (..),
                                                         MessageType (..),
                                                         Request (..),
                                                         Response (..))
import qualified Network.ABCI.Types.Messages.Request    as Req
import qualified Network.ABCI.Types.Messages.Response   as Resp
import           Polysemy
import           Polysemy.Error                         (Error, catch)
import           Tendermint.SDK.Application.AnteHandler (AnteHandler,
                                                         applyAnteHandler)
import qualified Tendermint.SDK.Application.Module      as M
import qualified Tendermint.SDK.BaseApp                 as BA
import           Tendermint.SDK.BaseApp.CoreEff         (CoreEffs)
import           Tendermint.SDK.BaseApp.Errors          (AppError,
                                                         SDKError (..),
                                                         queryAppError,
                                                         throwSDKError,
                                                         txResultAppError)
import qualified Tendermint.SDK.BaseApp.Query           as Q
import           Tendermint.SDK.BaseApp.Store           (ConnectionScope (..))
import qualified Tendermint.SDK.BaseApp.Store           as Store
import qualified Tendermint.SDK.BaseApp.Store.IAVLStore as IAVL
import           Tendermint.SDK.BaseApp.Transaction     as T
import           Tendermint.SDK.Crypto                  (RecoverableSignatureSchema,
                                                         SignatureSchema (..))
import           Tendermint.SDK.Types.Transaction       (parseTx)
import           Tendermint.SDK.Types.TxResult          (checkTxTxResult,
                                                         deliverTxTxResult)


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
  , modules       :: M.Modules ms r
  , compileToCore :: forall a. Sem (BA.BaseAppEffs BA.:& core) a -> Sem core a
  , anteHandler   :: AnteHandler r
  }

-- Common function between checkTx and deliverTx
makeHandlers
  :: forall alg ms r core.
     Member (Error AppError) r
  => RecoverableSignatureSchema alg
  => Message alg ~ Digest SHA256
  => M.AppTxRouter ms r 'T.DeliverTx
  => M.AppTxRouter ms r 'T.CheckTx
  => M.AppQueryRouter ms r
  => Q.HasQueryRouter (M.QApi ms) r
  => T.HasTxRouter (M.TApi ms) r 'T.DeliverTx
  => T.HasTxRouter (M.TApi ms) r 'T.CheckTx
  => Members CoreEffs core
  => M.Eval ms core
  => M.Effs ms core ~ r
  => HandlersContext alg ms r core
  -> Handlers core
makeHandlers HandlersContext{..} =
  let
      compileToBaseApp :: forall a. Sem r a -> Sem (BA.BaseAppEffs BA.:& core) a
      compileToBaseApp = IAVL.evalWrite undefined  . M.eval modules

      queryRouter = compileToBaseApp . M.appQueryRouter modules

      txParser bs = case parseTx signatureAlgP bs of
        Left err -> throwSDKError $ ParseError err
        Right tx -> pure $ T.RoutingTx tx

      txRouter ctx bs = compileToBaseApp $ do
        let router = applyAnteHandler anteHandler $ M.appTxRouter modules ctx
        tx <- txParser bs
        router tx

      query (RequestQuery q) =
        --Store.applyScope $
        catch
          (do
            queryResp <- queryRouter q
            pure $ ResponseQuery queryResp
          )
          (\(err :: AppError) ->
            return . ResponseQuery $ def & queryAppError .~ err
          )

      --beginBlock _ = Store.applyScope (def <$ Store.beginBlock)

      checkTx (RequestCheckTx _checkTx) =  do
        res <- catch
          ( let txBytes =  _checkTx ^. Req._checkTxTx . to Base64.toBytes
            in  txRouter (sing :: Sing 'T.CheckTx) txBytes
          )
          (\(err :: AppError) ->
            return $ def & txResultAppError .~ err
          )
        return . ResponseCheckTx $ res ^. from checkTxTxResult

      deliverTx (RequestDeliverTx _deliverTx) = do
        res <- catch @AppError
          ( let txBytes = _deliverTx ^. Req._deliverTxTx . to Base64.toBytes
            in txRouter (sing :: Sing 'T.CheckTx) txBytes
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
  => M.AppTxRouter ms r 'DeliverTx
  => M.AppTxRouter ms r 'CheckTx
  => M.AppQueryRouter ms r
  => Q.HasQueryRouter (M.QApi ms) r
  => T.HasTxRouter (M.TApi ms) r 'T.DeliverTx
  => T.HasTxRouter (M.TApi ms) r 'T.CheckTx
  => Members CoreEffs core
  => M.Eval ms core
  => M.Effs ms core ~ r
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
