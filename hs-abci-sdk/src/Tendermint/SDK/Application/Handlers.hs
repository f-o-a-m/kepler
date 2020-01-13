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
import           Tendermint.SDK.Application.AnteHandler (AnteHandler,
                                                         applyAnteHandler)
import qualified Tendermint.SDK.Application.Module      as M
import qualified Tendermint.SDK.BaseApp.BaseApp         as BA
import           Tendermint.SDK.BaseApp.CoreEff         (CoreEffs)
import           Tendermint.SDK.BaseApp.Errors          (AppError,
                                                         SDKError (..),
                                                         queryAppError,
                                                         throwSDKError,
                                                         txResultAppError)
import           Tendermint.SDK.BaseApp.Query           (HasRouter)
import           Tendermint.SDK.BaseApp.Store           (ConnectionScope (..))
import qualified Tendermint.SDK.BaseApp.Store           as Store
import           Tendermint.SDK.Crypto                  (RecoverableSignatureSchema,
                                                         SignatureSchema (..))
import qualified Tendermint.SDK.Modules.Auth            as A
import           Tendermint.SDK.Types.Transaction       (PreRoutedTx (..),
                                                         parseTx)
import           Tendermint.SDK.Types.TxResult          (checkTxTxResult,
                                                         deliverTxTxResult)


type Handler mt r = Request mt -> Sem r (Response mt)

data Handlers core = Handlers
  { info       :: Handler 'MTInfo (BA.ScopedBaseApp 'Query core)
  , setOption  :: Handler 'MTSetOption (BA.ScopedBaseApp 'Query core)
  , initChain  :: Handler 'MTInitChain (BA.ScopedBaseApp 'Consensus core)
  , query      :: Handler 'MTQuery (BA.ScopedBaseApp 'Query core)
  , checkTx    :: Handler 'MTCheckTx (BA.ScopedBaseApp 'Mempool core)
  , beginBlock :: Handler 'MTBeginBlock (BA.ScopedBaseApp 'Consensus core)
  , deliverTx  :: Handler 'MTDeliverTx (BA.ScopedBaseApp 'Consensus core)
  , endBlock   :: Handler 'MTEndBlock (BA.ScopedBaseApp 'Consensus core)
  , commit     :: Handler 'MTCommit (BA.ScopedBaseApp 'Consensus core)
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
  , compileToCore :: forall a. BA.ScopedEff core a -> Sem core a
  , anteHandler   :: AnteHandler r
  }

-- Common function between checkTx and deliverTx
makeHandlers
  :: forall alg ms r core.
     Members A.AuthEffs r
  => Member (Error AppError) r
  => RecoverableSignatureSchema alg
  => Message alg ~ Digest SHA256
  => M.TxRouter ms r
  => M.QueryRouter ms r
  => HasRouter (M.Api ms)
  => Members CoreEffs core
  => M.Eval ms core
  => M.Effs ms core ~ r
  => HandlersContext alg ms r core
  -> Handlers core
makeHandlers HandlersContext{..} =
  let
      compileToBaseApp :: forall a. Sem r a -> Sem (BA.BaseApp core) a
      compileToBaseApp = M.eval modules
      routerWithAH context = applyAnteHandler anteHandler $ M.txRouter context modules
      txRouter context bs = case parseTx signatureAlgP bs of
        Left err -> throwSDKError $ ParseError err
        Right tx -> compileToBaseApp $ M.runRouter (routerWithAH context) (PreRoutedTx tx)
      queryRouter = compileToBaseApp . M.queryRouter modules

      query (RequestQuery q) = Store.applyScope $
        catch
          (do
            queryResp <- queryRouter q
            pure $ ResponseQuery queryResp
          )
          (\(err :: AppError) ->
            return . ResponseQuery $ def & queryAppError .~ err
          )

      beginBlock _ = Store.applyScope (def <$ Store.beginBlock)

      checkTx (RequestCheckTx _checkTx) = Store.applyScope $ do
        res <- catch
          ( let txBytes =  _checkTx ^. Req._checkTxTx . to Base64.toBytes
            in txRouter M.CheckTxContext txBytes
          )
          (\(err :: AppError) ->
            return $ def & txResultAppError .~ err
          )
        return . ResponseCheckTx $ res ^. from checkTxTxResult

      deliverTx (RequestDeliverTx _deliverTx) = Store.applyScope $ do
        res <- catch @AppError
          ( let txBytes = _deliverTx ^. Req._deliverTxTx . to Base64.toBytes
            in txRouter M.DeliverTxContext txBytes
          )
          (\(err :: AppError) ->
            return $ def & txResultAppError .~ err
          )
        return . ResponseDeliverTx $ res ^. from deliverTxTxResult

      commit :: Handler 'MTCommit (BA.ScopedBaseApp 'Consensus core)
      commit _ = Store.applyScope $ do
        Store.commitBlock
        Store.mergeScopes
        rootHash <- Store.storeRoot
        return . ResponseCommit $ def
          & Resp._commitData .~ Base64.fromBytes rootHash

  in defaultHandlers
       { query = query
       , checkTx = checkTx
       , beginBlock = beginBlock
       , deliverTx = deliverTx
       , commit = commit
       }

makeApp
  :: forall alg ms r core.
     Members A.AuthEffs r
  => Members [Error AppError, Embed IO] r
  => RecoverableSignatureSchema alg
  => Message alg ~ Digest SHA256
  => M.TxRouter ms r
  => M.QueryRouter ms r
  => HasRouter (M.Api ms)
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
       msg@(RequestInfo _) -> compileToCore . BA.QueryScoped $  info msg
       msg@(RequestSetOption _) -> compileToCore . BA.QueryScoped $ setOption msg
       msg@(RequestInitChain _) ->  compileToCore . BA.ConsensusScoped $ initChain msg
       msg@(RequestQuery _) -> compileToCore . BA.QueryScoped $ query msg
       msg@(RequestBeginBlock _) -> compileToCore . BA.ConsensusScoped $ beginBlock msg
       msg@(RequestCheckTx _) -> compileToCore . BA.MempoolScoped $ checkTx msg
       msg@(RequestDeliverTx _) -> compileToCore . BA.ConsensusScoped $ deliverTx msg
       msg@(RequestEndBlock _) -> compileToCore . BA.ConsensusScoped $ endBlock msg
       msg@(RequestCommit _) ->  compileToCore . BA.ConsensusScoped $ commit msg
