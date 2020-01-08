module Tendermint.SDK.Application.Handlers
  ( Handler
  , HandlersContext(..)
  , makeApp
  ) where

import           Control.Lens                         (from, to, (&), (.~),
                                                       (^.))
import           Crypto.Hash                          (Digest)
import           Crypto.Hash.Algorithms               (SHA256)
import qualified Data.ByteArray.Base64String          as Base64
import           Data.Default.Class                   (Default (..))
import           Data.Proxy
import           Network.ABCI.Server.App              (App (..),
                                                       MessageType (..),
                                                       Request (..),
                                                       Response (..))
import qualified Network.ABCI.Types.Messages.Request  as Req
import qualified Network.ABCI.Types.Messages.Response as Resp
import           Polysemy
import           Polysemy.Error                       (Error, catch)
import qualified Tendermint.SDK.Application.Module    as M
import qualified Tendermint.SDK.BaseApp.BaseApp       as BA
import           Tendermint.SDK.BaseApp.CoreEff       (CoreEffs)
import           Tendermint.SDK.BaseApp.Errors        (AppError, SDKError (..),
                                                       queryAppError,
                                                       throwSDKError,
                                                       txResultAppError)
import           Tendermint.SDK.BaseApp.Query         (HasRouter)
import           Tendermint.SDK.BaseApp.Store         (ConnectionScope (..))
import qualified Tendermint.SDK.BaseApp.Store         as Store
import           Tendermint.SDK.Codec                 (HasCodec (..))
import           Tendermint.SDK.Crypto                (RecoverableSignatureSchema,
                                                       SignatureSchema (..))
import qualified Tendermint.SDK.Modules.Auth          as A
-- import           Tendermint.SDK.Types.Message         (Msg (..))
-- import           Tendermint.SDK.Types.Transaction     (RawTransaction (..),
--                                                        Tx (..), parseTx)
import           Tendermint.SDK.Types.TxResult        (checkTxTxResult,
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
  }

-- check/validate nonce
-- @TODO: don't parse the tx twice
-- probably need to change the args for txRouter
-- txAnteHandler
--   :: forall alg r.
--      Member A.Accounts r
--   => RawTransaction
--   -> Sem r ()
-- txAnteHandler (p :: Proxy alg) rawTx = do
--   let eTx = parseTx p rawTx
--   case eTx of
--     Left errMsg -> throwSDKError $ ParseError ("Transaction ParseError: " <> errMsg)
--     Right Tx{txMsg, txNonce} -> do
--       let Msg{msgAuthor} = txMsg
--       mAcnt <- A.getAccount msgAuthor
--       case mAcnt of
--         -- this should be probably be an error
--         -- but it doesn't make much sense to have a tx without an acc
--         -- signing it first
--         Nothing -> pure ()
--         Just A.Account{accountNonce} -> do
--           -- make sure nonce is strictly less than
--           -- for more strict validation, txNonce should be accountNonce + 1
--           if accountNonce < txNonce
--             then pure ()
--             else throwSDKError NonceException

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

      compileRawTx context = compileToBaseApp . M.txRouter signatureAlgP context modules
      txRouter context = either (throwSDKError . ParseError) (compileRawTx context) . decode
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
