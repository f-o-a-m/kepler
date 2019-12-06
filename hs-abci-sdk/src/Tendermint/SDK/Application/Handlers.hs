module Tendermint.SDK.Application.Handlers
  ( Handler
  , HandlersContext(..)
  , makeApp
  ) where

import           Control.Lens                         (to, (&), (.~), (^.))
import           Crypto.Hash                          (Digest)
import           Crypto.Hash.Algorithms               (SHA256)
import qualified Data.ByteArray.Base64String          as Base64
import           Data.ByteString                      (ByteString)
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
import           Tendermint.SDK.Auth                  (AuthError)
import qualified Tendermint.SDK.BaseApp.BaseApp       as BA
import           Tendermint.SDK.BaseApp.Errors        (AppError,
                                                       checkTxAppError,
                                                       deliverTxAppError,
                                                       queryAppError)
import           Tendermint.SDK.BaseApp.Events        (withEventBuffer)
import           Tendermint.SDK.BaseApp.Query         (HasRouter)
import           Tendermint.SDK.BaseApp.Store         (ConnectionScope (..))
import qualified Tendermint.SDK.BaseApp.Store         as Store
import           Tendermint.SDK.Crypto                (RecoverableSignatureSchema,
                                                       SignatureSchema (..))
import           Tendermint.SDK.Types.TxResult        (TxResult,
                                                       checkTxTxResult,
                                                       deliverTxTxResult,
                                                       txResultEvents)

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
  { signatureAlgP    :: Proxy alg
  , modules          :: M.Modules ms r
  , compileToBaseApp :: forall a. Sem r a -> Sem (BA.BaseApp core) a
  , compileToCore    :: forall a. BA.ScopedEff core a -> Sem core a
  }

-- Common function between checkTx and deliverTx
makeHandlers
  :: forall alg ms r core.
     Member (Error AuthError) r
  => RecoverableSignatureSchema alg
  => Message alg ~ Digest SHA256
  => M.TxRouter ms r
  => M.QueryRouter ms r
  => HasRouter (M.Api ms)
  => Members BA.CoreEffs core
  => HandlersContext alg ms r core
  -> Handlers core
makeHandlers HandlersContext{..} =
  let
      txRouter =  M.txRouter signatureAlgP modules
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

      checkTx (RequestCheckTx _checkTx) = Store.applyScope $
        catch
          (do
            txResult <- transactionHandler txRouter $
              _checkTx ^. Req._checkTxTx . to Base64.toBytes
            return $ ResponseCheckTx $ def & checkTxTxResult .~ txResult
          )
          (\(err :: AppError) ->
            return . ResponseCheckTx $ def & checkTxAppError .~ err
          )

      deliverTx (RequestDeliverTx _deliverTx) = Store.applyScope $
        catch @AppError
          (do
            txResult <- transactionHandler txRouter $
              _deliverTx ^. Req._deliverTxTx . to Base64.toBytes
            return $ ResponseDeliverTx $ def & deliverTxTxResult .~ txResult
          )
          (\(err :: AppError) ->
            return . ResponseDeliverTx $ def & deliverTxAppError .~ err
          )

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
  where
    transactionHandler
      :: (ByteString -> Sem r ())
      -> ByteString
      -> Sem (BA.BaseApp core) TxResult
    transactionHandler txRouter bs = do
      events <- withEventBuffer . compileToBaseApp $ txRouter bs
      pure $ def & txResultEvents .~ events

makeApp
  :: forall alg ms r core.
     Member (Error AuthError) r
  => RecoverableSignatureSchema alg
  => Message alg ~ Digest SHA256
  => M.TxRouter ms r
  => M.QueryRouter ms r
  => HasRouter (M.Api ms)
  => Members BA.CoreEffs core
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
