module Tendermint.SDK.Handlers where

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
import           Tendermint.SDK.Auth                  (AuthError)
import qualified Tendermint.SDK.BaseApp               as BA
import           Tendermint.SDK.Crypto                (RecoverableSignatureSchema,
                                                       SignatureSchema (..))
import           Tendermint.SDK.Errors                (AppError,
                                                       checkTxAppError,
                                                       deliverTxAppError)
import           Tendermint.SDK.Events                (withEventBuffer)
import           Tendermint.SDK.Module
import           Tendermint.SDK.Store                 (ConnectionScope (..))
import qualified Tendermint.SDK.Store                 as Store
import qualified Tendermint.SDK.TxRouter              as R
import           Tendermint.SDK.Types.TxResult        (TxResult,
                                                       checkTxTxResult,
                                                       deliverTxTxResult,
                                                       txResultEvents)

data Handlers r = Handlers
  { info       :: Request 'MTInfo -> Sem (BA.ScopedBaseApp 'Query r) (Response 'MTInfo)
  , setOption  :: Request 'MTSetOption -> Sem (BA.ScopedBaseApp 'Query r)  (Response 'MTSetOption)
  , initChain  :: Request 'MTInitChain -> Sem (BA.ScopedBaseApp 'Consensus r) (Response 'MTInitChain)
  , query      :: Request 'MTQuery -> Sem (BA.ScopedBaseApp 'Query r) (Response 'MTQuery)
  , checkTx    :: Request 'MTCheckTx -> Sem (BA.ScopedBaseApp 'Mempool r) (Response 'MTCheckTx)
  , beginBlock :: Request 'MTBeginBlock -> Sem (BA.ScopedBaseApp 'Consensus r) (Response 'MTBeginBlock)
  , deliverTx  :: Request 'MTDeliverTx -> Sem (BA.ScopedBaseApp 'Consensus r) (Response 'MTDeliverTx)
  , endBlock   :: Request 'MTEndBlock -> Sem (BA.ScopedBaseApp 'Consensus r) (Response 'MTEndBlock)
  , commit     :: Request 'MTCommit -> Sem (BA.ScopedBaseApp 'Consensus r) (Response 'MTCommit)
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

data HandlersContext alg ms r core = HandlersContext
  { signatureAlgP    :: Proxy alg
  , modules          :: Modules ms r
  , compileToBaseApp :: forall a. Sem r a -> Sem (BA.BaseApp core) a
  , compileBaseAppToCore :: forall a scope. Sem (BA.ScopedBaseApp scope core) a -> Sem core a
  }

-- Common function between checkTx and deliverTx
makeHandlers
  :: forall alg ms r core.
     Member (Error AuthError) r
  => RecoverableSignatureSchema alg
  => Message alg ~ Digest SHA256
  => R.Router ms r
  => Members BA.CoreEffs core
  => HandlersContext alg ms r core
  -> Handlers core
makeHandlers HandlersContext{..} =
  let
      router =  R.router signatureAlgP modules

      beginBlock _ = Store.applyScope (def <$ Store.beginBlock)

      checkTx (RequestCheckTx _checkTx) = Store.applyScope $
        catch
          (do
            txResult <- transactionHandler router $
              _checkTx ^. Req._checkTxTx . to Base64.toBytes
            return $ ResponseCheckTx $ def & checkTxTxResult .~ txResult
          )
          (\(err :: AppError) ->
            return . ResponseCheckTx $ def & checkTxAppError .~ err
          )

      deliverTx (RequestDeliverTx _deliverTx) = Store.applyScope $
        catch @AppError
          (do
            txResult <- transactionHandler router $
              _deliverTx ^. Req._deliverTxTx . to Base64.toBytes
            return $ ResponseDeliverTx $ def & deliverTxTxResult .~ txResult
          )
          (\(err :: AppError) ->
            return . ResponseDeliverTx $ def & deliverTxAppError .~ err
          )

      commit :: Request 'MTCommit -> Sem (BA.ScopedBaseApp 'Consensus core) (Response 'MTCommit)
      commit _ = Store.applyScope $ do
        Store.commitBlock
        Store.mergeScopes
        rootHash <- Store.storeRoot
        return . ResponseCommit $ def
          & Resp._commitData .~ Base64.fromBytes rootHash

  in defaultHandlers
       { beginBlock = beginBlock
       , checkTx = checkTx
       , deliverTx = deliverTx
       , commit = commit
       }
  where
    transactionHandler
      :: (ByteString -> Sem r ())
      -> ByteString
      -> Sem (BA.BaseApp core) TxResult
    transactionHandler router bs = do
      events <- withEventBuffer . compileToBaseApp $ router bs
      pure $ def & txResultEvents .~ events

makeApp
 :: Handlers core
 -> (forall a scope. Sem (BA.ScopedBaseApp scope core) a -> Sem core a)
 -> App (Sem core)
makeApp Handlers{..} compileToCore =
  App $ \case
    RequestEcho echo ->
      pure . ResponseEcho $ def
        & Resp._echoMessage .~ echo ^. Req._echoMessage
    RequestFlush _ -> pure def
    msg@(RequestInfo _) -> compileToCore $ info msg
    msg@(RequestSetOption _) -> compileToCore $ setOption msg
    msg@(RequestInitChain _) ->  compileToCore $ initChain msg
    msg@(RequestQuery _) -> compileToCore $ query msg
    msg@(RequestBeginBlock _) -> compileToCore $ beginBlock msg
    msg@(RequestCheckTx _) -> compileToCore $ checkTx msg
    msg@(RequestDeliverTx _) -> compileToCore $ deliverTx msg
    msg@(RequestEndBlock _) -> compileToCore $ endBlock msg
    msg@(RequestCommit _) ->  compileToCore $ commit msg
