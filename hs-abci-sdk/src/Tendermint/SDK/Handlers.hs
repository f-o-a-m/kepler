module Tendermint.SDK.Handlers where

import Control.Lens ((^.), (&), (.~), to)
import Polysemy
import qualified Tendermint.SDK.TxRouter as R
import qualified Tendermint.SDK.BaseApp as BA
import qualified Tendermint.SDK.Store.Scope as Scope
import Tendermint.SDK.Module
import qualified Network.ABCI.Types.Messages.Request  as Req
import Tendermint.SDK.Errors (AppError, checkTxAppError, deliverTxAppError)
import Tendermint.SDK.Events (withEventBuffer)
import qualified Network.ABCI.Types.Messages.Response as Resp
import           Data.Default.Class                   (Default (..))
import qualified Data.ByteArray.Base64String as Base64
import Data.ByteString (ByteString)
import           Crypto.Hash                      (Digest)
import Data.Proxy
import Polysemy.Error (Error, catch)
import           Tendermint.SDK.Crypto            (RecoverableSignatureSchema,
                                                   SignatureSchema (..))
import           Crypto.Hash.Algorithms           (SHA256)
import Tendermint.SDK.Auth (AuthError)
import           Network.ABCI.Server.App              (App (..),
                                                       MessageType (..),
                                                       Request (..),
                                                       Response (..))
import           Tendermint.SDK.Types.TxResult        (TxResult,
                                                       checkTxTxResult,
                                                       deliverTxTxResult,
                                                       txResultEvents)

data Handlers r = Handlers
  { info :: Request 'MTInfo -> Sem r (Response 'MTInfo)
  , setOption :: Request 'MTSetOption -> Sem r (Response 'MTSetOption)
  , initChain :: Request 'MTInitChain -> Sem r (Response 'MTInitChain)
  , query :: Request 'MTQuery -> Sem r (Response 'MTQuery)
  , checkTx :: Request 'MTCheckTx -> Sem r (Response 'MTCheckTx)
  , beginBlock :: Request 'MTBeginBlock -> Sem r (Response 'MTBeginBlock)
  , deliverTx :: Request 'MTDeliverTx -> Sem r (Response 'MTDeliverTx)
  , endBlock :: Request 'MTEndBlock -> Sem r (Response 'MTEndBlock)
  , commit :: Request 'MTCommit -> Sem r (Response 'MTCommit)
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
  , modules :: Modules ms r
  , compileToBaseApp :: forall a. Sem r a -> Sem BA.BaseApp a
  }

-- Common function between checkTx and deliverTx
makeHandlers 
  :: forall alg ms r .
     Member (Error AuthError) r
  => RecoverableSignatureSchema alg
  => Message alg ~ Digest SHA256
  => R.Router ms r
  => HandlersContext alg ms r
  -> Handlers BA.BaseApp
makeHandlers HandlersContext{..} = 
  let 
      router =  R.router signatureAlgP modules

      checkTx (RequestCheckTx _checkTx) =
        catch
          (do
            txResult <- transactionHandler router $ 
              _checkTx ^. Req._checkTxTx . to Base64.toBytes
            return $ ResponseCheckTx $ def & checkTxTxResult .~ txResult
          )
          (\(err :: AppError) ->
            return . ResponseCheckTx $ def & checkTxAppError .~ err
          )

      deliverTx (RequestDeliverTx _deliverTx) =
        catch @AppError
          (do
            txResult <- transactionHandler router $ 
              _deliverTx ^. Req._deliverTxTx . to Base64.toBytes
            return $ ResponseDeliverTx $ def & deliverTxTxResult .~ txResult
          )
          (\(err :: AppError) ->
            return . ResponseDeliverTx $ def & deliverTxAppError .~ err
          )

  in defaultHandlers
       { checkTx = checkTx
       , deliverTx = deliverTx
       }
  where
    transactionHandler 
      :: (ByteString -> Sem r ())
      -> ByteString
      -> Sem BA.BaseApp TxResult
    transactionHandler router bs = do 
      events <- withEventBuffer . compileToBaseApp $ router bs
      pure $ def & txResultEvents .~ events

makeApp 
 :: Handlers r 
 -> App (Sem r) 
makeApp Handlers{..} = 
  App $ \case
    RequestEcho echo ->
      pure . ResponseEcho $ def 
        & Resp._echoMessage .~ echo ^. Req._echoMessage
    RequestFlush _ -> pure def
    msg@(RequestInfo _) -> info msg
    msg@(RequestSetOption _) -> setOption msg
    msg@(RequestInitChain _) -> initChain msg
    msg@(RequestQuery _) -> query msg
    msg@(RequestBeginBlock _) -> beginBlock msg
    msg@(RequestCheckTx _) -> checkTx msg
    msg@(RequestDeliverTx _) -> deliverTx msg
    msg@(RequestEndBlock _) -> endBlock msg
    msg@(RequestCommit _) -> commit msg