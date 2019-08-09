module Network.ABCI.Server.Middleware.RequestLogger
    ( -- * Basic stdout logging
      mkLogStdout
    , mkLogStdoutDev
      -- * Custom Loggers
    , mkRequestLogger
    , mkRequestLoggerM
    ) where
import           Control.Lens            (at, (?~))
import           Control.Monad.IO.Class  (MonadIO, liftIO)
import qualified Data.Aeson              as A
import qualified Data.HashMap.Strict     as H
import           Data.Text               (Text)
import           Katip
import           Network.ABCI.Server.App (App (..), MessageType, Middleware,
                                          Request (..))
import           System.IO               (stdout)
---------------------------------------------------------------------------
-- Types
---------------------------------------------------------------------------
-- | Loggable newtype wrapper
newtype Loggable a = Loggable a

instance ToObject (Loggable (Request (t :: MessageType))) where
  toObject (Loggable v) =
    H.fromList ["type" A..= A.String (toMsgType v), "message" A..= A.toJSON v]
    where
      toMsgType :: forall t. Request (t :: MessageType) -> Text
      toMsgType (RequestEcho _)       = "echo"
      toMsgType (RequestFlush _)      = "flush"
      toMsgType (RequestInfo _)       = "info"
      toMsgType (RequestSetOption _)  = "set_option"
      toMsgType (RequestInitChain _)  = "init_chain"
      toMsgType (RequestQuery _)      = "query"
      toMsgType (RequestBeginBlock _) = "begin_block"
      toMsgType (RequestCheckTx _)    = "check_tx"
      toMsgType (RequestDeliverTx _)  = "deliver_tx"
      toMsgType (RequestEndBlock _)   = "end_block"
      toMsgType (RequestCommit _)     = "commit"

instance LogItem (Loggable (Request (t :: MessageType))) where
  payloadKeys V0 _ = SomeKeys ["message_type"]
  payloadKeys _ _  = AllKeys

---------------------------------------------------------------------------
-- mkLogStdout
--------------------------------------------------------------------------
-- | Creates a production request logger as middleware for ABCI requests.
-- Uses Lowest possible verbosity.
mkLogStdout :: (MonadIO m) => m (Middleware m)
mkLogStdout = do
  handleScribe <- liftIO $ mkHandleScribe ColorIfTerminal stdout InfoS V0
  le <- liftIO (registerScribe "stdout" handleScribe defaultScribeSettings
        =<< initLogEnv "ABCI" "production")
  let ns = "Server"
  pure $ mkRequestLogger le ns

---------------------------------------------------------------------------
-- mkLogStdoutDev
--------------------------------------------------------------------------
-- | Creates a request logger as middleware for ABCI requests.
-- Uses highest possible verbosity.
mkLogStdoutDev :: (MonadIO m) => m (Middleware m)
mkLogStdoutDev = do
  handleScribe <- liftIO $ mkHandleScribe ColorIfTerminal stdout DebugS V3
  le <- liftIO (registerScribe "stdout" handleScribe defaultScribeSettings
        =<< initLogEnv "ABCI" "development")
  let ns = "Server"
  pure $ mkRequestLogger le ns

---------------------------------------------------------------------------
-- mkRequestLogger
---------------------------------------------------------------------------
-- | Request logger middleware for ABCI requests with custom 'Katip.LogEnv'
-- and 'Katip.Namespace'. This method makes it easy use various scribes such as
-- <http://hackage.haskell.org/package/katip-elasticsearch-0.5.1.1/docs/Katip-Scribes-ElasticSearch.html elastic-search>.
mkRequestLogger :: (MonadIO m) => LogEnv -> Namespace -> Middleware m
mkRequestLogger le ns (App app) = App $ \ req -> do
  runKatipContextT le () ns $ logRequest req
  app req

---------------------------------------------------------------------------
-- mkRequestLoggerM
---------------------------------------------------------------------------
-- | Request logger middleware for ABCI requests in app with KatipContext.
-- Great for `App m` with a `KatipContext` instance.
mkRequestLoggerM :: (KatipContext m) => Middleware m
mkRequestLoggerM (App app) = App $ \ req -> logRequest req >> app req

---------------------------------------------------------------------------
-- Common
---------------------------------------------------------------------------
-- | Request logger function.
logRequest :: (KatipContext m) => Request t ->  m ()
logRequest req = katipAddContext (Loggable req) $ logFM InfoS "Request Received"
