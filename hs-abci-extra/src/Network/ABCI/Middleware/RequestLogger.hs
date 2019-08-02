module Network.ABCI.Middleware.RequestLogger
    ( mkLogStdout
    , mkLogStdoutDev
    , mkRequestLogger
    , mkRequestLoggerM
    ) where

import           Control.Lens                        (at, (?~))
import           Control.Monad.IO.Class              (MonadIO, liftIO)
import qualified Data.Aeson                          as A
import           Katip
import           System.IO                           (stdout)


import           Network.ABCI.Types.App              (App (..), Middleware)
import           Network.ABCI.Types.Messages.Request (MessageType (..),
                                                      Request (..))

---------------------------------------------------------------------------
-- Types
---------------------------------------------------------------------------
-- | Loggable newtype wrapper
newtype Loggable a = Loggable a

instance ToObject (Loggable (Request (t :: MessageType))) where
  toObject (Loggable v) = case A.toJSON v of
    A.Object o -> addMessageType v o
    -- unreachable case
    _          -> mempty
   where
    addMessageType :: forall t. Request (t :: MessageType) -> A.Object -> A.Object
    addMessageType (RequestEcho _) o       = at "message_type" ?~ A.String "echo" $ o
    addMessageType (RequestFlush _) o      = at "message_type" ?~ A.String "flush" $ o
    addMessageType (RequestInfo _) o       = at "message_type" ?~ A.String "info" $ o
    addMessageType (RequestSetOption _) o  = at "message_type" ?~ A.String "set_option" $ o
    addMessageType (RequestInitChain _) o  = at "message_type" ?~ A.String "init_chain" $ o
    addMessageType (RequestQuery _) o      = at "message_type" ?~ A.String "query" $ o
    addMessageType (RequestBeginBlock _) o = at "message_type" ?~ A.String "begin_block" $ o
    addMessageType (RequestCheckTx _) o    = at "message_type" ?~ A.String "check_tx" $ o
    addMessageType (RequestDeliverTx _) o  = at "message_type" ?~ A.String "deliver_tx" $ o
    addMessageType (RequestEndBlock _) o   = at "message_type" ?~ A.String "end_block" $ o
    addMessageType (RequestCommit _) o     = at "message_type" ?~ A.String "commit" $ o

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
        =<< initLogEnv "ABCI" "production")
  let ns = "Server"
  pure $ mkRequestLogger le ns

---------------------------------------------------------------------------
-- mkRequestLogger
---------------------------------------------------------------------------
-- | Request logger middleware for ABCI requests with custom Katip LogEnv
-- and Namespace.
mkRequestLogger :: (MonadIO m) => LogEnv -> Namespace -> Middleware m
mkRequestLogger le ns (App app) = App $ \ req -> do
  runKatipContextT le () ns $ logRequest req
  app req

---------------------------------------------------------------------------
-- mkRequestLoggerM
---------------------------------------------------------------------------
-- | Request logger middleware for ABCI requests in app with KatipContext.
mkRequestLoggerM :: (KatipContext m) => Middleware m
mkRequestLoggerM (App app) = App $ \ req -> logRequest req >> app req

---------------------------------------------------------------------------
-- Common
---------------------------------------------------------------------------
-- | Request logger function.
logRequest :: (KatipContext m) => Request t ->  m ()
logRequest req = katipAddContext (Loggable req) $ logFM InfoS "Request Received"
