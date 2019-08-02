module Network.ABCI.Middleware.RequestLogger
    ( mkLogStdout
    , mkRequestLogger
    , mkRequestLoggerM
    ) where

import           Control.Monad.IO.Class              (MonadIO, liftIO)
import           Data.Aeson                          (FromJSON (..),
                                                      ToJSON (..),
                                                      genericParseJSON,
                                                      genericToJSON)
import           Data.Aeson.Casing                   (aesonPrefix, camelCase)
import           Katip
import           System.IO                           (stdout)


import           Network.ABCI.Types.App              (App (..), Middleware)
import           Network.ABCI.Types.Messages.Request (Request)

---------------------------------------------------------------------------
-- | Types
---------------------------------------------------------------------------
-- | Loggable newtype wrapper
newtype Loggable a = Loggable a
instance ToJSON (Loggable (Request t)) where
  toJSON (Loggable a) = toJSON a

instance ToObject (Loggable (Request t))
instance LogItem (Loggable (Request t)) where
  payloadKeys _ _ = AllKeys

---------------------------------------------------------------------------
-- | mkLogStdout
---------------------------------------------------------------------------
-- | Creates a production request katip logger as middleware for ABCI
-- requests.
mkLogStdout :: (MonadIO m) => m (Middleware m)
mkLogStdout = do
  handleScribe <- liftIO $ mkHandleScribe ColorIfTerminal stdout InfoS V2
  le <- liftIO (registerScribe "stdout" handleScribe defaultScribeSettings =<< initLogEnv "ABCI" "production")
  let ns = "Server"
  pure $ mkRequestLogger le ns

---------------------------------------------------------------------------
-- | mkRequestLogger
---------------------------------------------------------------------------
-- | Request logger middleware for ABCI requests with custom Katip LogEnv
-- and Namespace.
mkRequestLogger :: (MonadIO m) => LogEnv -> Namespace -> Middleware m
mkRequestLogger le ns (App app) = App $ \ req -> do
  runKatipContextT le () ns $ logRequest req
  app req

---------------------------------------------------------------------------
-- | mkRequestLoggerM
---------------------------------------------------------------------------
-- | Request logger middleware for ABCI requests in app with KatipContext.
mkRequestLoggerM :: (KatipContext m) => Middleware m
mkRequestLoggerM (App app) = App $ \ req -> logRequest req >> app req

---------------------------------------------------------------------------
-- | Common
---------------------------------------------------------------------------
-- | Request logger function.
logRequest :: (KatipContext m) => Request t ->  m ()
logRequest req = katipAddContext (Loggable req) $ logFM InfoS "Request Received"
