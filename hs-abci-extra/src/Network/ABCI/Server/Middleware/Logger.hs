module Network.ABCI.Server.Middleware.Logger
    ( -- * Custom Loggers
      mkLogger
    , mkLoggerM
    ) where

import           Control.Monad.IO.Class    (MonadIO)
import           Control.Monad.Trans.Class (lift)
import qualified Data.Aeson                as A
import qualified Data.List.NonEmpty        as NE
import           Data.Semigroup            (sconcat)
import           Data.String               (fromString)
import           Katip
import           Network.ABCI.Server.App   (App (..), MessageType, Middleware,
                                            Request (..), Response (..),
                                            demoteRequestType, hashRequest,
                                            msgTypeKey, transformApp)

---------------------------------------------------------------------------
-- Types
---------------------------------------------------------------------------
-- | Loggable newtype wrapper
newtype Loggable a = Loggable a

instance ToObject (Loggable (Request (t :: MessageType))) where
  toObject (Loggable v) = case A.toJSON v of
      A.Object o -> o
      _          -> error "Contract violation: `toJSON` of any `Request t` must result with json object"

instance LogItem (Loggable (Request (t :: MessageType))) where
  payloadKeys V3 _ = AllKeys
  payloadKeys _ _  = SomeKeys ["type"]

instance ToObject (Loggable (Response (t :: MessageType))) where
  toObject (Loggable v) = case A.toJSON v of
      A.Object o -> o
      _          -> error "Contract violation: `toJSON` of any `Response t` must result with json object"

instance LogItem (Loggable (Response (t :: MessageType))) where
  payloadKeys V3 _ = AllKeys
  payloadKeys _ _  = SomeKeys ["type"]

---------------------------------------------------------------------------
-- mkLogger
---------------------------------------------------------------------------
-- | Logger middleware for ABCI messages with custom 'Katip.LogEnv'
-- and 'Katip.Namespace'. This method makes it easy use various scribes such as
-- <http://hackage.haskell.org/package/katip-elasticsearch-0.5.1.1/docs/Katip-Scribes-ElasticSearch.html elastic-search>.
mkLogger
  :: MonadIO m
  => LogEnv
  -> Namespace
  -> Middleware m
mkLogger le ns =
  transformApp (runKatipContextT le () ns) . mkLoggerM . transformApp lift

---------------------------------------------------------------------------
-- mkLoggerM
---------------------------------------------------------------------------
-- | Logger middleware for ABCI messages in app with KatipContext.
-- Great for `App m` with a `KatipContext` instance.
mkLoggerM
  :: KatipContext m
  => Middleware m
mkLoggerM (App app) = App $ \ req -> do
  let globalContext = sconcat . NE.fromList $
        [ liftPayload $ sl "message_type" (msgTypeKey $ demoteRequestType req)
        , liftPayload $ sl "message_id" (hashRequest req)
        ]
  katipAddContext globalContext $ do
    katipAddNamespace (fromString "server") $
      logRequest req
    resp <- katipAddNamespace (fromString "application") $
      app req
    katipAddNamespace (fromString "server") $
      logResponse resp
    return resp

---------------------------------------------------------------------------
-- Common
---------------------------------------------------------------------------

-- | Request logger function.
logRequest
  :: KatipContext m
  => Request t
  ->  m ()
logRequest req = katipAddContext (Loggable req) $
  logFM logLevel "Request Received"
  where
    logLevel = case req of
      RequestFlush _ -> DebugS
      RequestEcho _  -> DebugS
      _              -> InfoS

logResponse
  :: KatipContext m
  => Response t
  ->  m ()
logResponse resp = katipAddContext (Loggable resp) $
  logFM logLevel "Response Sent"
  where
    logLevel = case resp of
      ResponseFlush _ -> DebugS
      ResponseEcho _  -> DebugS
      _               -> InfoS
