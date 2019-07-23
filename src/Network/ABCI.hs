module Network.ABCI where

import           Control.Monad.IO.Class               (MonadIO (..))
import           Control.Monad.Trans.Control          (MonadBaseControl)
import           Data.Conduit                         (ConduitT, runConduit,
                                                       (.|))
import qualified Data.Conduit.List                    as CL
import           Data.Conduit.Network                 (AppData, ServerSettings,
                                                       appSink, appSockAddr,
                                                       appSource,
                                                       runGeneralTCPServer,
                                                       serverSettings)
import           Data.Monoid                          ((<>))
import qualified Data.ProtoLens.Encoding              as PL
import           Data.String                          (fromString)
import           Data.String.Conversions              (cs)
import           Data.Text                            ()
import           Data.Traversable                     (forM, traverse)
import           Network.ABCI.Internal.Wire           as Wire
import           Network.ABCI.Types.App               (App (..))
import qualified Network.ABCI.Types.Messages.Request  as Request
import qualified Network.ABCI.Types.Messages.Response as Response
import           Network.Socket                       (SockAddr)
import qualified Proto.Types                          as PT
import           UnliftIO                             (MonadUnliftIO)



-- | Default ABCI app network settings.
defaultSettings :: ServerSettings
defaultSettings = serverSettings 26658 $ fromString "127.0.0.1"

-- | Serve an ABCI application with custom 'ServerSettings'
serveAppWith
  :: ( MonadIO m
     , MonadBaseControl IO m
     , MonadUnliftIO m
     )
  => ServerSettings
  -> (SockAddr -> m (App m))
  -> m ()
serveAppWith cfg mkApp = runGeneralTCPServer cfg $ \appData -> do
  app <- mkApp (appSockAddr appData)
  runConduit (setupConduit app appData)

-- | Serve an ABCI application with default 'ServerSettings'
serveApp
  :: ( MonadIO m
     , MonadBaseControl IO m
     , MonadUnliftIO m
     )
  => (SockAddr -> m (App m))
  -> m ()
serveApp = serveAppWith defaultSettings

-- | Sets up the application wire pipeline.
setupConduit
  :: ( MonadIO m
     , MonadUnliftIO m
     )
  => App m
  -> AppData
  -> ConduitT i o m ()
setupConduit app appData =
     appSource appData
  .| Wire.decodeLengthPrefixC
  .| CL.map (traverse PL.decodeMessage =<<)
  .| CL.mapM (respondWith app)
  .| CL.map (map PL.encodeMessage)
  .| Wire.encodeLengthPrefixC
  .| appSink appData

respondWith
  :: ( Monad m
     , MonadIO m
     )
  => App m
  -> Either String [PT.Request]
  -> m [PT.Response]
respondWith app eReq =
  case eReq of
    Left err -> pure [makeResponseError ("Invalid request: " <> cs err)]
    Right reqs -> forM reqs $ \req ->
      Request.withProto req (runApp app)
  where
    runApp (App f) mParsedReq = case mParsedReq of
      Nothing        -> pure . makeResponseError $ "Invalid request"
      Just parsedReq -> Response.toProto <$> f parsedReq
    makeResponseError err =
      Response.toProto . Response.ResponseException  $ Response.Exception err
