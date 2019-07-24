module Network.ABCI where

import           Control.Lens                         ((^.))
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
import qualified Data.ProtoLens                       as PL
import           Data.String                          (fromString)
import           Data.String.Conversions              (cs)
import           Data.Text                            ()
import           Data.Traversable                     (forM, traverse)
import           Network.ABCI.Internal.Wire           as Wire
import           Network.ABCI.Types.App               (App (..))
import qualified Network.ABCI.Types.Error             as Error
import qualified Network.ABCI.Types.Messages.Request  as Request
import qualified Network.ABCI.Types.Messages.Response as Response
import           Network.Socket                       (SockAddr)
import qualified Proto.Types                          as PT
import qualified Proto.Types_Fields                   as PT
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
  .| CL.map (traverse decodeRequestValue =<<)
  .| CL.mapM (respondWith app)
  .| CL.map (map PL.encodeMessage)
  .| Wire.encodeLengthPrefixC
  .| appSink appData
  where
    decodeRequestValue input = case PL.decodeMessage input of
      Left parseError -> Left $ Error.CanNotDecodeRequest input parseError
      Right (request :: PT.Request) -> case request ^. PT.maybe'value of
        Nothing -> Left $ Error.NoValueInRequest input (request ^. PL.unknownFields)
        Just value -> Right $ value

respondWith
  :: ( Monad m
     , MonadIO m
     )
  => App m
  -> Either Error.Error [PT.Request'Value]
  -> m [PT.Response]
respondWith (App f) eReq =
  case eReq of
    Left err ->
      pure [Response.toProto $ Response.ResponseException $ Response.Exception $ cs $ Error.print err]
    Right reqs ->
      forM reqs $ Request.withProto $ fmap Response.toProto . f
