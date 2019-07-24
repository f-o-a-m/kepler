module Network.ABCI
  ( defaultLocalSettings
  , serveAppWith
  , serveApp
  ) where

import           Data.Conduit                         (ConduitT, runConduit,
                                                       (.|))
import qualified Data.Conduit.List                    as CL
import           Data.Conduit.Network                 (AppData, ServerSettings,
                                                       appSink, appSource,
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
import qualified Proto.Types                          as PT


-- | Default ABCI app network settings for serving on localhost at the
-- standard port.
defaultLocalSettings :: ServerSettings
defaultLocalSettings = serverSettings 26658 $ fromString "127.0.0.1"

-- | Serve an ABCI application with custom 'ServerSettings' and a custom
-- action to perform on aquiring the socket resource.
serveAppWith
  :: ServerSettings
  -> App IO
  -> (AppData -> IO ())
  -> IO ()
serveAppWith cfg app onAquire = runGeneralTCPServer cfg $ \appData -> do
  onAquire appData
  runConduit $ setupConduit app appData

-- | Serve an ABCI application with default local 'ServerSettings'
-- and a no-op on aquiring the socket resource.
serveApp
  :: App IO
  -> IO ()
serveApp app = serveAppWith defaultLocalSettings app (const $ pure ())

-- | Sets up the application wire pipeline.
setupConduit
  :: App IO
  -> AppData
  -> ConduitT i o IO ()
setupConduit app appData =
     appSource appData
  .| Wire.decodeLengthPrefixC
  .| CL.map (traverse PL.decodeMessage =<<)
  .| CL.mapM (respondWith app)
  .| CL.map (map PL.encodeMessage)
  .| Wire.encodeLengthPrefixC
  .| appSink appData

respondWith
  :: App IO
  -> Either String [PT.Request]
  -> IO [PT.Response]
respondWith app eReq =
  case eReq of
    Left err -> pure [makeResponseError ("Invalid request: " <> cs err)]
    Right reqs -> forM reqs $ \req ->
      Request.withProto req (run app)
  where
    run _app mParsedReq = case mParsedReq of
      Nothing        -> pure . makeResponseError $ "Invalid request"
      Just parsedReq -> Response.toProto <$> runApp _app parsedReq
    makeResponseError err =
      Response.toProto . Response.ResponseException  $ Response.Exception err
