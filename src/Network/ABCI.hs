module Network.ABCI where

import           Data.Conduit                         (ConduitT, runConduit,
                                                       (.|))
import qualified Data.Conduit.List                    as CL
import           Data.Conduit.Network                 (AppData, ServerSettings,
                                                       appSink,
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
import qualified Proto.Types                          as PT


-- | Default ABCI app network settings.
defaultLocalSettings :: ServerSettings
defaultLocalSettings = serverSettings 26658 $ fromString "127.0.0.1"

-- | Serve an ABCI application with custom 'ServerSettings'
serveAppWith
  :: Monad m
  => ServerSettings
  -> (forall a. m a -> IO a)
  -> App m
  -> (AppData -> IO ())
  -> IO ()
serveAppWith cfg nat app onAquire = runGeneralTCPServer cfg $ \appData -> do
  onAquire appData
  runConduit $ setupConduit app nat appData

-- | Serve an ABCI application with default 'ServerSettings'
serveApp
  :: Monad m
  => (forall a. m a -> IO a)
  -> App m
  -> (AppData -> IO ())
  -> IO ()
serveApp = serveAppWith defaultLocalSettings

-- | Sets up the application wire pipeline.
setupConduit
  :: Monad m
  => App m
  -> (forall a. m a -> IO a)
  -> AppData
  -> ConduitT i o IO ()
setupConduit app nat appData =
     appSource appData
  .| Wire.decodeLengthPrefixC
  .| CL.map (traverse PL.decodeMessage =<<)
  .| CL.mapM (respondWith app nat)
  .| CL.map (map PL.encodeMessage)
  .| Wire.encodeLengthPrefixC
  .| appSink appData

respondWith
  :: Monad m
  => App m
  -> (forall a. m a -> IO a)
  -> Either String [PT.Request]
  -> IO [PT.Response]
respondWith app nat eReq =
  case eReq of
    Left err -> pure [makeResponseError ("Invalid request: " <> cs err)]
    Right reqs -> nat $ forM reqs $ \req ->
      Request.withProto req (runApp app)
  where
    runApp (App f) mParsedReq = case mParsedReq of
      Nothing        -> pure . makeResponseError $ "Invalid request"
      Just parsedReq -> Response.toProto <$> f parsedReq
    makeResponseError err =
      Response.toProto . Response.ResponseException  $ Response.Exception err
