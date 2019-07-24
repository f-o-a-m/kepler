module Network.ABCI
  ( defaultLocalSettings
  , serveAppWith
  , serveApp
  ) where

import           Control.Lens                         ((^.))
import           Data.Conduit                         (ConduitT, runConduit,
                                                       (.|))
import qualified Data.Conduit.List                    as CL
import           Data.Conduit.Network                 (AppData, ServerSettings,
                                                       appSink, appSource,
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
import qualified Proto.Types                          as PT

import qualified Proto.Types_Fields                   as PT


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
  :: App IO
  -> Either Error.Error [PT.Request'Value]
  -> IO [PT.Response]
respondWith (App f) eReq =
  case eReq of
    Left err ->
      pure [Response.toProto $ Response.ResponseException $ Response.Exception $ cs $ Error.print err]
    Right reqs ->
      forM reqs $ Request.withProto $ fmap Response.toProto . f
