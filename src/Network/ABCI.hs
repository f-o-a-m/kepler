module Network.ABCI where

import           Control.Lens                         ((^.))
import           Control.Monad.Trans.Class            (lift)
import           Control.FromSum                      (fromEither)
import           Control.Monad.Trans.Except           (runExceptT, except)
import qualified Data.ByteString                      as BS
import           Data.Conduit                         (runConduit, (.|))
import qualified Data.Conduit.List                    as CL
import           Data.Conduit.Network                 (ServerSettings,
                                                       appSink,
                                                       appSource,
                                                       runTCPServer,
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

-- | Default ABCI app network settings.
defaultSettings :: ServerSettings
defaultSettings = serverSettings 26658 $ fromString "127.0.0.1"

type AppRunner m = BS.ByteString -> m BS.ByteString

-- | Serve an ABCI application with custom 'ServerSettings'
serveAppWith :: ServerSettings -> AppRunner IO -> IO ()
serveAppWith cfg runner = runTCPServer cfg $ \appData -> do
  runConduit $
      appSource appData
    .| CL.mapM runner
    .| appSink appData

-- | Serve an ABCI application with default 'ServerSettings'
serveApp :: AppRunner IO -> IO ()
serveApp = serveAppWith defaultSettings


runApp :: Monad m => App m -> AppRunner m
runApp (App app) bsInput = do
  responcesE <- runExceptT $ do
    packets <- except $ Wire.decodeLengthPrefix bsInput
    requests <- except $ traverse decodeRequest packets
    lift $ forM requests $ Request.withProto $ fmap Response.toProto . app
  pure $ Wire.encodeLengthPrefix $ map PL.encodeMessage $ fromEither errToResponses responcesE
  where
    errToResponses err =
      [Response.toProto $ Response.ResponseException $ Response.Exception $ cs $ Error.print err]

    decodeRequest packet = case PL.decodeMessage packet of
      Left parseError -> Left $ Error.CanNotDecodeRequest packet parseError
      Right (request :: PT.Request) -> case request ^. PT.maybe'value of
        Nothing -> Left $ Error.NoValueInRequest packet (request ^. PL.unknownFields)
        Just value -> Right $ value
