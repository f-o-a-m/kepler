module Network.ABCI where

import           Data.Conduit           (runConduit, (.|))
import qualified Data.Conduit.List      as CL
import           Data.Conduit.Network   (AppData, ServerSettings, appSink,
                                         appSource, runTCPServer,
                                         serverSettings)
import           Data.Monoid            (mempty)
import           Data.String            (fromString)
import           Network.ABCI.Types.App (App (..))
import qualified Network.ABCI.Types.App as App


-- | Default ABCI app network settings for serving on localhost at the
-- standard port.
defaultLocalSettings :: ServerSettings
defaultLocalSettings = serverSettings 26658 $ fromString "127.0.0.1"

-- | Serve an ABCI application with custom 'ServerSettings' and a custom
-- action to perform on acquiring the socket resource.
serveAppWith
  :: ServerSettings
  -> (AppData -> IO ())
  -> App IO
  -> IO ()
serveAppWith cfg onAquire app = runTCPServer cfg $ \appData -> do
  onAquire appData
  runConduit $ appSource appData
    .| CL.mapM (fmap App.unLPByteStrings . App.runApp app . App.LPByteStrings)
    .| appSink appData


-- | Serve an ABCI application with default local 'ServerSettings'
-- and a no-op on acquiring the socket resource.
serveApp :: App IO -> IO ()
serveApp = serveAppWith defaultLocalSettings mempty
