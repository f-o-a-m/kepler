module SimpleStorage.Server (makeAndServeApplication) where

import           Data.Foldable                         (fold)
import           Data.Monoid                           (Endo (..))
import           Network.ABCI.Server                   (serveApp)
import           Network.ABCI.Server.App               (Middleware)
import qualified Network.ABCI.Server.Middleware.Logger as Logger
import           Polysemy                              (Sem)
import           SimpleStorage.Application             (handlersContext)
import           SimpleStorage.Config                  (AppConfig (..))
import           Tendermint.SDK.Application            (createIOApp, makeApp)
import           Tendermint.SDK.BaseApp                (CoreEffs, runCoreEffs)

makeAndServeApplication :: AppConfig -> IO ()
makeAndServeApplication AppConfig{..} = do
  putStrLn "Starting ABCI application..."
  let nat :: forall a. Sem CoreEffs a -> IO a
      nat = catchError . runCoreEffs _baseAppContext
      application = makeApp handlersContext
      middleware :: Middleware (Sem CoreEffs)
      middleware = appEndo . fold $
          [ Endo Logger.mkLoggerM
          ]
  serveApp $ createIOApp nat (middleware application)
 where
  catchError f = f >>= \case
    Left err -> error (show err)
    Right a -> pure a
