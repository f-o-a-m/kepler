module Tendermint.SDK.Module where

import           Control.Monad.IO.Class (MonadIO (..))
import           Polysemy
import           Tendermint.SDK.Logger
import           Tendermint.SDK.Store

type BaseApp r =
  ( Member Logger r
  , Member RawStore r
  )

type BaseAppR = [Logger, RawStore]

interpretBaseAppStandard
  :: MonadIO (Sem r)
  => Sem (Logger ': RawStore ': r) a
  -> Sem r a
interpretBaseAppStandard = undefined


--data TendermintIO query output api m = TendermintIO
--  { ioQuery     :: forall a. query a -> m a
--  , ioServer    :: RouteT api m
--  , ioSubscribe :: ConduitT output Void IO () -> IO (Async.Async ())
--  }
--
--runComponent
--  :: MonadIO m
--  => Component query input output api m
--  -> input
--  -> (output -> m ())
--  -> m (DriverStateX query output m, RouteT api m)
--runComponent component i handler =
--  withComponent (\componentSpec -> do
--    initDriverState componentSpec i handler
--    ) component
--
--runApp
--  :: MonadIO m
--  => Component query input output api m
--  -> input
--  -> m (TendermintIO query output api m)
--runApp c i = do
--  fresh <- liftIO $ IORef.newIORef 0
--  listeners <- liftIO $ IORef.newIORef M.empty
--  (ds, server) <- runComponent c i (liftIO . rootHandler listeners)
--  withDriverStateX (\st -> do
--    evalM st (eval (component st) $ Initialize ())
--    return $ TendermintIO
--      { ioQuery =  evalDriver st
--      , ioServer = server
--      , ioSubscribe = subscribe fresh listeners
--      }
--    ) ds
--
