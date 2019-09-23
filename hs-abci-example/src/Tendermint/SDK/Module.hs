{-# LANGUAGE StandaloneDeriving #-}

module Tendermint.SDK.Module where

import qualified Control.Concurrent.Async as Async
import qualified Control.Concurrent.MVar  as MVar
import           Control.Monad            (forM_, forever)
import           Control.Monad.Free       (Free, foldFree, liftF)
import           Control.Monad.IO.Class   (MonadIO, liftIO)
import           Data.Conduit
import           Data.Foldable            (traverse_)
import           Data.Functor             (($>))
import           Data.Functor.Coyoneda    (Coyoneda (..), liftCoyoneda)
import qualified Data.IORef               as IORef
import qualified Data.Map                 as M
import           Tendermint.SDK.Router
-- import qualified Debug.Trace as Trace

--import Tendermint.SDK.Store

data TendermintF state action output m a =
    State (state -> m a)
  | Lift (m a)
  | Raise output a

deriving instance Functor m => Functor (TendermintF state action output m)

newtype TendermintM state action output m a = TendermintM (Free (TendermintF state action output m) a)
  deriving (Functor, Applicative, Monad)

withState :: Functor m => (state -> m a) -> TendermintM state action output m a
withState = TendermintM . liftF . State

raise :: Functor m => output -> TendermintM state action output m ()
raise o = TendermintM . liftF $ Raise o ()

data TendermintQ query action input a
  = Initialize a
  | Action action a
  | Receive input a
  | Query (Coyoneda query a)
  deriving (Functor)


data EvalSpec state query action input output m = EvalSpec
    { handleAction :: action -> TendermintM state action output m ()
    , handleQuery  :: forall a. query a -> TendermintM state action output m a
    , receive      :: input -> Maybe action
    , initialize   :: Maybe action
    }

defaultEvalSpec
  :: Applicative m
  => (forall a. query a -> TendermintM state action output m a)
  -> EvalSpec state query action input output m
defaultEvalSpec f = EvalSpec
  { handleAction = const $ pure ()
  , handleQuery = f
  , receive = const Nothing
  , initialize = Nothing
  }

mkEval
  :: forall state query action input output m.
     Functor m
  => EvalSpec state query action input output m
  -> (forall a. TendermintQ query action input a -> TendermintM state action output m a)
mkEval EvalSpec{..} q = case q of
  -- the module can use this to initialize itself, seems self evident
  Initialize a ->
    traverse_ handleAction initialize $> a
  -- as far as i can tell, this is only used by halogen in the case where a
  -- compenet is a child component and is being run. See
  -- https://github.com/slamdata/purescript-halogen/blob/78a47710678ac8b59142263149f7c532387b662d/src/Halogen/Component.purs#L229
  Receive i a ->
    traverse_ handleAction (receive i) $> a
  -- an action is an internal version of a query, so that a module can raise events that are
  -- not known to external modules
  Action action a ->
    handleAction action $> a
  -- the module exposes this algebra to be used by other modules as a command / msg passing
  -- algebra
  Query (Coyoneda g a) ->  g <$> handleQuery a

data ComponentSpec state query action input output (api :: *) m = ComponentSpec
  { initialState :: input -> m state
  , eval :: forall a. TendermintQ query action input a -> TendermintM state action output m a
  , mkServer :: state -> RouteT api m
  }

data Component query input output api m where
  Component :: ComponentSpec state query action input output api m -> Component query input output api m

withComponent
  :: forall query input output api m a.
     (forall state action. ComponentSpec state query action input output api m -> a)
  -> Component query input output api m -> a
withComponent f (Component c) = f c

data DriverState state query action input output api m = DriverState
  { component :: ComponentSpec state query action input output api m
  , state     :: state
  , handler   :: output -> m ()
  }

evalM
  :: MonadIO m
  => DriverState state query action input output api m
  -> (forall a. TendermintM state action output m a -> m a)
evalM ds (TendermintM tm) = foldFree (go ds) tm
  where
  go
    :: MonadIO m
    => DriverState state query action input output api m
    -> (forall a. TendermintF state action output m a -> m a)
  go ds' = \case
    State f -> do
      let DriverState {state} = ds'
      f state
    Lift m -> m
    Raise output a -> do
      let DriverState {handler} = ds'
      handler output
      pure a

evalQ
  :: MonadIO m
  => DriverState state query action input output api m
  -> query a
  -> m a
evalQ ds q = do
  let DriverState{component} = ds
      ComponentSpec{eval} = component
  evalM ds . eval $ Query (liftCoyoneda q)

-- TODO: Use GADTs
data DriverStateX query output m where
  DriverStateX ::  DriverState state query action input output api m -> DriverStateX query output m

withDriverStateX
  :: forall x query output m.
     (forall state action input api. DriverState state query action input output api m -> x)
  -> DriverStateX query output m
  -> x
withDriverStateX f (DriverStateX ds) = f ds

initDriverState
  :: forall state query action input output api m.
     MonadIO m
  => ComponentSpec state query action input output api m
  -> input
  -> (output -> m ())
  -> m (DriverStateX query output m,  RouteT api m)
initDriverState c@ComponentSpec{initialState, mkServer} i handler = do
  s <- initialState i
  let dsx =  DriverStateX $ DriverState
               { component = c
               , state = s
               , handler = handler
               }
      server = mkServer s
  return (dsx, server)

-- NOTE: this is dumb, it's just a renaming at this point
evalDriver
  :: MonadIO m
  => DriverState state query action input output api m
  -> forall a. (query a -> m a)
evalDriver ds q = evalQ ds q

type Request f a = (a -> a) -> f a
request :: forall f a. Request f a -> f a
request req = req id


type Tell f = () -> f ()
tell :: forall f. Tell f -> f ()
tell act = act ()

data TendermintIO query output api m = TendermintIO
  { ioQuery     :: forall a. query a -> m a
  , ioServer    :: RouteT api m
  , ioSubscribe :: ConduitT output Void IO () -> IO (Async.Async ())
  }

rootHandler
  :: IORef.IORef (M.Map Int (MVar.MVar output))
  -> output
  -> IO ()
rootHandler ref message = do
  listeners <- IORef.readIORef ref
  forM_ listeners $ \listener -> MVar.putMVar listener message

subscribe
  :: IORef.IORef Int
  -> IORef.IORef (M.Map Int (MVar.MVar output))
  -> ConduitT output Void IO ()
  -> IO (Async.Async ())
subscribe fresh ref consumer = do
  inputVar <- MVar.newEmptyMVar
  listenerId <- do
    listenerId <- IORef.readIORef fresh
    IORef.modifyIORef fresh (1 +)
    IORef.modifyIORef ref (M.insert listenerId inputVar)
    pure listenerId
  let producer = mkProducer inputVar
  Async.async $ do
    runConduit (producer .| consumer)
    IORef.modifyIORef ref (M.delete listenerId)
  where
    mkProducer :: MonadIO m => MVar.MVar output -> ConduitT () output m ()
    mkProducer var = forever $ do
      mInput <- liftIO $ MVar.tryTakeMVar var
      case mInput of
        Nothing -> pure ()
        Just a  -> yield a


runComponent
  :: MonadIO m
  => Component query input output api m
  -> input
  -> (output -> m ())
  -> m (DriverStateX query output m, RouteT api m)
runComponent component i handler =
  withComponent (\componentSpec -> do
    initDriverState componentSpec i handler
    ) component

runApp
  :: MonadIO m
  => Component query input output api m
  -> input
  -> m (TendermintIO query output api m)
runApp component i = do
  fresh <- liftIO $ IORef.newIORef 0
  listeners <- liftIO $ IORef.newIORef M.empty
  (ds, server) <- runComponent component i (liftIO . rootHandler listeners)
  withDriverStateX (\st -> do
    return $ TendermintIO
      { ioQuery =  evalDriver st
      , ioServer = server
      , ioSubscribe = subscribe fresh listeners
      }
    ) ds
