module Tendermint.SDK.Module where

import           Control.Monad.Free     (Free, foldFree, liftF)
import           Control.Monad.IO.Class (MonadIO)
import           Data.Foldable          (traverse_)
import           Data.Functor           (($>))
import           Data.Functor.Coyoneda  (Coyoneda (..), liftCoyoneda)
import           Data.Proxy
import           Tendermint.SDK.Router
--import Tendermint.SDK.Store

data TendermintF state action m a =
    State (state -> m a)
  | Lift (m a)

instance Functor m => Functor (TendermintF state action m) where
    fmap f a = case a of
      State g -> State (fmap f . g)
      Lift b  -> Lift (f <$> b)

newtype TendermintM state action m a = TendermintM (Free (TendermintF state action m) a)
  deriving (Functor, Applicative, Monad)

withState :: Functor m => (state -> m a) -> TendermintM state action m a
withState = TendermintM . liftF . State

data TendermintQ query action input a
  = Initialize a
  | Action action a
  | Receive input a
  | Query (Coyoneda query a)
  deriving (Functor)


data EvalSpec state query action input m = EvalSpec
    { handleAction :: action -> TendermintM state action m ()
    , handleQuery  :: forall a. query a -> TendermintM state action m a
    , receive      :: input -> Maybe action
    , initialize   :: Maybe action
    }

mkEval
  :: forall state query action input m.
     Functor m
  => EvalSpec state query action input m
  -> (forall a. TendermintQ query action input a -> TendermintM state action m a)
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

data ComponentSpec state query action input (api :: *) m = ComponentSpec
  { initialState :: input -> m state
  , eval :: forall a. TendermintQ query action input a -> TendermintM state action m a
  , mkServer :: state -> RouteT api m
  }

data Component query input api m where
  Component :: ComponentSpec state query action input api m -> Component query input api m

withComponent
  :: forall query input api m a.
     (forall state action. ComponentSpec state query action input api m -> a)
  -> Component query input api m -> a
withComponent f (Component c) = f c

data DriverState state query action input api m = DriverState
  { component :: ComponentSpec state query action input api m
  , state     :: state
  }

evalM
  :: Monad m
  => DriverState state query action input api m
  -> (forall a. TendermintM state action m a -> m a)
evalM ds (TendermintM tm) = foldFree (go ds) tm
  where
  go
    :: DriverState state query action input api m
    -> (forall a. TendermintF state action m a -> m a)
  go ds' = \case
    State f -> do
      let DriverState {state} = ds'
      f state
    Lift m -> m

evalQ
  :: Monad m
  => DriverState state query action input api m
  -> query a
  -> m a
evalQ ds@DriverState{component} q = do
  let ComponentSpec{eval} = component
  evalM ds . eval $ Query (liftCoyoneda q)

-- TODO: Use GADTs
data DriverStateX query m where
  DriverStateX ::  DriverState state query action input api m -> DriverStateX query m

withDriverStateX
  :: forall x query m.
     (forall state action input api. DriverState state query action input api m -> x)
  -> DriverStateX query m
  -> x
withDriverStateX f (DriverStateX ds) = f ds

initDriverState
  :: forall state query action input api m server.
     HasRouter api
  => RouteT api m ~ server
  => MonadIO m
  => ComponentSpec state query action input api m
  -> input
  -> m (DriverStateX query m, Router () m)
initDriverState c@ComponentSpec{initialState, mkServer} i = do
  s <- initialState i
  let dsx =  DriverStateX $ DriverState
               { component = c
               , state = s
               }
      server = mkServer s
      router = route (Proxy :: Proxy api) (Proxy :: Proxy m)
                 (emptyDelayed (Route server) :: Delayed () server)
  return (dsx, router)

-- NOTE: this is dumb, it's just a renaming at this point
evalDriver
  :: Monad m
  => DriverState state query action input api m
  -> forall a. (query a -> m a)
evalDriver ds q = evalQ ds q

type Request f a = (a -> a) -> f a
request :: forall f a. Request f a -> f a
request req = req id


type Tell f = () -> f ()
tell :: forall f. Tell f -> f ()
tell act = act ()

data TendermintIO query m = TendermintIO
  { ioQuery  :: forall a. query a -> m a
  , ioRouter :: Router () m
  }

runTendermint
  :: MonadIO m
  => HasRouter api
  => Component query input api m
  -> input
  -> m (TendermintIO query m)
runTendermint component i =
  withComponent (\componentSpec -> do
    (ds, router) <- initDriverState componentSpec i
    withDriverStateX (\st ->
      return $ TendermintIO
        { ioQuery =  evalDriver st
        , ioRouter = router
        }
      ) ds
    ) component

