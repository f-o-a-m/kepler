module Tendermint.SDK.Module where

import Data.Functor (($>))
import Data.Foldable (traverse_)
import Control.Monad.Free (Free, foldFree, liftF)
import Data.Functor.Coyoneda (Coyoneda(..), liftCoyoneda)
import Unsafe.Coerce (unsafeCoerce)

--import Tendermint.SDK.Store

data TendermintF state action m a =
    State (state -> m a)
  | Lift (m a)

instance Functor m => Functor (TendermintF state action m) where
    fmap f a = case a of
      State g -> State (fmap f . g)
      Lift b -> Lift (f <$> b)

newtype TendermintM state action m a = TendermintM (Free (TendermintF state action m) a)
  deriving (Functor, Applicative, Monad)

tState :: Functor m => (state -> m a) -> TendermintM state action m a
tState = TendermintM . liftF . State

data TendermintQ query action input a
  = Initialize a
  | Action action a
  | Receive input a
  | Query (Coyoneda query a) (() -> a)
  deriving (Functor)
      

data EvalSpec state query action input m = EvalSpec
    { handleAction :: action -> TendermintM state action m ()
    , handleQuery :: forall a. query a -> TendermintM state action m (Maybe a)
    , receive :: input -> Maybe action
    , initialize :: Maybe action
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
  Query (Coyoneda g a) f ->  maybe (f ()) g <$> handleQuery a

data ComponentSpec state query action input m = ComponentSpec 
  { initialState :: input -> m state
  , eval :: forall a. TendermintQ query action input a -> TendermintM state action m a
  }

data Component query input m


-- TODO: Use GADTs
mkComponent :: ComponentSpec state query action input m -> Component query input m
mkComponent = unsafeCoerce

unComponent 
  :: forall query input m a. 
     (forall state action. ComponentSpec state query action input m -> a)
  -> Component query input m -> a
unComponent = unsafeCoerce

data DriverState state query action input m = DriverState
  { component :: ComponentSpec state query action input m
  , state :: state
  }

evalM
  :: Monad m
  => DriverState state query action input m
  -> (forall a. TendermintM state action m a -> m a)
evalM ds (TendermintM tm) = foldFree (go ds) tm
  where
  go
    :: DriverState state query action input m
    -> (forall a. TendermintF state action m a -> m a)
  go ds' = \case
    State f -> do
      let DriverState {state} = ds'
      f state
    Lift m -> m

evalQ
  :: Monad m
  => DriverState state query action input m
  -> query a
  -> m (Maybe a)
evalQ ds@DriverState{component} q = do
  let ComponentSpec{eval} = component  
  evalM ds (eval (Query (Just <$> liftCoyoneda q) (const Nothing)))

-- TODO: Use GADTs
data DriverStateX query m

mkDriverStateX :: DriverState state query action input m -> DriverStateX query m
mkDriverStateX = unsafeCoerce

unDriverStateX
  :: forall x query m.
     (forall state action input. DriverState state query action input m -> x)
  -> DriverStateX query m
  -> x
unDriverStateX = unsafeCoerce

initDriverState
  :: Monad m
  => ComponentSpec state query action input m
  -> input
  -> m (DriverStateX query m)
initDriverState c@ComponentSpec{initialState} i = do
  s <- initialState i
  return . mkDriverStateX $ DriverState
    { component = c
    , state = s
    }

-- NOTE: this is dumb, it's just a renaming at this point
evalDriver
  :: Monad m
  => DriverState state query action input m
  -> forall a. (query a -> m (Maybe a))
evalDriver ds q = evalQ ds q

type Request f a = (a -> a) -> f a
request :: forall f a. Request f a -> f a
request req = req id


type Tell f = () -> f ()
tell :: forall f. Tell f -> f ()
tell act = act ()

data TendermintIO query m = TendermintIO
  { query :: forall a. query a -> m (Maybe a)
  }

runTendermint
  :: Monad m
  => Component query input m
  -> input
  -> m (TendermintIO query m)
runTendermint component i = 
  unComponent (\componentSpec -> do
    ds <- initDriverState componentSpec i
    unDriverStateX (\st ->
      return $ TendermintIO
        { query =  evalDriver st
        }
      ) ds
    ) component

