module Tendermint.SDK.Module where

import Data.Functor (($>))
import Data.Foldable (traverse_)
import Control.Monad.Free (Free)
import Data.Functor.Coyoneda (Coyoneda(..))
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
  { initialState :: input -> state
  , eval :: forall a. TendermintQ query action input a -> TendermintM state action m a
  }

data Component query input m

mkComponent :: ComponentSpec state query action input m -> Component query input m
mkComponent = unsafeCoerce

unComponent 
  :: forall query input m a. 
     (forall state action. ComponentSpec state query action input m -> a)
  -> Component query input m -> a
unComponent = unsafeCoerce
