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
  Initialize a ->
    traverse_ handleAction initialize $> a
  Receive i a ->
    traverse_ handleAction (receive i) $> a
  Action action a ->
    handleAction action $> a
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
