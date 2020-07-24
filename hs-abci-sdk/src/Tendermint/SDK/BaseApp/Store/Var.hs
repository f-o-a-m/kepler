module Tendermint.SDK.BaseApp.Store.Var
  ( Var
  , makeVar
  , makeFullStoreKey
  , takeVar
  , unsafeTakeVar
  , putVar
  , deleteVar
  ) where

import           Control.Lens                          ((^.))
import           Polysemy                              (Member, Members, Sem)
import           Polysemy.Error                        (Error)
import           Tendermint.SDK.BaseApp.Errors         (AppError,
                                                        SDKError (StoreError),
                                                        throwSDKError)
import qualified Tendermint.SDK.BaseApp.Store.RawStore as S
import           Tendermint.SDK.Codec                  (HasCodec (..))
import Data.Kind (Type)

data Var (a :: Type) = Var
  { varStore :: S.Store (Var a) }

instance S.IsKey () (Var a) where
    type Value () (Var a) = a

makeVar
  :: S.IsKey k ns
  => S.Value k ns ~ Var a
  => k
  -> S.Store ns
  -> S.Value k ns
makeVar key store =
  Var $ S.nestStore store $
    S.makeStore . S.KeyRoot $ key ^. S.rawKey

makeFullStoreKey
  :: Var a
  -> S.StoreKey
makeFullStoreKey Var{..} =
  S.makeStoreKey varStore ()

takeVar
  :: Members [S.ReadStore, Error AppError] r
  => HasCodec a
  => Var a
  -> Sem r (Maybe a)
takeVar Var{..} = S.get varStore ()

unsafeTakeVar
  :: Members [S.ReadStore, Error AppError] r
  => HasCodec a
  => Var a
  -> Sem r a
unsafeTakeVar Var{..} = do
  mRes <- S.get varStore ()
  case mRes of
    Just a  -> pure a
    Nothing -> throwSDKError $ StoreError "Var key not found."

putVar
  :: Member S.WriteStore r
  => HasCodec a
  => a
  -> Var a
  -> Sem r ()
putVar a Var{..} = S.put varStore () a

deleteVar
  :: Member S.WriteStore r
  => Var a
  -> Sem r ()
deleteVar Var{..} = S.delete varStore ()
