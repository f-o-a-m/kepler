{-# LANGUAGE TemplateHaskell #-}

module Tendermint.SDK.BaseApp.Store.RawStore
  (
  -- * Effects
    StoreEffs
  , ReadStore(..)
  , storeGet
  , get
  , prove
  , WriteStore(..)
  , put
  , storePut
  , delete
  , storeDelete
  , CommitBlock(..)
  , commitBlock
  , Transaction(..)
  , beginTransaction
  , withSandbox
  , withTransaction
  , commit

  -- * Types
  , Scope(..)
  , Version(..)
  , RawKey(..)
  , IsKey(..)
  , RawStoreKey(..)
  , KeyRoot(..)
  , makeRawKey
  , Store
  , nestStore
  , makeRawStoreKey
  , makeStore
  , CommitResponse(..)
  ) where

import           Control.Lens                  (Iso', iso, (^.))
import           Data.ByteArray.Base64String   (Base64String)
import qualified Data.ByteString               as BS
import           Data.Proxy
import           Data.String.Conversions       (cs)
import           Numeric.Natural               (Natural)
import           Polysemy                      (Member, Members, Sem, makeSem)
import           Polysemy.Error                (Error, catch, throw)
import           Polysemy.Resource             (Resource, finally, onException)
import           Polysemy.Tagged               (Tagged)
import           Tendermint.SDK.BaseApp.Errors (AppError, SDKError (ParseError),
                                                throwSDKError)
import           Tendermint.SDK.Codec          (HasCodec (..))
import           Tendermint.SDK.Types.Address  (Address, addressFromBytes,
                                                addressToBytes)

data RawStoreKey = RawStoreKey
  { rsPathFromRoot :: [BS.ByteString]
  , rsKey          :: BS.ByteString
  } deriving (Eq, Show, Ord)

makeRawKey :: RawStoreKey -> BS.ByteString
makeRawKey RawStoreKey{..} =  mconcat rsPathFromRoot <> rsKey

data ReadStore m a where
  StoreGet   :: RawStoreKey -> ReadStore m (Maybe BS.ByteString)
  StoreProve :: RawStoreKey -> ReadStore m (Maybe BS.ByteString)

makeSem ''ReadStore

data WriteStore m a where
  StorePut   :: RawStoreKey -> BS.ByteString -> WriteStore m ()
  StoreDelete :: RawStoreKey -> WriteStore m ()

makeSem ''WriteStore

class RawKey k where
  rawKey :: Iso' k BS.ByteString

instance RawKey Address where
    rawKey = iso addressToBytes addressFromBytes

class RawKey k => IsKey k ns where
  type Value k ns :: *
  prefix :: Proxy k -> Proxy ns -> BS.ByteString

  default prefix :: Proxy k -> Proxy ns -> BS.ByteString
  prefix _ _ = ""

newtype KeyRoot ns =
  KeyRoot BS.ByteString deriving (Eq, Show)

data Store ns = Store
  { storePathFromRoot :: [BS.ByteString]
  }

makeStore :: KeyRoot ns  -> Store ns
makeStore (KeyRoot ns) = Store
  { storePathFromRoot = [ns]
  }

nestStore :: Store parentns -> Store childns -> Store childns
nestStore (Store parentPath) (Store childPath) =
  Store
    { storePathFromRoot =  parentPath ++ childPath
    }

makeRawStoreKey
  :: forall k ns.
     IsKey k ns
  => Store ns
  -> k
  -> RawStoreKey
makeRawStoreKey (Store path) k =
  RawStoreKey
    { rsKey = prefix (Proxy @k) (Proxy @ns) <> k ^. rawKey
    , rsPathFromRoot = path
    }

data CommitBlock m a where
  CommitBlock :: CommitBlock m Base64String

makeSem ''CommitBlock

put
  :: forall k r ns.
     IsKey k ns
  => HasCodec (Value k ns)
  => Member WriteStore r
  => Store ns
  -> k
  -> Value k ns
  -> Sem r ()
put store k a =
  let key = makeRawStoreKey store k
      val = encode a
  in storePut key val

get
  :: forall k r ns.
     IsKey k ns
  => HasCodec (Value k ns)
  => Members [ReadStore, Error AppError] r
  => Store ns
  -> k
  -> Sem r (Maybe (Value k ns))
get store k = do
  let key = makeRawStoreKey store k
  mRes <- storeGet key
  case mRes of
    Nothing -> pure Nothing
    Just raw -> case decode raw of
      Left e  -> throwSDKError (ParseError $ "Impossible codec error: "  <> cs e)
      Right a -> pure $ Just a

delete
  :: forall k ns r.
     IsKey k ns
  => Member WriteStore r
  => Store ns
  -> k
  -> Sem r ()
delete store k =
  let key = makeRawStoreKey store k
  in storeDelete key

prove
  :: forall k ns r.
     IsKey k ns
  => Member ReadStore r
  => Store ns
  -> k
  -> Sem r (Maybe BS.ByteString)
prove store k =
  let key = makeRawStoreKey store k
  in storeProve key


data CommitResponse = CommitResponse
  { rootHash   :: Base64String
  , newVersion :: Natural
  } deriving (Eq, Show)

data Transaction m a where
  -- transact
  BeginTransaction :: Transaction m ()
  Rollback :: Transaction m ()
  Commit :: Transaction m CommitResponse

makeSem ''Transaction

withTransaction
  :: forall r a.
     Members [Transaction, Resource, Error AppError] r
  => Sem r a
  -> Sem r (a, CommitResponse)
withTransaction m =
   let tryTx = m `catch` (\e -> rollback *> throw e)
       actionWithCommit = do
         res <- tryTx
         c <- commit
         pure (res, c)
   in do
      onException actionWithCommit rollback

withSandbox
  :: forall r a.
     Members [Transaction, Resource, Error AppError] r
  => Sem r a
  -> Sem r a
withSandbox m =
   let tryTx = m `catch` (\e -> rollback *> throw e)
   in finally (tryTx <* rollback) rollback

data Scope = Consensus | QueryAndMempool

data Version =
    Genesis
  | Version Natural
  | Latest
  deriving (Eq, Show)

type StoreEffs =
  [ Tagged 'Consensus ReadStore
  , Tagged 'QueryAndMempool ReadStore
  , Tagged 'Consensus WriteStore
  , Transaction
  , CommitBlock
  ]
