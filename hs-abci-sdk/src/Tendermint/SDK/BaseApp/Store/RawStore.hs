{-# LANGUAGE TemplateHaskell #-}

module Tendermint.SDK.BaseApp.Store.RawStore
  ( StoreEffs
  , Scope(..)
  , ApplyScope
  , applyScope
  , RawKey(..)
  , IsKey(..)
  , RawStoreKey(..)
  , makeRawKey
  , StoreKey(..)
  , ReadStore(..)
  , get
  , prove
  , WriteStore(..)
  , storePut
  , storeDelete
  , put
  , delete
  , CommitBlock(..)
  , commitBlock
  , Transaction(..)
  , CommitResponse(..)
  , beginTransaction
  , commit
  , withSandbox
  , withTransaction
  ) where

import           Control.Lens                  (Iso', iso, (^.))
import           Data.ByteArray.Base64String   (Base64String)
import qualified Data.ByteString               as BS
import           Data.Proxy
import           Data.String.Conversions       (cs)
import           Numeric.Natural               (Natural)
import           Polysemy                      (EffectRow, Member, Members, Sem,
                                                makeSem)
import           Polysemy.Error                (Error, catch, throw)
import           Polysemy.Resource             (Resource, finally, onException)
import           Polysemy.Tagged               (Tagged, tag)
import           Tendermint.SDK.BaseApp.Errors (AppError, SDKError (ParseError),
                                                throwSDKError)
import           Tendermint.SDK.Codec          (HasCodec (..))
import           Tendermint.SDK.Types.Address  (Address, addressFromBytes,
                                                addressToBytes)

data RawStoreKey = RawStoreKey
  { rsStoreKey :: BS.ByteString
  , rsKey      :: BS.ByteString
  } deriving (Eq, Ord)

makeRawKey :: RawStoreKey -> BS.ByteString
makeRawKey RawStoreKey{..} = rsStoreKey <> rsKey

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
  type Value k ns = a | a -> ns k
  prefix :: Proxy k -> Proxy ns -> BS.ByteString

  default prefix :: Proxy k -> Proxy ns -> BS.ByteString
  prefix _ _ = ""

newtype StoreKey ns = StoreKey BS.ByteString

data CommitBlock m a where
  CommitBlock :: CommitBlock m Base64String

makeSem ''CommitBlock

put
  :: forall k r ns.
     IsKey k ns
  => HasCodec (Value k ns)
  => Member WriteStore r
  => StoreKey ns
  -> k
  -> Value k ns
  -> Sem r ()
put (StoreKey sk) k a =
  let key = RawStoreKey
        { rsStoreKey = sk
        , rsKey = prefix (Proxy @k) (Proxy @ns) <> k ^. rawKey
        }
      val = encode a
  in storePut key val

get
  :: forall k r ns.
     IsKey k ns
  => HasCodec (Value k ns)
  => Members [ReadStore, Error AppError] r
  => StoreKey ns
  -> k
  -> Sem r (Maybe (Value k ns))
get (StoreKey sk) k = do
  let key = RawStoreKey
        { rsStoreKey = sk
        , rsKey = prefix (Proxy @k) (Proxy @ns) <> k ^. rawKey
        }
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
  => StoreKey ns
  -> k
  -> Sem r ()
delete (StoreKey sk) k =
  let key = RawStoreKey
        { rsStoreKey = sk
        , rsKey = prefix (Proxy @k) (Proxy @ns) <> k ^. rawKey
        }
  in storeDelete key

prove
  :: forall k ns r.
     IsKey k ns
  => Member ReadStore r
  => StoreKey ns
  -> k
  -> Sem r (Maybe BS.ByteString)
prove (StoreKey sk) k =
  let key = RawStoreKey
        { rsStoreKey = sk
        , rsKey = prefix (Proxy @k) (Proxy @ns) <> k ^. rawKey
        }
  in storeProve key


data CommitResponse = CommitResponse
  { rootHash   :: Base64String
  , newVersion :: Natural
  }

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

type family ApplyScope (s :: Scope) (es :: EffectRow) :: EffectRow where
  ApplyScope _ '[] = '[]
  ApplyScope s (ReadStore ': rest) = Tagged s ReadStore ': ApplyScope s rest
  ApplyScope s (e ': rest) = e ': ApplyScope s rest

applyScope
  :: forall (s :: Scope) r.
     Member (Tagged s ReadStore) r
  => forall a.
     Sem (ReadStore ': r) a
  -> Sem r a
applyScope = tag @s

type StoreEffs =
  [ Tagged 'Consensus ReadStore
  , Tagged 'QueryAndMempool ReadStore
  , Tagged 'Consensus WriteStore
  , Transaction
  , CommitBlock
  ]
