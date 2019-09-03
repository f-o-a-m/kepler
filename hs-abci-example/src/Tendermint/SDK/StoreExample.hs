module Tendermint.SDK.StoreExample where

import           Control.Concurrent.STM           (atomically)
import           Control.Concurrent.STM.TVar
import           Control.Lens                     (iso, (^.), from)
import qualified Crypto.Data.Auth.Tree            as AT
import qualified Crypto.Data.Auth.Tree.Class      as AT
import qualified Crypto.Data.Auth.Tree.Cryptonite as Cryptonite
import qualified Crypto.Hash                      as Cryptonite
import           Data.Binary                      (Binary)
import qualified Data.Binary                     as Binary
import           Data.ByteArray                   (convert)
import           Data.ByteArray.HexString
import           Data.String.Conversions          (cs)
import           GHC.Generics                     (Generic)
import           Tendermint.SDK.Codec
import           Tendermint.SDK.Store

import           Tendermint.SDK.Router.Types

--------------------------------------------------------------------------------
-- Example Store
--------------------------------------------------------------------------------

newtype AuthTreeHash =  AuthTreeHash (Cryptonite.Digest Cryptonite.SHA256)

instance AT.MerkleHash AuthTreeHash where
    emptyHash = AuthTreeHash Cryptonite.emptyHash
    hashLeaf k v = AuthTreeHash $ Cryptonite.hashLeaf k v
    concatHashes (AuthTreeHash a) (AuthTreeHash b) = AuthTreeHash $ Cryptonite.concatHashes a b

type AuthTreeRawStore = RawStore IO

mkAuthTreeStore :: IO AuthTreeRawStore
mkAuthTreeStore = do
  treeV <- newTVarIO AT.empty
  pure $ RawStore
    { rawStorePut = \k v -> atomically $ do
        tree <- readTVar treeV
        writeTVar treeV $ AT.insert k v tree
    , rawStoreGet = \_ k -> atomically $ do
        tree <- readTVar treeV
        pure $ AT.lookup k tree
    , rawStoreProve = \_ _ -> pure Nothing
    , rawStoreRoot = atomically $ do
        tree <- readTVar treeV
        let AuthTreeHash r = AT.merkleHash tree :: AuthTreeHash
        pure $ Root $ convert r
    }

data User = User
  { userAddress :: String
  , userName    :: String
  } deriving (Eq, Show, Generic)

newtype UserKey = UserKey String deriving Show

instance Binary User

instance HasCodec User where
    encode = cs . Binary.encode
    decode = Right . Binary.decode . cs

instance HasKey User where
    type Key User = UserKey
    rawKey = iso (\(UserKey k) -> cs k) (UserKey . cs)

instance FromQueryData UserKey where
  fromQueryData hx = Right (toBytes hx ^. from rawKey)

instance EncodeQueryResult User where
  encodeQueryResult = fromBytes . encode

instance Queryable User where
  type Name User = "user"

--------------------------------------------------------------------------------

data Dog = Dog
  { dogId :: String
  , dogName  :: String
  } deriving (Eq, Show, Generic)

instance Binary Dog

newtype DogKey = DogKey String deriving Show

instance HasKey Dog where
  type Key Dog = DogKey
  rawKey = iso (\(DogKey k) -> cs k) (DogKey . cs)

instance HasCodec Dog where
  encode = cs . Binary.encode 
  decode = Right . Binary.decode . cs

instance EncodeQueryResult Dog where
  encodeQueryResult  = fromBytes . encode

instance FromQueryData DogKey where
  fromQueryData hx = Right (toBytes hx ^. from rawKey)

instance Queryable Dog where
  type Name Dog = "dog"

--------------------------------------------------------------------------------

type UserStoreContents = '[Dog, User]

type UserStore = Store UserStoreContents IO

putDog
  :: DogKey
  -> Dog
  -> UserStore
  -> IO ()
putDog k dog store = put k dog store

putUser
  :: UserKey
  -> User
  -> UserStore
  -> IO ()
putUser k user store = put k user store

{-

userStore :: UserStore
userStore = unsafePerformIO $ do
  rawStore <- mkAuthTreeStore
  pure $ Store
    { storeRawStore = rawStore
    }
{-# NOINLINE userStore #-}

userApi :: Proxy (QueryApi UserStoreContents)
userApi = Proxy

userServer :: RouteT (QueryApi UserStoreContents) IO 
userServer = storeQueryHandlers (Proxy :: Proxy UserStoreContents) userStore

serveRoutes :: Application IO
serveRoutes = serve userApi (Proxy :: Proxy IO) userServer

-}