module SimpleStorage.DB
  ( Connection
  , makeConnection
  , put
  , get
  , withHashTree
  ) where

import           Control.Concurrent.MVar (MVar, modifyMVar_, newMVar, readMVar)
import qualified Crypto.Data.Auth.Tree   as AT
import           Data.Binary             (Binary, decode, encode)
import           Data.ByteString         (ByteString)
import           Data.ByteString.Lazy    (fromStrict, toStrict)
import           Data.Proxy              (Proxy)
import           GHC.TypeLits            (Symbol)

data DB (name :: Symbol) a = DB
  { dbTree :: AT.Tree ByteString ByteString
  }

newtype Connection name a = Connection (MVar (DB name a))

-- | Create a handle to a new DB.
makeConnection
  :: Proxy name
  -> Proxy a
  -> IO (Connection name a)
makeConnection _ _ = fmap Connection . newMVar $
  DB { dbTree = AT.empty
     }

-- | Put an item in the DB.
put
  :: Binary a
  => Connection name a
  -> ByteString
  -> a
  -> IO ()
put (Connection c) k a = do
  let v = toStrict . encode $ a
  modifyMVar_ c $ \DB{dbTree} ->
    pure $ DB { dbTree = AT.insert k v dbTree
              }

-- | Get an item from the DB using its hash as a key.
get
  :: Binary a
  => Connection name a
  -> ByteString
  -> IO (Maybe a)
get (Connection c) k = do
  DB{dbTree} <- readMVar c
  pure $ decode . fromStrict <$> AT.lookup k dbTree

-- | Query the HashTree in a read-only fashion.
withHashTree
  :: Connection name a
  -> (AT.Tree ByteString ByteString -> a)
  -> IO a
withHashTree (Connection c) f = do
  DB{dbTree} <- readMVar c
  pure $ f dbTree
