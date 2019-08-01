module SimpleStorage.DB
  ( Connection
  , makeConnection
  , put
  , get
  , withHashTree
  ) where

import           Codec.Serialise         (Serialise, deserialise, serialise)
import           Control.Concurrent.MVar (MVar, modifyMVar_, newMVar, readMVar)
import           Crypto.Hash             (Digest, hashWith)
import           Crypto.Hash.Algorithms  (SHA256 (..))
import           Data.ByteArray          (convert)
import           Data.ByteString         (ByteString)
import           Data.ByteString.Lazy    (fromStrict, toStrict)
import           Data.HashTree           (MerkleHashTrees, add, defaultSettings,
                                          empty)
import qualified Data.Map.Strict         as M

data DB = DB
  { dbTree :: MerkleHashTrees ByteString SHA256
  , dbMap  :: M.Map (Digest SHA256) ByteString
  }

newtype Connection = Connection (MVar DB)

-- | Create a handle to a new DB.
makeConnection :: IO Connection
makeConnection = fmap Connection . newMVar $
  DB { dbTree = empty defaultSettings
     , dbMap = mempty
     }

-- | Put an item in the DB.
put
  :: Serialise a
  => Connection
  -> a
  -> IO ()
put (Connection c) a = do
  let entry = serialise a
      entryHash = hashWith SHA256 . toStrict $ entry
  modifyMVar_ c $ \DB{dbTree, dbMap} ->
    pure $ DB { dbTree = add (convert entryHash) dbTree
              , dbMap = M.insert entryHash (toStrict $ serialise a) dbMap
              }

-- | Get an item from the DB using its hash as a key.
get
  :: forall a.
     Serialise a
  => Connection
  -> Digest SHA256
  -> IO (Maybe a)
get (Connection c) d = do
  DB{dbMap} <- readMVar c
  pure $ deserialise . fromStrict <$> M.lookup d dbMap

-- | Query the HashTree in a read-only fashion.
withHashTree
  :: Connection
  -> (MerkleHashTrees ByteString SHA256 -> a)
  -> IO a
withHashTree (Connection c) f = do
  DB{dbTree} <- readMVar c
  pure $ f dbTree
