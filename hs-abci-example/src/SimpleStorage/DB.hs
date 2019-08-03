module SimpleStorage.DB
  ( Connection(..)
  , DB
  , makeConnection
  , put
  , get
  , withHashTree
  ) where

import           Control.Concurrent.STM.TVar (TVar, newTVarIO)
import qualified Crypto.Data.Auth.Tree       as AT
import           Data.Binary                 (Binary, decode, encode)
import           Data.ByteString             (ByteString)
import           Data.ByteString.Lazy        (fromStrict, toStrict)
import           Data.Proxy                  (Proxy)
import           GHC.TypeLits                (Symbol)

data DB (name :: Symbol) = DB
  { dbTree :: AT.Tree ByteString ByteString
  }

newtype Connection name = Connection (TVar (DB name))

-- | Create a handle to a new DB.
makeConnection
  :: Proxy name
  -> IO (Connection name)
makeConnection _ = fmap Connection . newTVarIO $
  DB { dbTree = AT.empty
     }

-- | Put an item in the DB.
put
  :: Binary a
  => DB name
  -> ByteString
  -> a
  -> DB name
put DB{dbTree} k a =
  DB { dbTree = AT.insert k (toStrict . encode $ a) dbTree
     }

-- | Get an item from the DB from the key.
get
  :: forall a name.
     Binary a
  => DB name
  -> ByteString
  -> Maybe a
get db k = withHashTree db $
  fmap (decode . fromStrict) . AT.lookup k

-- | Query the HashTree in a read-only fashion.
withHashTree
  :: DB name
  -> (AT.Tree ByteString ByteString -> a)
  -> a
withHashTree DB{dbTree} f = f dbTree
