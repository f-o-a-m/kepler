module SimpleStorage.StateMachine
  ( initStateMachine
  , updateCount
  , readCount
  ) where

import Data.Int (Int32)
import qualified SimpleStorage.DB    as DB
import           SimpleStorage.Types
import Crypto.Hash (hashWith, SHA256(..))
import Data.ByteArray (convert)
import qualified Data.Text.Encoding as T
import Data.ByteString (ByteString)
import Data.Maybe (fromJust)
import Data.Proxy

initStateMachine :: IO (DB.Connection "count" Int32)
initStateMachine = do
  conn <- DB.makeConnection (Proxy @"count") (Proxy @Int32)
  DB.put conn countKey 0
  pure conn

countKey :: ByteString
countKey = convert . hashWith SHA256 . T.encodeUtf8 $ "count"

updateCount
  :: DB.Connection "count" Int32
  -> UpdateCountTx
  -> IO ()
updateCount conn UpdateCountTx{updateCountTxCount} =
  DB.put conn countKey updateCountTxCount

-- NOTE: fromJust is actually safe because there's no way
-- to create a count DB without initializing the value to 0
readCount
  :: DB.Connection "count" Int32
  -> IO Int32
readCount conn = fromJust <$> DB.get conn countKey
