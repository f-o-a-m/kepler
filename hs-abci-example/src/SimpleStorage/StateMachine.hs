module SimpleStorage.StateMachine
  ( initStateMachine
  , updateCount
  , readCount
  ) where

import           Crypto.Hash         (SHA256 (..), hashWith)
import           Data.ByteArray      (convert)
import           Data.ByteString     (ByteString)
import           Data.Int            (Int32)
import           Data.Maybe          (fromJust)
import           Data.Proxy
import qualified Data.Text.Encoding  as T
import qualified SimpleStorage.DB    as DB
import           SimpleStorage.Types

countKey :: ByteString
countKey = convert . hashWith SHA256 . T.encodeUtf8 $ "count"

initStateMachine :: IO (DB.Connection "count")
initStateMachine = do
  conn <- DB.makeConnection (Proxy @"count")
  DB.put conn countKey (0 :: Int32)
  pure conn

updateCount
  :: UpdateCountTx
  -> DB.Transaction "count" ()
updateCount UpdateCountTx{updateCountTxCount} =
  DB.withConnection $ \conn ->
    DB.put conn countKey updateCountTxCount

-- NOTE: fromJust is actually safe because there's no way
-- to create a count DB without initializing the value to 0
readCount
  :: DB.Connection "count"
  -> IO Int32
readCount conn = fromJust <$> DB.get conn countKey
