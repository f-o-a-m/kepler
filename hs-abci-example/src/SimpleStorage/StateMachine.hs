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
import           SimpleStorage.Transaction (Transaction, stateChange)
import           SimpleStorage.Types
import Control.Concurrent.STM.TVar (readTVarIO, modifyTVar)
import Control.Concurrent.STM (atomically)
import Data.Monoid (Endo(..))

countKey :: ByteString
countKey = convert . hashWith SHA256 . T.encodeUtf8 $ "count"

initStateMachine :: IO (DB.Connection "count")
initStateMachine = do
  conn@(DB.Connection c) <- DB.makeConnection (Proxy @"count")
  atomically $ modifyTVar c $ \db ->
    DB.put db countKey (0 :: Int32)
  pure conn

updateCount
  :: UpdateCountTx
  -> Transaction "count" ()
updateCount UpdateCountTx{updateCountTxCount} =
  stateChange $ Endo $ \db ->
    DB.put db countKey updateCountTxCount

-- NOTE: fromJust is actually safe because there's no way
-- to create a count DB without initializing the value to 0
readCount
  :: DB.Connection "count"
  -> IO Int32
readCount (DB.Connection c) = do
  db <- readTVarIO c
  pure $ fromJust $ DB.get db countKey
