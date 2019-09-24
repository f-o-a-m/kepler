module SimpleStorage.Types where

import           Control.Lens                        (iso, view, (&), (.~),
                                                      (^.), from)
import           Control.Lens.Wrapped                (Wrapped (..), _Unwrapped')
import           Data.Binary                         (Binary)
import           Data.ByteString                     (ByteString)
import           Data.Int                            (Int32)
import qualified Data.ProtoLens                      as PL
import           Data.ProtoLens.Message              (Message (..))
import           Data.Text                           (Text)
import           GHC.Generics                        (Generic)
import           Proto.SimpleStorage.Messages        as M
import           Proto.SimpleStorage.Messages_Fields as M

data AppTxMessage =
    ATMUpdateCount UpdateCountTx

decodeAppTxMessage
  :: ByteString
  -> Either String AppTxMessage
decodeAppTxMessage = fmap (ATMUpdateCount . view _Unwrapped') . PL.decodeMessage

encodeAppTxMessage
  :: AppTxMessage 
  -> ByteString
encodeAppTxMessage = \case
  ATMUpdateCount a -> PL.encodeMessage  $ a ^. from _Unwrapped'


--------------------------------------------------------------------------------

data UpdateCountTx = UpdateCountTx
  { updateCountTxUsername :: Text
  , updateCountTxCount    :: Int32
  } deriving (Show, Eq, Generic)

instance Binary UpdateCountTx

instance Wrapped UpdateCountTx where
  type Unwrapped UpdateCountTx = M.UpdateCount

  _Wrapped' = iso t f
    where
      t UpdateCountTx{..} =
        defMessage
          & M.username .~ updateCountTxUsername
          & M.count .~ updateCountTxCount
      f msg =
        UpdateCountTx { updateCountTxUsername = msg ^. M.username
                      , updateCountTxCount = msg ^. M.count
                      }
