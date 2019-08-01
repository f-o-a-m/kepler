module SimpleStorage.Types where

import           Codec.Serialise.Class               (Serialise)
import           Control.Lens                        (iso, (&), (.~), (^.))
import           Control.Lens.Wrapped                (Wrapped (..))
import           Data.Int                            (Int32)
import           Data.ProtoLens.Message              (Message (..))
import           Data.Text                           (Text)
import           GHC.Generics                        (Generic)
import           Proto.SimpleStorage.Messages        as M
import           Proto.SimpleStorage.Messages_Fields as M

data UpdateCountTx = UpdateCountTx
  { updateCountTxUsername :: Text
  , updateCountTxCount    :: Int32
  } deriving (Show, Eq, Generic)

instance Serialise UpdateCountTx

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
