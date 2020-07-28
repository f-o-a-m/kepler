module SimpleStorage.Modules.SimpleStorage.Message
  ( UpdateCountTx(..)
  )where

import           Control.Lens                        (from, iso, view, (&),
                                                      (.~), (^.))
import           Control.Lens.Wrapped                (Wrapped (..))
import           Data.Bifunctor                      (bimap)
import           Data.Int                            (Int32)
import qualified Data.ProtoLens                      as P
import           Data.ProtoLens.Message              (Message (..))
import           Data.Serialize.Text                 ()
import           Data.String.Conversions             (cs)
import           Data.Text                           (Text)
import           Data.Validation                     (Validation (..))
import           GHC.Generics                        (Generic)
import           Proto.SimpleStorage.Messages        as M
import           Proto.SimpleStorage.Messages_Fields as M
import           Tendermint.SDK.Codec                (HasCodec (..))
import           Tendermint.SDK.Types.Message        (HasMessageType (..),
                                                      ValidateMessage (..))


data UpdateCountTx = UpdateCountTx
  { updateCountTxUsername :: Text
  , updateCountTxCount    :: Int32
  } deriving (Show, Eq, Generic)

instance HasMessageType UpdateCountTx where
  messageType _ = "update_count"

instance ValidateMessage UpdateCountTx where
  validateMessage _ = Success ()

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

instance HasCodec UpdateCountTx where
  encode = P.encodeMessage . view _Wrapped'
  decode = bimap cs (view $ from _Wrapped') . P.decodeMessage
