module SimpleStorage.Modules.SimpleStorage.Message
  ( SimpleStorageMessage(..)
  , UpdateCountTx(..)
  )where

import           Control.Lens                        (from, iso, view, (&),
                                                      (.~), (^.))
import           Control.Lens.Wrapped                (Wrapped (..), _Unwrapped')
import           Data.Bifunctor                      (first)
import           Data.Int                            (Int32)
import qualified Data.ProtoLens                      as PL
import           Data.ProtoLens.Message              (Message (..))
import           Data.Serialize.Text                 ()
import           Data.String.Conversions             (cs)
import           Data.Text                           (Text)
import           Data.Validation                     (Validation (..))
import           GHC.Generics                        (Generic)
import           Proto.SimpleStorage.Messages        as M
import           Proto.SimpleStorage.Messages_Fields as M
import           Tendermint.SDK.Codec                (HasCodec (..))
import           Tendermint.SDK.Types.Message        (Msg (..),
                                                      ValidateMessage (..))

data SimpleStorageMessage =
  UpdateCount UpdateCountTx

instance ValidateMessage SimpleStorageMessage where
  validateMessage m@Msg{msgData} = case msgData of
    UpdateCount msg -> validateMessage m {msgData = msg}

instance HasCodec SimpleStorageMessage where
  decode = first cs . fmap (UpdateCount . view _Unwrapped') . PL.decodeMessage
  encode = \case
    UpdateCount a -> PL.encodeMessage  $ a ^. from _Unwrapped'

--------------------------------------------------------------------------------

data UpdateCountTx = UpdateCountTx
  { updateCountTxUsername :: Text
  , updateCountTxCount    :: Int32
  } deriving (Show, Eq, Generic)

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
