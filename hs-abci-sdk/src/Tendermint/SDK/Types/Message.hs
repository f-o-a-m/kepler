module Tendermint.SDK.Types.Message where

import           Control.Lens                 (( # ))
import           Data.Text                    (Text)
import qualified Data.Validation              as V
import           Tendermint.SDK.Errors        (MessageSemanticError (..))
import           Tendermint.SDK.Types.Address (Address)

-- | The basic message format embedded in any transaction.
data Msg msg = Msg
  { msgAuthor :: Address
  , msgData   :: msg
  }

class ValidateMessage msg where
  validateMessage :: Msg msg -> V.Validation [MessageSemanticError] ()

nonEmptyCheck
  :: Eq a
  => Monoid a
  => Text
  -> a
  -> V.Validation [MessageSemanticError] ()
nonEmptyCheck fieldName x
  | x == mempty = V._Failure # [InvalidFieldError $ fieldName <> " must be nonempty."]
  | otherwise = V.Success ()

isAuthorCheck
  :: Text
  -> Msg msg
  -> (msg -> Address)
  -> V.Validation [MessageSemanticError] ()
isAuthorCheck fieldName Msg{msgAuthor, msgData} getAuthor
  | getAuthor msgData /= msgAuthor = V._Failure # [PermissionError $ fieldName <> " must be message author."]
  | otherwise = V.Success ()
