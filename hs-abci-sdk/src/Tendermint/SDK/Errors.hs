module Tendermint.SDK.Errors
  ( AppError(..)
  , IsAppError(..)
  ) where

import           Data.Int  (Int32)
import           Data.Text (Text)

data AppError = AppError
  { appErrorCode      :: Int32
  , appErrorCodeSpace :: Text
  , appErrorMessage   :: Text
  }

class IsAppError e where
  makeAppError :: e -> AppError
