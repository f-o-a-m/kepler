module Network.ABCI.Types.Messages.Common
  ( defaultABCIOptions
  ) where

import           Data.Aeson        (Options)
import           Data.Aeson.Casing (aesonDrop, snakeCase)

defaultABCIOptions :: String -> Options
defaultABCIOptions prefix = aesonDrop (length prefix) snakeCase
