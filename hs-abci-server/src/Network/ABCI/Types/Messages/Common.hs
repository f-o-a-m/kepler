module Network.ABCI.Types.Messages.Common
  ( defaultABCIOptions
  ) where

import Data.Aeson (Options)
import Data.Aeson.Casing (camelCase, aesonDrop)

defaultABCIOptions :: String -> Options
defaultABCIOptions prefix = aesonDrop (length prefix) camelCase
