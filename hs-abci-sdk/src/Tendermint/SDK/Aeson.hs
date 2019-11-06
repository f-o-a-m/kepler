module Tendermint.SDK.Aeson
  ( defaultSDKAesonOptions
  ) where

import           Data.Aeson        (Options)
import           Data.Aeson.Casing (aesonDrop, snakeCase)

defaultSDKAesonOptions :: String -> Options
defaultSDKAesonOptions prefix = aesonDrop (length prefix) snakeCase
