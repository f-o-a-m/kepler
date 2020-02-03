module Nameservice.Aeson where

import           Data.Aeson        (Options)
import           Data.Aeson.Casing (aesonDrop, snakeCase)

defaultNameserviceOptions :: String -> Options
defaultNameserviceOptions prefix = aesonDrop (length prefix) snakeCase
