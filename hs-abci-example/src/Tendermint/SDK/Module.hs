module Tendermint.SDK.Module where

import           Polysemy
import           Tendermint.SDK.Logger
import           Tendermint.SDK.Store

type BaseApp r =
  ( Member Logger r
  , Member RawStore r
  )
