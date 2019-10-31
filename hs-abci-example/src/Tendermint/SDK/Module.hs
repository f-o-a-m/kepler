module Tendermint.SDK.Module where

import           Polysemy
import           Polysemy.Output
import           Tendermint.SDK.Events
import           Tendermint.SDK.Logger
import           Tendermint.SDK.Store

type BaseApp r =
  ( Member Logger r
  , Member RawStore r
  , Member (Output Event) r
  )
