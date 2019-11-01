module Tendermint.SDK.BaseApp where

import           Polysemy              (Member)
import           Polysemy.Output       (Output)
import           Polysemy.Resource     (Resource)
import           Tendermint.SDK.Events (Event)
import           Tendermint.SDK.Logger (Logger)
import           Tendermint.SDK.Store  (RawStore)

type BaseApp r =
  ( Member Logger r
  , Member RawStore r
  , Member (Output Event) r
  , Member Resource r
  )
