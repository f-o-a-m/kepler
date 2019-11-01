module Tendermint.SDK.BaseApp where

import           Polysemy              (Member, Embed)
import           Polysemy.Output       (Output)
import           Polysemy.Resource     (Resource)
import           Tendermint.SDK.Events (Event, EventBuffer)
import           Tendermint.SDK.Logger (Logger, LogConfig)
import           Tendermint.SDK.Store  (RawStore)
import           Tendermint.SDK.AuthTreeStore (AuthTreeDriver)

type HasBaseApp r =
  ( Member Logger r
  , Member RawStore r
  , Member (Output Event) r
  , Member Resource r
  )

type BaseApp = 
  [ Output Event
  , RawStore
  , Logger
  , Resource
  , Embed IO
  ]


data BaseAppContext = BaseAppContext
  { baseAppLogConfig :: LogConfig
  , baseAppEventBuffer :: EventBuffer
  , baseAppAuthTreeDriver :: AuthTreeDriver
  }


-- TODO: write eval function for BaseApp using BaseAppContext