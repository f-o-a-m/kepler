module Tendermint.SDK.BaseApp.Block
  ( serveBeginBlockApplication
  , serveEndBlockApplication
  , HasBeginBlockRouter(..)
  , HasEndBlockRouter(..)
  , BlockEffs
  , BeginBlockRequest (..)
  , EndBlockRequest (..)
  , BeginBlockApplication
  , EndBlockApplication
  , EmptyBeginBlockServer (..)
  , EmptyEndBlockServer (..)
  ) where

import           Data.Default.Class                   (def)
import           Data.Proxy                           (Proxy)
import qualified Network.ABCI.Types.Messages.Response as Response
import           Polysemy                             (Sem)
import           Tendermint.SDK.BaseApp.Block.Effect
import           Tendermint.SDK.BaseApp.Block.Router
import           Tendermint.SDK.BaseApp.Block.Types
import           Tendermint.SDK.BaseApp.Router
import           Tendermint.SDK.Types.Effects



serveBeginBlockApplication ::
  HasBeginBlockRouter layout r =>
  Proxy layout ->
  Proxy r ->
  RouteBB layout (BlockEffs :& r) ->
  BeginBlockApplication (Sem r)
serveBeginBlockApplication pl pr server =
  toBeginBlockApplication (runRouter (routeBB pl pr (emptyDelayed (Route server))) ())

toBeginBlockApplication ::
  Application (Sem r) BeginBlockRequest Response.BeginBlock ->
  BeginBlockApplication (Sem r)
toBeginBlockApplication ra beginBlock = do
  res <- ra beginBlock
  case res of
    Fail _      -> pure $ def
    FailFatal _ -> pure $ def
    Route a     -> pure a


-------------------------------------------------



serveEndBlockApplication ::
  HasEndBlockRouter layout r =>
  Proxy layout ->
  Proxy r ->
  RouteEB layout (BlockEffs :& r) ->
  EndBlockApplication (Sem r)
serveEndBlockApplication pl pr server =
  toEndBlockApplication (runRouter (routeEB pl pr (emptyDelayed (Route server))) ())

toEndBlockApplication ::
  Application (Sem r) EndBlockRequest Response.EndBlock ->
  EndBlockApplication (Sem r)
toEndBlockApplication ra endBlock = do
  res <- ra endBlock
  case res of
    Fail _      -> pure def
    FailFatal _ -> pure def
    Route a     -> pure a
