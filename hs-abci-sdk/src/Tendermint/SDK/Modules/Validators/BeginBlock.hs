module Tendermint.SDK.Modules.Validators.BeginBlock where

import qualified Network.ABCI.Types.Messages.Response    as Response
import           Polysemy
import           Tendermint.SDK.BaseApp
import qualified Tendermint.SDK.BaseApp.Block            as B
import qualified Tendermint.SDK.BaseApp.Store.Array      as A
import qualified Tendermint.SDK.BaseApp.Store.Var        as V
import           Tendermint.SDK.Modules.Validators.Store
import           Tendermint.SDK.Modules.Validators.Types



type BeginBlockAPI = B.BeginBlockRequest :~> Return Response.BeginBlock

beginBlocker
  :: Members BaseEffs r
  => Members B.BlockEffs r
  => B.RouteBB BeginBlockAPI r
beginBlocker = beginBlockH


beginBlockH
  :: Members B.BlockEffs r
  => B.BeginBlockRequest
  -> Sem r Response.BeginBlock
beginBlockH _ = do
  A.append (UpdatesList []) updatesArray -- Create a new list for updates from this block
  maybeLastHeight <- V.takeVar heightVar
  case maybeLastHeight of
    Nothing -> V.putVar 0 heightVar -- set current height to 0 if no existing height
    Just h  -> V.putVar (h+1) heightVar -- increment height otherwise
  pure $ Response.BeginBlock []

