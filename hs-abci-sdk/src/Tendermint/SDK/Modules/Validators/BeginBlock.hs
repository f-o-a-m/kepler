module Tendermint.SDK.Modules.Validators.BeginBlock where

import qualified Network.ABCI.Types.Messages.Request     as Request
import qualified Network.ABCI.Types.Messages.Response    as Response
import           Polysemy
import           Tendermint.SDK.BaseApp.Block.Effect     (BlockEffs)
import qualified Tendermint.SDK.BaseApp.Store.Array      as A
import qualified Tendermint.SDK.BaseApp.Store.Var        as V
import           Tendermint.SDK.Modules.Validators.Store
import           Tendermint.SDK.Modules.Validators.Types

beginBlockF
  :: Members BlockEffs r
  => Request.BeginBlock
  -> Sem r Response.BeginBlock
beginBlockF _ = do
  A.append (UpdatesList []) updatesArray -- Create a new list for updates from this block
  maybeLastHeight <- V.takeVar heightVar
  case maybeLastHeight of
    Nothing -> V.putVar 0 heightVar -- set current height to 0 if no existing height
    Just h  -> V.putVar (h+1) heightVar -- increment height otherwise
  pure $ Response.BeginBlock []
