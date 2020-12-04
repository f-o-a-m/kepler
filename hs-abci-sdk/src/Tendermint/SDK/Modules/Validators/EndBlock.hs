module Tendermint.SDK.Modules.Validators.EndBlock where

import qualified Network.ABCI.Types.Messages.Request     as Request
import qualified Network.ABCI.Types.Messages.Response    as Response
import           Polysemy
import           Tendermint.SDK.BaseApp.Block.Effect     (BlockEffs)
import qualified Tendermint.SDK.BaseApp.Store.Array      as A
import qualified Tendermint.SDK.BaseApp.Store.Var        as V
import           Tendermint.SDK.Modules.Validators.Store
import           Tendermint.SDK.Modules.Validators.Types

endBlockF
  :: Members BlockEffs r
  => Request.EndBlock
  -> Sem r Response.EndBlock
endBlockF _ = do
  height <- V.unsafeTakeVar heightVar
  updates <- updatesArray A.!! height
  case updates of
    Just (UpdatesList l) -> pure $ Response.EndBlock l Nothing []
    Nothing              -> pure $ Response.EndBlock [] Nothing []

