module Tendermint.SDK.Modules.Validators.EndBlock where

import qualified Network.ABCI.Types.Messages.Response    as Response
import           Polysemy
import           Tendermint.SDK.BaseApp
import qualified Tendermint.SDK.BaseApp.Block            as B
import qualified Tendermint.SDK.BaseApp.Store.Array      as A
import qualified Tendermint.SDK.BaseApp.Store.Var        as V
import           Tendermint.SDK.Modules.Validators.Store
import           Tendermint.SDK.Modules.Validators.Types


type EndBlockAPI = B.EndBlockRequest :~> Return Response.EndBlock

endBlocker
  :: Members BaseEffs r
  => Members B.BlockEffs r
  => B.RouteEB EndBlockAPI r
endBlocker = endBlockH


endBlockH
  :: Members B.BlockEffs r
  => B.EndBlockRequest
  -> Sem r Response.EndBlock
endBlockH _ = do
  height <- V.unsafeTakeVar heightVar
  updates <- updatesArray A.!! height
  case updates of
    Just (UpdatesList l) -> pure $ Response.EndBlock l Nothing []
    Nothing              -> pure $ Response.EndBlock [] Nothing []
