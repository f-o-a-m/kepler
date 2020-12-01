module Tendermint.SDK.BaseApp.Block.Types where

import           Control.Lens                         (lens)
import qualified Network.ABCI.Types.Messages.Request  as Request
import qualified Network.ABCI.Types.Messages.Response as Response
import           Tendermint.SDK.BaseApp.Router        (HasPath (..))

data EmptyBeginBlockServer = EmptyBeginBlockServer

newtype BeginBlockRequest = BeginBlockRequest Request.BeginBlock

instance HasPath BeginBlockRequest where
  path = lens (const "") const

type BeginBlockApplication m = BeginBlockRequest -> m Response.BeginBlock


data EmptyEndBlockServer = EmptyEndBlockServer

newtype EndBlockRequest = EndBlockRequest Request.EndBlock

instance HasPath EndBlockRequest where
  path = lens (const "") const

type EndBlockApplication m = EndBlockRequest -> m Response.EndBlock
