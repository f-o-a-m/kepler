module Tendermint.SDK.Modules.Validators where

import           Polysemy
import           Tendermint.SDK.Application
import           Tendermint.SDK.BaseApp
import           Tendermint.SDK.BaseApp.Block.Effect
import           Tendermint.SDK.Modules.Validators.BeginBlock
import           Tendermint.SDK.Modules.Validators.EndBlock
import qualified Tendermint.SDK.Modules.Validators.Keeper     as Keeper
import           Tendermint.SDK.Modules.Validators.Types


import           Network.ABCI.Server.App



import           Data.Proxy
import           Tendermint.SDK.Crypto                        (Secp256k1)
-- import Tendermint.SDK.Application.Handlers
import qualified Tendermint.SDK.Modules.Auth                  as A

type Validators = Module ValidatorsName EmptyTxServer EmptyTxServer EmptyQueryServer Keeper.ValidatorsEffs '[]

validatorsModule ::
  -- Members (ModuleEffs Validators) r =>
  Validators r
validatorsModule =
  Module
    { moduleTxDeliverer = EmptyTxServer,
      moduleTxChecker = EmptyTxServer,
      moduleQuerier = EmptyQueryServer,
      moduleEval = Keeper.eval
    }


--- application construction example / type check test

type ValidatorsModules =
  '[ Validators, A.Auth ]


hc :: HandlersContext Secp256k1 ValidatorsModules CoreEffs
hc = HandlersContext
  { signatureAlgP = Proxy @Secp256k1
  , modules = valModules
  , beginBlockers = [runBeginBlock . beginBlockF]
  , endBlockers = [runEndBlock . endBlockF]
  , compileToCore  = defaultCompileToCore
  , anteHandler = baseAppAnteHandler
  }
  where
    valModules = validatorsModule :+ A.authModule :+ NilModules

valApp :: App (Sem CoreEffs)
valApp = makeApp hc
