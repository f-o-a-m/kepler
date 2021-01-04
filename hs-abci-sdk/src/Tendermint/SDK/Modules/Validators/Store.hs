{-# LANGUAGE TemplateHaskell #-}

module Tendermint.SDK.Modules.Validators.Store
  (
    updatesList
  , validatorsMap
  , validatorsKeySet
  ) where

import           Data.Word                               (Word64)
import           Tendermint.SDK.BaseApp                  (KeyRoot (..), Store,
                                                          makeStore)
import qualified Tendermint.SDK.BaseApp.Store.List       as L
import qualified Tendermint.SDK.BaseApp.Store.Map        as M
import           Tendermint.SDK.BaseApp.Store.TH         (makeSubStore)
import qualified Tendermint.SDK.BaseApp.Store.Var        as V
import           Tendermint.SDK.Modules.Validators.Types


store :: Store ValidatorsNameSpace
store = makeStore $ KeyRoot "validators"

$(makeSubStore 'store "updatesList" [t|L.List ValidatorUpdate_|] updatesListKey)

$(makeSubStore 'store "validatorsMap" [t|M.Map PubKey_ Word64|] validatorsMapKey)

$(makeSubStore 'store "validatorsKeySet" [t|V.Var KeySet|] validatorsKeySetKey)

