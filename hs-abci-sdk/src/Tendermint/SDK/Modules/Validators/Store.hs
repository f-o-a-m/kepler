{-# LANGUAGE TemplateHaskell #-}

module Tendermint.SDK.Modules.Validators.Store where

import           Data.Word                               (Word64)
import           Tendermint.SDK.BaseApp                  (KeyRoot (..), Store,
                                                          makeStore)
import qualified Tendermint.SDK.BaseApp.Store.Array      as A
import           Tendermint.SDK.BaseApp.Store.TH         (makeSubStore)
import qualified Tendermint.SDK.BaseApp.Store.Var        as V
import           Tendermint.SDK.Modules.Validators.Types

--------------------------------------------------------------------------------



store :: Store ValidatorsNameSpace
store = makeStore $ KeyRoot "validators"

$(makeSubStore 'store "heightVar" [t|V.Var Word64|] heightKey)

$(makeSubStore 'store "updatesArray" [t|A.Array UpdatesList|] updatesKey)
