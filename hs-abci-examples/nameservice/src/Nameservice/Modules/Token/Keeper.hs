module Nameservice.Modules.Token.Keeper where

import qualified Tendermint.SDK.Store            as Store

storeKey :: Store.StoreKey "token"
storeKey = Store.StoreKey "token"
