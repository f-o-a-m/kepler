module Tendermint.SDK.Modules.Validators.Query
  (
    querier
  , QueryApi
  )where

import qualified Data.Map.Strict                          as Map
import qualified Data.Set                                 as Set
import           Data.Word                                (Word64)
import           Polysemy                                 (Members, Sem)
import           Servant.API
import           Tendermint.SDK.BaseApp
import qualified Tendermint.SDK.BaseApp.Store.Map         as M
import qualified Tendermint.SDK.BaseApp.Store.Var         as V
import qualified Tendermint.SDK.Modules.Validators.Keeper as Keeper
import           Tendermint.SDK.Modules.Validators.Store
import           Tendermint.SDK.Modules.Validators.Types

type QueryApi = GetPowerOf :<|> GetValidatorsKeys :<|> GetValidators

querier
  :: Members QueryEffs r
  => Members Keeper.ValidatorsEffs r
  => RouteQ QueryApi r
querier =
  getPowerOfQuery :<|> getValidatorsKeys :<|> getValidators

type GetPowerOf = "powerOf" :> StoreLeaf (M.Map PubKey_ Word64)
getPowerOfQuery
  :: Members QueryEffs r
  => RouteQ GetPowerOf r
getPowerOfQuery = storeQueryHandler validatorsMap

type GetValidatorsKeys = "validatorsKeys" :> StoreLeaf (V.Var KeySet)
getValidatorsKeys
  :: Members QueryEffs r
  => RouteQ GetValidatorsKeys r
getValidatorsKeys = storeQueryHandler validatorsKeySet

type GetValidators = "validators" :> Leaf (Map.Map PubKey_ Word64)
getValidators
  :: Members Keeper.ValidatorsEffs r
  => Sem r (QueryResult (Map.Map PubKey_ Word64))
getValidators = do
  keyList <- fmap Set.toList Keeper.getValidatorsKeys
  vs <- fmap Map.fromList $ mapM (\k -> fmap (\p -> (k, p)) (Keeper.getPowerOf k)) keyList
  pure $ QueryResult
    { queryResultData = vs
    , queryResultIndex = 0
    , queryResultKey = ""
    , queryResultProof = Nothing
    , queryResultHeight = 0
    }




