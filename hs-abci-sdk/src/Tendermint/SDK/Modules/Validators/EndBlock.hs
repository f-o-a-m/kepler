module Tendermint.SDK.Modules.Validators.EndBlock where

import           Control.Monad.State                      (MonadTrans (lift),
                                                           execStateT, forM_,
                                                           modify)
import qualified Data.Map.Strict                          as Map
import qualified Data.Set                                 as Set
import qualified Network.ABCI.Types.Messages.FieldTypes   as ABCI
import qualified Network.ABCI.Types.Messages.Request      as Request
import           Polysemy                                 (Members, Sem)
import           Tendermint.SDK.BaseApp                   (BlockEffs,
                                                           EndBlockResult (..))
import qualified Tendermint.SDK.BaseApp.Store.List        as L
import qualified Tendermint.SDK.BaseApp.Store.Map         as M
import qualified Tendermint.SDK.BaseApp.Store.Var         as V
import           Tendermint.SDK.Modules.Validators.Keeper
import           Tendermint.SDK.Modules.Validators.Store
import           Tendermint.SDK.Modules.Validators.Types


endBlock
  :: Members BlockEffs r
  => Members ValidatorsEffs r
  => Request.EndBlock
  -> Sem r EndBlockResult
endBlock _ = do
  updatesMap <- getQueuedUpdates
  curValKeySet <- getValidatorsKeys

  -- update the Validators map and key set
  newValKeySet <- flip execStateT curValKeySet $
    forM_ (Map.toList updatesMap) $ \(key, newPower) ->
      if newPower == 0 then do
        -- delete from Validators map and key set
        lift $ M.delete key validatorsMap
        modify $ Set.delete key
      else do
        -- update power in Validators map and ensure key is in key set
        lift $ M.insert key newPower validatorsMap
        modify $ Set.insert key

  -- store new set of validator keys
  V.putVar (KeySet newValKeySet) validatorsKeySet

  -- reset the updatesList to empty
  L.deleteWhen (const True) updatesList

  -- return EndBlockResult with validator updates for Tendermint
  pure $ EndBlockResult (map convertToValUp (Map.assocs updatesMap)) Nothing
  where
    convertToValUp (PubKey_ key, power) =
      ABCI.ValidatorUpdate key (ABCI.WrappedVal (fromIntegral power))
