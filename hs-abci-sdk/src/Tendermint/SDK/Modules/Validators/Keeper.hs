{-# LANGUAGE TemplateHaskell #-}

module Tendermint.SDK.Modules.Validators.Keeper where

import qualified Data.Map.Strict                         as Map
import           Data.Maybe                              (fromMaybe)
import qualified Data.Set                                as Set
import           Data.Word                               (Word64)
import           Polysemy                                (Members, Sem,
                                                          interpret, makeSem)
import           Polysemy.Error                          (Error)
import           Tendermint.SDK.BaseApp                  (AppError, ReadStore,
                                                          WriteStore)
import qualified Tendermint.SDK.BaseApp.Store.List       as L
import qualified Tendermint.SDK.BaseApp.Store.Map        as M
import qualified Tendermint.SDK.BaseApp.Store.Var        as V
import           Tendermint.SDK.Modules.Validators.Store
import           Tendermint.SDK.Modules.Validators.Types


data ValidatorsKeeper m a where
  GetValidators :: ValidatorsKeeper m (Map.Map PubKey_ Word64)
  GetValidatorsKeys :: ValidatorsKeeper m (Set.Set PubKey_)
  GetPowerOf :: PubKey_ -> ValidatorsKeeper m Word64
  GetQueuedUpdates :: ValidatorsKeeper m (Map.Map PubKey_ Word64)
  GetQueuedUpdate :: PubKey_ -> ValidatorsKeeper m (Maybe Word64)
  QueueUpdate :: PubKey_ -> Word64 -> ValidatorsKeeper m ()
  QueueUpdates :: [(PubKey_, Word64)] -> ValidatorsKeeper m ()

makeSem ''ValidatorsKeeper

type ValidatorsEffs = '[ValidatorsKeeper]

eval
  :: Members [ReadStore, WriteStore, Error AppError] r
  => Sem (ValidatorsKeeper : r) a
  -> Sem r a
eval = interpret (\case
  GetValidators -> getValidatorsF
  GetValidatorsKeys -> getValidatorsKeysF
  GetPowerOf key -> getPowerOfF key
  GetQueuedUpdates -> getQueuedUpdatesF
  GetQueuedUpdate key -> getQueuedUpdateF key
  QueueUpdate key power -> queueUpdateF key power
  QueueUpdates updates -> queueUpdatesF updates
  )

getValidatorsF
  :: Members [ReadStore, Error AppError] r
  => Sem r (Map.Map PubKey_ Word64)
getValidatorsF = do
  keyList <- fmap Set.toList getValidatorsKeysF
  fmap Map.fromList $ mapM (\k -> fmap (\p -> (k, p)) (getPowerOfF k)) keyList

getValidatorsKeysF
  :: Members [ReadStore, Error AppError] r
  => Sem r (Set.Set PubKey_)
getValidatorsKeysF =
  fmap (maybe Set.empty (\(KeySet x) -> x)) $ V.takeVar validatorsKeySet

getPowerOfF
  :: Members [ReadStore, Error AppError] r
  => PubKey_
  -> Sem r Word64
getPowerOfF key =
  fmap (fromMaybe 0) $ M.lookup key validatorsMap

getQueuedUpdatesF
  :: Members [ReadStore, Error AppError] r
  => Sem r (Map.Map PubKey_ Word64)
getQueuedUpdatesF = L.foldl (\m ValidatorUpdate{..} ->
  Map.alter (maybe (Just power) Just) key m) Map.empty updatesList

getQueuedUpdateF
  :: Members [ReadStore, Error AppError] r
  => PubKey_
  -> Sem r (Maybe Word64)
getQueuedUpdateF key =
  fmap (Map.lookup key) getQueuedUpdatesF

queueUpdateF
  :: Members [ReadStore, WriteStore, Error AppError] r
  => PubKey_
  -> Word64
  -> Sem r ()
queueUpdateF key power =
  L.append (ValidatorUpdate key power) updatesList

queueUpdatesF
  :: Members [ReadStore, WriteStore, Error AppError] r
  => [(PubKey_, Word64)]
  -> Sem r ()
queueUpdatesF = mapM_ $ uncurry queueUpdateF

