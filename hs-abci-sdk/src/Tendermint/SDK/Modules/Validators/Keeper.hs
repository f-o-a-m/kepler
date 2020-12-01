{-# LANGUAGE TemplateHaskell #-}

module Tendermint.SDK.Modules.Validators.Keeper where

import           Network.ABCI.Types.Messages.FieldTypes
import           Numeric.Natural
import           Polysemy
import           Polysemy.Error                          (Error)
import           Tendermint.SDK.BaseApp                  (AppError, ReadStore,
                                                          WriteStore)
import qualified Tendermint.SDK.BaseApp.Store.Array      as A
import qualified Tendermint.SDK.BaseApp.Store.Var        as V
import           Tendermint.SDK.Modules.Validators.Store
import           Tendermint.SDK.Modules.Validators.Types

data ValidatorsKeeper m a where
  SetPower :: PubKey -> Natural -> ValidatorsKeeper m ()

makeSem ''ValidatorsKeeper

type ValidatorsEffs = '[ValidatorsKeeper]

eval
  :: Members [ReadStore, WriteStore, Error AppError] r
  => Sem (ValidatorsKeeper : r) a
  -> Sem r a
eval = interpret (\case
  SetPower key power -> setPowerF key power
  )


setPowerF
  :: Members [ReadStore, WriteStore, Error AppError] r
  => PubKey
  -> Natural
  -> Sem r ()
setPowerF key power = do
  height <- V.unsafeTakeVar heightVar
  _ <- A.modifyAtIndex height (\(UpdatesList us) -> UpdatesList (update : us)) updatesArray
  pure ()
  where
    update = ValidatorUpdate (Just key) (WrappedVal (fromIntegral power))
