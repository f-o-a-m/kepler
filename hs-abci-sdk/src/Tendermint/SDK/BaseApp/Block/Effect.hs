module Tendermint.SDK.BaseApp.Block.Effect (BlockEffs, runBeginBlock, runEndBlock) where

import           Network.ABCI.Types.Messages.Response  as Response
import           Polysemy                              (Embed, Members, Sem)

import           Polysemy.Error                        (Error, runError)
import           Polysemy.Tagged                       (Tagged, tag)
import           Tendermint.SDK.BaseApp.Errors         (AppError)
import           Tendermint.SDK.BaseApp.Store.RawStore
import           Tendermint.SDK.Types.Effects          ((:&))

type BlockEffs =
 [ WriteStore
 , ReadStore
 , Error AppError
 ]


evalBB
  :: Members [Tagged 'Consensus ReadStore,
                 Tagged 'Consensus WriteStore] r
  => Sem (BlockEffs :& r) Response.BeginBlock
  -> Sem r (Either AppError Response.BeginBlock)
evalBB = runError . tag . tag


evalEB
  :: Members [Tagged 'Consensus ReadStore,
              Tagged 'Consensus WriteStore] r
  => Sem (BlockEffs :& r) Response.EndBlock
  -> Sem r (Either AppError Response.EndBlock)
evalEB = runError . tag . tag

runBeginBlock
  :: Members [Embed IO, Tagged 'Consensus ReadStore, Tagged 'Consensus WriteStore] r
  => Sem (BlockEffs :& r) Response.BeginBlock
  -> Sem r Response.BeginBlock
runBeginBlock b = do
  r <- evalBB b
  case r of
    Left _    -> pure (Response.BeginBlock [])
    Right rbb -> pure rbb

runEndBlock
  :: Members [Embed IO, Tagged 'Consensus ReadStore, Tagged 'Consensus WriteStore] r
  => Sem (BlockEffs :& r) Response.EndBlock
  -> Sem r Response.EndBlock
runEndBlock b = do
  r <- evalEB b
  case r of
    Left _    -> pure (Response.EndBlock [] Nothing [])
    Right reb -> pure reb
