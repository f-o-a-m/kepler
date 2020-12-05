module Tendermint.SDK.BaseApp.Block.Effect (BlockEffs, runBeginBlock, runEndBlock) where

import           Network.ABCI.Types.Messages.Response  as Response (BeginBlock (BeginBlock),
                                                                    EndBlock (EndBlock))
import           Polysemy                              (Members, Sem)

import           Polysemy.Error                        (Error, runError)
import           Polysemy.Tagged                       (Tagged, tag)
import           Tendermint.SDK.BaseApp.Errors         (AppError)
import           Tendermint.SDK.BaseApp.Store.RawStore (ReadStore,
                                                        Scope (Consensus),
                                                        WriteStore)
import           Tendermint.SDK.Types.Effects          ((:&))
import           Data.Either

type BlockEffs =
 [ WriteStore
 , ReadStore
 , Error AppError
 ]

runBeginBlock
  :: Members [Tagged 'Consensus ReadStore, Tagged 'Consensus WriteStore] r
  => Sem (BlockEffs :& r) Response.BeginBlock
  -> Sem r Response.BeginBlock
runBeginBlock b = do
  res <- (runError . tag . tag) b
  pure $ fromRight (Response.BeginBlock []) res

runEndBlock
  :: Members [Tagged 'Consensus ReadStore, Tagged 'Consensus WriteStore] r
  => Sem (BlockEffs :& r) Response.EndBlock
  -> Sem r Response.EndBlock
runEndBlock b = do
  res <- (runError . tag . tag) b
  pure $ fromRight (Response.EndBlock [] Nothing []) res
