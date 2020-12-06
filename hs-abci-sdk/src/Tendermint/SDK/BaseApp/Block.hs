module Tendermint.SDK.BaseApp.Block (BlockEffs, EndBlockResult (..), runBeginBlock, runEndBlock) where

import           Network.ABCI.Types.Messages.FieldTypes (ConsensusParams,
                                                         ValidatorUpdate)
import           Network.ABCI.Types.Messages.Response   as Response (BeginBlock (BeginBlock),
                                                                     EndBlock (EndBlock))
import           Polysemy                               (Members, Sem)
import           Polysemy.Error                         (Error, runError)
import           Polysemy.Output                        (Output, runOutputList)
import           Polysemy.Tagged                        (Tagged, tag)
import           Tendermint.SDK.BaseApp.Errors          (AppError)
import qualified Tendermint.SDK.BaseApp.Events          as E
import           Tendermint.SDK.BaseApp.Store.RawStore  (ReadStore,
                                                         Scope (Consensus),
                                                         WriteStore)
import           Tendermint.SDK.Types.Effects           ((:&))

type BlockEffs =
 [ WriteStore
 , ReadStore
 , Error AppError
 , Output E.Event
 ]


runBeginBlock
  :: Members [Tagged 'Consensus ReadStore, Tagged 'Consensus WriteStore] r
  => Sem (BlockEffs :& r) ()
  -> Sem r Response.BeginBlock
runBeginBlock b = do
  (events, _) <- (runOutputList . runError . tag . tag) b
  pure $ Response.BeginBlock events

data EndBlockResult = EndBlockResult [ValidatorUpdate] (Maybe ConsensusParams)

runEndBlock
  :: Members [Tagged 'Consensus ReadStore, Tagged 'Consensus WriteStore] r
  => Sem (BlockEffs :& r) EndBlockResult
  -> Sem r Response.EndBlock
runEndBlock b = do
  (events, res) <- (runOutputList . runError . tag . tag) b
  case res of
    Left _ -> pure $ Response.EndBlock [] Nothing events
    Right (EndBlockResult updates params) -> pure $ Response.EndBlock updates params events
