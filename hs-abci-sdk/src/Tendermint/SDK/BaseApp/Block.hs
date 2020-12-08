module Tendermint.SDK.BaseApp.Block
  ( BlockEffs
  , BlockContext(..)
  , newBlockContext
  , evalBlockHandler
  ) where

import           Data.IORef                                (newIORef)
import           Data.Proxy
--import qualified Network.ABCI.Types.Messages.Request       as Req
--import qualified Network.ABCI.Types.Messages.Response      as Resp
import           Polysemy
import           Polysemy.Tagged                           (Tagged (..))
--import qualified Tendermint.SDK.BaseApp.Events             as E
import           Control.Monad.IO.Class                    (liftIO)
import qualified Tendermint.SDK.BaseApp.Store              as Store
import qualified Tendermint.SDK.BaseApp.Transaction.Cache  as Cache
import           Tendermint.SDK.BaseApp.Transaction.Effect (TxEffs, runTx)
import           Tendermint.SDK.BaseApp.Transaction.Types  (TransactionContext (..))
import           Tendermint.SDK.Types.Effects              ((:&))

data BlockContext = BlockContext TransactionContext

newBlockContext :: IO BlockContext
newBlockContext = do
  initialCache <- newIORef Cache.emptyCache
  gasRemaining <- newIORef 0
  es <- newIORef []
  pure . BlockContext $
    TransactionContext
      { gasRemaining
      , txRequiresGas = False
      , storeCache = initialCache
      , events = es
      }

type BlockEffs = TxEffs

evalBlockHandler
  :: Members [Embed IO, Tagged 'Store.Consensus Store.ReadStore, Tagged 'Store.Consensus Store.WriteStore] r
  => Sem (BlockEffs :& r) ()
  -> Sem r ()
evalBlockHandler action = do
  (BlockContext txCtx)  <- liftIO newBlockContext
  (_,_,cache)  <- runTx (Proxy @'Store.Consensus) txCtx action
  maybe (pure ()) Cache.writeCache cache



-- module Tendermint.SDK.BaseApp.Block (BlockEffs, EndBlockResult (..), runBeginBlock, runEndBlock) where

-- import           Network.ABCI.Types.Messages.FieldTypes (ConsensusParams,
--                                                          ValidatorUpdate)
-- import           Network.ABCI.Types.Messages.Response   as Response (BeginBlock (BeginBlock),
--                                                                      EndBlock (EndBlock))
-- import           Polysemy                               (Members, Sem)
-- import           Polysemy.Error                         (Error, runError)
-- import           Polysemy.Output                        (Output, runOutputList)
-- import           Polysemy.Tagged                        (Tagged, tag)
-- import           Tendermint.SDK.BaseApp.Errors          (AppError)
-- import qualified Tendermint.SDK.BaseApp.Events          as E
-- import           Tendermint.SDK.BaseApp.Store.RawStore  (ReadStore,
--                                                          Scope (Consensus),
--                                                          WriteStore)
-- import           Tendermint.SDK.Types.Effects           ((:&))

-- type BlockEffs =
--  [ WriteStore
--  , ReadStore
--  , Error AppError
--  , Output E.Event
--  ]


-- runBeginBlock
--   :: Members [Tagged 'Consensus ReadStore, Tagged 'Consensus WriteStore] r
--   => Sem (BlockEffs :& r) ()
--   -> Sem r Response.BeginBlock
-- runBeginBlock b = do
--   (events, _) <- (runOutputList . runError . tag . tag) b
--   pure $ Response.BeginBlock events

-- data EndBlockResult = EndBlockResult [ValidatorUpdate] (Maybe ConsensusParams)

-- runEndBlock
--   :: Members [Tagged 'Consensus ReadStore, Tagged 'Consensus WriteStore] r
--   => Sem (BlockEffs :& r) EndBlockResult
--   -> Sem r Response.EndBlock
-- runEndBlock b = do
--   (events, res) <- (runOutputList . runError . tag . tag) b
--   case res of
--     Left _ -> pure $ Response.EndBlock [] Nothing events
--     Right (EndBlockResult updates params) -> pure $ Response.EndBlock updates params events