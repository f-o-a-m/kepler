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
