module Tendermint.SDK.Module where

--import Tendermint.SDK.Store
--import Tendermint.SDK.Codec
--import Network.ABCI.Types.Messages.Request (BeginBlock, EndBlock)
--
--data TendermintF msg store m a =
--    State (store -> m a)
--  | Transaction msg a
--  | BeginBlocker BeginBlock a
--  | EndBlocker EndBlock a
--  | Lift (m a)
--
--instance Functor m => Functor (TendermintF msg store m) where
--    fmap f a = case a of
--      State g -> State (fmap f . g)
--      Transaction msg b -> Transaction msg (f b)
--      BeginBlocker bb b -> BeginBlocker bb (f b)
--      EndBlocker eb b -> EndBlocker eb (f b)
--      Lift b -> Lift (f <$> b)

