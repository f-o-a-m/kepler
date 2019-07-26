module Network.ABCI.Types.Messages.Types where

-- | Used to parametrize Request and Respone types
data MessageType =
    MTEcho
  | MTFlush
  | MTInfo
  | MTSetOption
  | MTInitChain
  | MTQuery
  | MTBeginBlock
  | MTCheckTx
  | MTDeliverTx
  | MTEndBlock
  | MTCommit
