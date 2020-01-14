{-# OPTIONS_GHC -fno-warn-orphans #-}

module Nameservice.Test.EventOrphans where

import           Nameservice.Modules.Nameservice (NameClaimed, NameDeleted,
                                                  NameRemapped)
import           Tendermint.SDK.Modules.Token    (Faucetted, TransferEvent)
import qualified Tendermint.Utils.Events         as Event

-- Orphan instances for retrieving event logs for unit testing

instance Event.FromEvent NameClaimed
instance Event.FromEvent NameRemapped
instance Event.FromEvent NameDeleted
instance Event.FromEvent Faucetted
instance Event.FromEvent TransferEvent
