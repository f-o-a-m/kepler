module Nameservice.Modules.Nameservice.Router where

import           Nameservice.Modules.Nameservice.Keeper   (HasNameserviceEff,
                                                           buyName, deleteName,
                                                           setName)
import           Nameservice.Modules.Nameservice.Messages (NameserviceMessage (..))
import           Nameservice.Modules.Token                (HasTokenEff)
import           Polysemy                                 (Sem)

router
  :: HasTokenEff r
  => HasNameserviceEff r
  => NameserviceMessage
  -> Sem r ()
router = \case
  SetName msg -> setName msg
  BuyName msg -> buyName msg
  DeleteName msg -> deleteName msg
