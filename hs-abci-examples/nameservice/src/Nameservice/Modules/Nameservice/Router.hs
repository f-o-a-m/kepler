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
  NSetName msg -> setName msg
  NBuyName msg -> buyName msg
  NDeleteName msg -> deleteName msg
