module Network.ABCI.Types.Messages.Common
  ( defaultABCIOptions
  , makeABCILenses
  ) where

import           Control.Lens        ((&), (.~))
import           Control.Lens.TH     (DefName (TopName), lensField, lensRules,
                                      makeLensesWith)
import           Data.Aeson          (Options)
import           Data.Aeson.Casing   (aesonDrop, snakeCase)
import           Language.Haskell.TH
--import           Language.Haskell.TH.Syntax (Name, nameBase)


defaultABCIOptions :: String -> Options
defaultABCIOptions prefix = aesonDrop (length prefix) snakeCase

makeABCILenses :: Name -> DecsQ
makeABCILenses = makeLensesWith $ lensRules
  & lensField .~ \_ _ name -> [TopName (mkName $ '_' : nameBase name)]
