module Nameservice.Modules.Nameservice.Store
  ( Name(..)
  , whoisMap
  ) where

import           Control.Lens                          (iso)
import qualified Data.Aeson                            as A
import           Data.Proxy
import           Data.String.Conversions               (cs)
import           Data.Text                             (Text)
import           GHC.TypeLits                          (symbolVal)
import           Nameservice.Modules.Nameservice.Types
import qualified Tendermint.SDK.BaseApp                as BaseApp
import qualified Tendermint.SDK.BaseApp.Store.Map      as M

data NameserviceNamespace

store :: BaseApp.Store NameserviceNamespace
store = BaseApp.makeStore $
  BaseApp.KeyRoot $ cs . symbolVal $ Proxy @NameserviceName

newtype Name = Name {unName :: Text} deriving (Eq, Show, A.ToJSON, A.FromJSON)

instance BaseApp.RawKey Name where
    rawKey = iso (\(Name n) -> cs n) (Name . cs)

instance BaseApp.QueryData Name

data WhoisMapKey = WhoisMapKey

instance BaseApp.RawKey WhoisMapKey where
    rawKey = iso (const "whoisMap") (const WhoisMapKey)

instance BaseApp.IsKey WhoisMapKey NameserviceNamespace where
  type Value WhoisMapKey NameserviceNamespace = M.Map Name Whois

whoisMap :: M.Map Name Whois
whoisMap = M.makeMap WhoisMapKey store
