{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}

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
import           Nameservice.Modules.Nameservice.Keys  (whoisMapKey)
import           Nameservice.Modules.Nameservice.Types
import qualified Tendermint.SDK.BaseApp                as BaseApp
import qualified Tendermint.SDK.BaseApp.Store.Map      as M
import           Tendermint.SDK.BaseApp.Store.TH       (makeSubStore)

data NameserviceNamespace

store :: BaseApp.Store NameserviceNamespace
store = BaseApp.makeStore $
  BaseApp.KeyRoot $ cs . symbolVal $ Proxy @NameserviceName

newtype Name = Name {unName :: Text} deriving (Eq, Show, A.ToJSON, A.FromJSON)

instance BaseApp.RawKey Name where
    rawKey = iso (\(Name n) -> cs n) (Name . cs)

instance BaseApp.QueryData Name

$(makeSubStore 'store "whoisMap" [t| M.Map Name Whois|] whoisMapKey)
