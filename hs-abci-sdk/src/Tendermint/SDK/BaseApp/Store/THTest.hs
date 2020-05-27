{-# LANGUAGE TemplateHaskell #-}

module Tendermint.SDK.BaseApp.Store.THTest where

import           Tendermint.SDK.BaseApp.Store.TH

{-
data CountKey = CountKey

instance BaseApp.RawKey CountKey where
    rawKey = iso (\_ -> cs countKey) (const CountKey)
      where
        countKey :: ByteString
        countKey = convert . hashWith SHA256 . cs @_ @ByteString $ ("count" :: String)


-}

data Nameserver

data Count = Count String

$(makeRootKey ''Nameserver ''Count "count")
