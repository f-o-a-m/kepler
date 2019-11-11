module Nameservice.Modules.Nameservice

  (
    -- * types
    Name(..)
  , Whois (..)
  , NameserviceException(..)

  -- * effects
  , NameserviceEffR
  , HasNameserviceEff
  , getWhois
  , nameIsAvailable
  , buyName

  -- * interpreter
  , eval

  -- * query API
  , Api
  , server

  ) where

import           Nameservice.Modules.Nameservice.Keeper
import           Nameservice.Modules.Nameservice.Messages
import           Nameservice.Modules.Nameservice.Query
import           Nameservice.Modules.Nameservice.Types
