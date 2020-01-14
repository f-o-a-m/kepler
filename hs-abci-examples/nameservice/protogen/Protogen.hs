{-# LANGUAGE MagicHash #-}

-- @NOTE: ^ possibly the only language extension needed from tutorial
-- Seems to be the only requirement for generating the .proto string

module Protogen (messagesProtoFile, whoisProtoFile) where

import           Data.Aeson.Casing                        (snakeCase)
import qualified Data.ByteString.Lazy                     as BL
import           GHC.Exts                                 (Proxy#, proxy#)
import           Nameservice.Modules.Nameservice.Messages (BuyName (..),
                                                           DeleteName (..),
                                                           SetName (..))
import           Nameservice.Modules.Nameservice.Types    (Name (..),
                                                           Whois (..))
import           Proto3.Suite                             (DotProtoDefinition,
                                                           Message,
                                                           fromByteString,
                                                           message,
                                                           packageFromDefs)
import           Proto3.Suite.DotProto                    as DotProto
import           Proto3.Suite.DotProto.Rendering          (RenderingOptions,
                                                           defRenderingOptions)
import qualified Proto3.Wire.Decode                       as Decode
import           Proto3.Wire.Types                        (FieldNumber (..))
import           Tendermint.SDK.Modules.Token             (Amount (..))
import           Tendermint.SDK.Types.Address             (Address (..))
import qualified Text.PrettyPrint                         as PP


--------------------------------------------------------------------------------
-- Requires magic hash extension
--------------------------------------------------------------------------------

stripPrefixName :: DotProtoIdentifier -> DotProtoIdentifier -> FieldNumber -> PP.Doc
stripPrefixName (Single typeName) (Single fieldName) _ =
  let prefixLen = length typeName
      fieldName' = Single . snakeCase . drop prefixLen $ fieldName
  in pPrint fieldName'
-- @NOTE: we don't yet need for other identifiers
stripPrefixName _ _ _ = error "stripPrefixName unused case"

msgStripPrefixOptions :: RenderingOptions
msgStripPrefixOptions = defRenderingOptions { roSelectorName = stripPrefixName }

messagesProtoFile :: String
messagesProtoFile = toProtoFile msgStripPrefixOptions $ packageFromDefs "nameservice"
  ([ message (proxy# :: Proxy# SetName)
   , message (proxy# :: Proxy# BuyName)
   , message (proxy# :: Proxy# DeleteName)
   ] :: [DotProtoDefinition])

whoisProtoFile :: String
whoisProtoFile = toProtoFile msgStripPrefixOptions $ packageFromDefs "nameservice"
  ([ message (proxy# :: Proxy# Whois) ] :: [DotProtoDefinition])
