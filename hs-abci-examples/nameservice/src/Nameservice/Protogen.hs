{-# LANGUAGE MagicHash #-}

-- @NOTE: ^ possibly the only language extension needed from tutorial
-- Seems to be the only requirement for generating the .proto string

module Nameservice.Protogen where

import           Data.Aeson.Casing                        (snakeCase)
import qualified Data.ByteString.Lazy                     as BL
import           GHC.Exts                                 (Proxy#, proxy#)
import           Nameservice.Modules.Nameservice.Messages (MsgBuyName (..),
                                                           MsgDeleteName (..),
                                                           MsgSetName (..))
import           Nameservice.Modules.Nameservice.Types    (Name (..),
                                                           Whois (..))
import           Nameservice.Modules.Token                (Address (..),
                                                           Amount (..))
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
import qualified Text.PrettyPrint                         as PP

--------------------------------------------------------------------------------
-- test
--------------------------------------------------------------------------------

testSN :: MsgSetName
testSN = MsgSetName (Name "satoshi") "cool cats" (Address "01")

testBN :: MsgBuyName
testBN = MsgBuyName (Name "satoshi") "cool cats" (Address "01") (Amount 999)

testDN :: MsgDeleteName
testDN = MsgDeleteName (Name "satoshi") (Address "01")

-- @NOTE: for some reason, encoding results in a lazy bytestring
-- while the provided decoder only accepts a strict bytestring
fromLazyByteString :: Message a => BL.ByteString -> Either Decode.ParseError a
fromLazyByteString = fromByteString . BL.toStrict

--------------------------------------------------------------------------------
-- Requires magic hash extension
--------------------------------------------------------------------------------

stripPrefixName :: DotProtoIdentifier -> DotProtoIdentifier -> FieldNumber -> PP.Doc
stripPrefixName (Single typeName) (Single fieldName) _ =
  let prefixLen = length typeName
      fieldName' = Single . snakeCase . drop prefixLen $ fieldName
  in pPrint fieldName'
-- @NOTE: we don't yet need for other identifiers
stripPrefixName _ _ _ = undefined

msgStripPrefixOptions :: RenderingOptions
msgStripPrefixOptions = defRenderingOptions { roSelectorName = stripPrefixName }

messagesProtoFile :: String
messagesProtoFile = toProtoFile msgStripPrefixOptions $ packageFromDefs "nameservice"
  ([ message (proxy# :: Proxy# MsgSetName)
   , message (proxy# :: Proxy# MsgBuyName)
   , message (proxy# :: Proxy# MsgDeleteName)
   ] :: [DotProtoDefinition])

whoisProtoFile :: String
whoisProtoFile = toProtoFile msgStripPrefixOptions $ packageFromDefs "nameservice"
  ([ message (proxy# :: Proxy# Whois) ] :: [DotProtoDefinition])

