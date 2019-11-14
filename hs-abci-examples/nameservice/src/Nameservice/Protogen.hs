{-# LANGUAGE MagicHash #-}

-- @NOTE: ^ possibly the only language extension needed from tutorial
-- Seems to be the only requirement for generating the .proto string

module Nameservice.Protogen where

import           Data.Aeson.Casing               (snakeCase)
import qualified Data.ByteArray.HexString        as Hex
import qualified Data.ByteString.Lazy            as BL
import           Data.Text.Lazy                  (Text)
import           Data.Word                       (Word64)
import           GHC.Exts                        (Proxy#, proxy#)
import           GHC.Generics
import           Proto3.Suite                    (DotProtoDefinition,
                                                  HasDefault (..), Message,
                                                  MessageField (..), Named,
                                                  Primitive (..),
                                                  fromByteString, message,
                                                  packageFromDefs)
import           Proto3.Suite.DotProto           as DotProto
import           Proto3.Suite.DotProto.Rendering (RenderingOptions,
                                                  defRenderingOptions)
import qualified Proto3.Wire.Decode              as Decode
import qualified Proto3.Wire.Encode              as Encode
import           Proto3.Wire.Types               (FieldNumber (..))
import qualified Text.PrettyPrint                as PP

--------------------------------------------------------------------------------
-- field instance derivations
--------------------------------------------------------------------------------

newtype Address = Address Hex.HexString deriving (Eq, Show, Ord)
instance Primitive Address where
  encodePrimitive n (Address hx) = Encode.byteString n (Hex.toBytes hx)
  decodePrimitive = Address . Hex.fromBytes <$> Decode.byteString
  primType _ = Bytes
instance HasDefault Address where -- @NOTE: deriving generic gets rid of this; see below
  def = Address ""
  isDefault = (==) def
instance MessageField Address

newtype Name = Name Text deriving (Eq, Show, Generic)
instance Primitive Name where
  encodePrimitive n (Name txt) = Encode.text n txt
  decodePrimitive = Name <$> Decode.text
  primType _ = String
instance HasDefault Name
instance MessageField Name

newtype Amount = Amount Word64 deriving (Eq, Show, Generic, Ord)
instance Primitive Amount where
  encodePrimitive n (Amount amt) = Encode.uint64 n amt
  decodePrimitive = Amount <$> Decode.uint64
  primType _ = UInt64
instance HasDefault Amount
instance MessageField Amount

--------------------------------------------------------------------------------
-- messages
--------------------------------------------------------------------------------

data SetName = SetName
  { setNameName  :: Name
  , setNameValue :: Text
  , setNameOwner :: Address
  } deriving (Show, Eq, Generic)

instance Message SetName
instance Named SetName

-- same order of fields
data AnotherSetName = AnotherSetName
  { anotherSetNameName  :: Name
  , anotherSetNameValue :: Text
  , anotherSetNameOwner :: Address
  } deriving (Show, Eq, Generic)

instance Message AnotherSetName
instance Named AnotherSetName

-- different order of fields
data DifferentSetName = DifferentSetName
  { differentSetNameName  :: Name
  , differentSetNameOwner :: Address
  , differentSetNameValue :: Text
  } deriving (Show, Eq, Generic)

instance Message DifferentSetName
instance Named DifferentSetName

data BuyName = BuyName
  { buyNameName  :: Name
  , buyNameValue :: Text
  , buyNameBuyer :: Address
  , buyNameBid   :: Amount
  } deriving (Show, Eq, Generic)

instance Message BuyName
instance Named BuyName

data DeleteName = DeleteName
  { deleteNameName  :: Name
  , deleteNameOwner :: Address
  } deriving (Show, Eq, Generic)

instance Message DeleteName
instance Named DeleteName

--------------------------------------------------------------------------------
-- scratch
--------------------------------------------------------------------------------

testSN :: SetName
testSN = SetName (Name "satoshi") "cool cats" (Address "01")

testBN :: BuyName
testBN = BuyName (Name "satoshi") "cool cats" (Address "01") (Amount 999)

testDN :: DeleteName
testDN = DeleteName (Name "satoshi") (Address "01")

-- @NOTE: for some reason, encoding results in a lazy bytestring
-- while the provided decoder only accepts a strict bytestring
fromLazyByteString :: Message a => BL.ByteString -> Either Decode.ParseError a
fromLazyByteString = fromByteString . BL.toStrict

-- -- Encode/Decode
-- λ> fromLazyByteString $ Proto3.Suite.toLazyByteString testSN :: Either Decode.ParseError SetName
-- Right (SetName {setName = Name "satoshi", setValue = "cool cats", setOwner = Address HexString "0x01"})
-- λ> fromLazyByteString $ Proto3.Suite.toLazyByteString testBN :: Either Decode.ParseError BuyName
-- Right (BuyName {buyName = Name "satoshi", buyValue = "cool cats", buyBuyer = Address HexString "0x01", buyBid = Amount 999})
-- λ> fromLazyByteString $ Proto3.Suite.toLazyByteString testDN :: Either Decode.ParseError DeleteName
-- Right (DeleteName {deleteName = Name "satoshi", deleteOwner = Address HexString "0x01"})


-- -- from different types
-- WORKS
-- λ> fromLazyByteString $ Proto3.Suite.toLazyByteString testSN :: Either Decode.ParseError AnotherSetName
-- Right (AnotherSetName {anotherSetNameName = Name "satoshi", anotherSetNameValue = "cool cats", anotherSetNameOwner = Address HexString "0x01"})
-- NOPE - decodes incorrectly
-- λ> fromLazyByteString $ Proto3.Suite.toLazyByteString testSN :: Either Decode.ParseError DifferentSetName
-- Right (DifferentSetName {differentSetNameName = Name "satoshi", differentSetNameOwner = Address HexString "0x636f6f6c2063617473", differentSetNameValue = "\SOH"})

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

stripPrefixOptions :: RenderingOptions
stripPrefixOptions = defRenderingOptions { roSelectorName = stripPrefixName }

protoFile :: String
protoFile = toProtoFile stripPrefixOptions $ packageFromDefs "nameservice"
  ([ message (proxy# :: Proxy# SetName)
   , message (proxy# :: Proxy# BuyName)
   , message (proxy# :: Proxy# DeleteName)
   ] :: [DotProtoDefinition])
