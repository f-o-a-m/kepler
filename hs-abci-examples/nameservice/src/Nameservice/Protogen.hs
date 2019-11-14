{-# LANGUAGE MagicHash            #-}

-- @NOTE: ^ possibly the only language extension needed from tutorial
-- Seems to be the only requirement for generating the .proto string

module Nameservice.Protogen where

import qualified Data.ByteString.Lazy     as BL
import qualified Data.ByteArray.HexString as Hex
import           Data.Text.Lazy           (Text) -- @NOTE: lazy text
import           Data.Word                (Word64)
import           GHC.Exts                 (Proxy#, proxy#)
import           GHC.Generics
import           Proto3.Suite             (DotProtoDefinition, HasDefault (..),
                                           Message, MessageField (..), Named,
                                           Primitive (..), message,
                                           packageFromDefs, toProtoFileDef, fromByteString)
import           Proto3.Suite.DotProto    as DotProto
import qualified Proto3.Wire.Decode       as Decode
import qualified Proto3.Wire.Encode       as Encode

-- field instance derivations
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

-- messages
data SetName = SetName
  { setName  :: Name
  , setValue :: Text
  , setOwner :: Address
  } deriving (Show, Eq, Generic)

instance Message SetName
instance Named SetName

data BuyName = BuyName
  { buyName  :: Name
  , buyValue :: Text
  , buyBuyer :: Address
  , buyBid   :: Amount
  } deriving (Show, Eq, Generic)

instance Message BuyName
instance Named BuyName

data DeleteName = DeleteName
  { deleteName  :: Name
  , deleteOwner :: Address
  } deriving (Show, Eq, Generic)

instance Message DeleteName
instance Named DeleteName

-- scratch
testSN :: SetName
testSN = SetName (Name "satoshi") "cool cats" (Address "01")

testBN :: BuyName
testBN = BuyName (Name "satoshi") "cool cats" (Address "01") (Amount 999)

testDN :: DeleteName
testDN = DeleteName (Name "satoshi") (Address "01")

fromLazyByteString :: Message a => BL.ByteString -> Either Decode.ParseError a
fromLazyByteString = fromByteString . BL.toStrict

-- Encode/Decode
-- λ> fromLazyByteString $ Proto3.Suite.toLazyByteString testSN :: Either Decode.ParseError SetName
-- Right (SetName {setName = Name "satoshi", setValue = "cool cats", setOwner = Address HexString "0x01"})
-- λ> fromLazyByteString $ Proto3.Suite.toLazyByteString testBN :: Either Decode.ParseError BuyName
-- Right (BuyName {buyName = Name "satoshi", buyValue = "cool cats", buyBuyer = Address HexString "0x01", buyBid = Amount 999})
-- λ> fromLazyByteString $ Proto3.Suite.toLazyByteString testDN :: Either Decode.ParseError DeleteName
-- Right (DeleteName {deleteName = Name "satoshi", deleteOwner = Address HexString "0x01"})

-- extension
protoFile :: String
protoFile = toProtoFileDef $ packageFromDefs "nameservice"
  ([ message (proxy# :: Proxy# SetName)
   , message (proxy# :: Proxy# BuyName)
   , message (proxy# :: Proxy# DeleteName)
   ] :: [DotProtoDefinition])
