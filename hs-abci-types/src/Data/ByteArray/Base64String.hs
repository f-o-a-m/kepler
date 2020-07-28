{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module Data.ByteArray.Base64String where

import           Data.Aeson              (FromJSON (..), ToJSON (..),
                                          Value (..), withText)
import           Data.ByteArray          (ByteArray, ByteArrayAccess, convert)
import           Data.ByteArray.Encoding (Base (Base64), convertFromBase,
                                          convertToBase)
import           Data.ByteString         (ByteString)
import           Data.String             (IsString (..))
import           Data.Text               (Text)
import           Data.Text.Encoding      (decodeUtf8, encodeUtf8)


-- | Represents a Hex string. Guarantees that all characters it contains
--   are valid hex characters.
newtype Base64String = Base64String { unBase64String :: ByteString }
  deriving (Eq, Ord, Semigroup, Monoid, ByteArrayAccess, ByteArray)

instance Show Base64String where
    show = ("Base64String " ++) . show . format

instance IsString Base64String where
    fromString = base64String' . fromString
      where
        base64String' :: ByteString -> Base64String
        base64String' = either error id . base64String

instance FromJSON Base64String where
    parseJSON Null = pure (fromBytes ("" :: ByteString))
    parseJSON v = withText "Base64String" (either fail pure . base64String . encodeUtf8) v

instance ToJSON Base64String where
    toJSON = String . toText

-- | Smart constructor that works with any mixed casing of characters:
--   `Base64String "AA" == Base64String "0xAa" == Base64String "0xaA" == Base64String "aa"`
base64String :: ByteArray ba => ba -> Either String Base64String
base64String bs = Base64String <$> convertFromBase Base64 bs

-- | Reads a raw bytes and converts to hex representation.
fromBytes :: ByteArrayAccess ba => ba -> Base64String
fromBytes = Base64String . convert

-- | Access to the raw bytes of 'Base64String'.
toBytes :: ByteArray ba => Base64String -> ba
toBytes = convert . unBase64String

-- | Access to a 'Text' representation of the 'Base64String'
toText :: Base64String -> Text
toText = decodeUtf8 . convertToBase Base64 . unBase64String

-- | Access to a 'Text' representation of the 'Base64String'
format :: Base64String -> Text
format = toText
