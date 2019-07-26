module Network.ABCI.Types.DecodeError where

import qualified Data.ByteString         as BS
import qualified Data.ByteString.Base16  as BS16
import qualified Data.ProtoLens          as PL
import           Data.String.Conversions (cs)

data DecodeError
  = CanNotDecodeRequest
      { canNotDecodeRequestRequest :: BS.ByteString
      , canNotDecodeRequestError   :: String
      }
  | NoValueInRequest
      { noValueInRequestRequest       :: BS.ByteString
      , noValueInRequestUnknownFields :: PL.FieldSet
      }
  | ProtoLensParseError
      { protoLensParseErrorRequest :: BS.ByteString
      , protoLensParseErrorError   :: String
      }
  | InvalidPrefix
      { invalidPrefixMsg    :: BS.ByteString
      , invalidPrefixPrefix :: BS.ByteString
      }

print :: DecodeError -> String
print e =
  case e of
    CanNotDecodeRequest {canNotDecodeRequestRequest, canNotDecodeRequestError} ->
      "Got decoding error: "
        <> canNotDecodeRequestError
        <> " for request: "
        <> showBS canNotDecodeRequestRequest
    NoValueInRequest {noValueInRequestRequest, noValueInRequestUnknownFields} ->
      "Got empty request: "
        <> showBS noValueInRequestRequest
        <> " with unknown fields: "
        <> show (map showFields noValueInRequestUnknownFields)
    ProtoLensParseError {protoLensParseErrorRequest, protoLensParseErrorError} ->
      "Got parse error while parsing length prefix: "
        <> show protoLensParseErrorError
        <> " for request: "
        <> showBS protoLensParseErrorRequest
    InvalidPrefix {invalidPrefixMsg, invalidPrefixPrefix} ->
      "Got Invalid length prefix: "
        <> showBS invalidPrefixPrefix
        <> " for request: "
        <> showBS invalidPrefixMsg
  where
    showBS v = show $ (cs . BS16.encode $ v :: String)
    showFields (PL.TaggedValue tag _) = show tag
