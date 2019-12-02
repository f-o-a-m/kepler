module Tendermint.SDK.Format where

import           Data.String.Conversions (cs)
import           Data.Text               (Text)
import qualified Proto3.Wire.Decode      as Wire
import           Tendermint.SDK.Errors   (MessageParseError (..),
                                          MessageSemanticError (..))

-- | Useful for returning in error logs or console logging.
formatMessageParseError
  :: MessageParseError
  -> Text
formatMessageParseError = cs . go
  where
    go err =
      let (context,msg) = case err of
             WireTypeError txt -> ("Wire Type Error", txt)
             BinaryError txt -> ("Binary Error", txt)
             EmbeddedError txt err' -> ("Embedded Error", txt <> ". " <>  maybe "" go err')
             OtherParseError txt -> ("Other Error", txt)
      in "Parse Error [" <> context <> "]: " <> msg

-- Used to facilitate writing 'HasCodec' instances for protobuf messages that use
-- the proto3-suite library.
coerceProto3Error
  :: Wire.ParseError
  -> MessageParseError
coerceProto3Error = \case
  Wire.WireTypeError txt -> WireTypeError (cs txt)
  Wire.BinaryError txt -> BinaryError (cs txt)
  Wire.EmbeddedError txt merr -> EmbeddedError (cs txt) (coerceProto3Error <$> merr)

-- Used to facilitate writing 'HasCodec' instances for protobuf messages that use
-- the proto-lens library.
coerceProtoLensError
  :: String
  -> MessageParseError
coerceProtoLensError = OtherParseError . cs

formatMessageSemanticError
  :: MessageSemanticError
  -> Text
formatMessageSemanticError err =
    let (context, msg) = case err of
          PermissionError m    -> ("Permission Error", m)
          InvalidFieldError m  -> ("Invalid Field Error", m)
          OtherSemanticError m -> ("Other Error", m)
    in "Semantic Error [" <> context <> "]:" <> msg
