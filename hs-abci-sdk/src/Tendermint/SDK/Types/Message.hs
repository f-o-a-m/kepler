module Tendermint.SDK.Types.Message where

import           Control.Lens                 ((#), (&), (.~), (^.), Wrapped(..), iso, view, from)
import Data.ByteString (ByteString)
import           Data.String.Conversions      (cs)
import           Data.Text                    (Text)
import qualified Data.Validation              as V
import qualified Proto3.Wire.Decode           as Wire
import           Tendermint.SDK.Types.Address (Address)
import Data.Bifunctor (bimap)
import qualified Proto.Types.Transaction        as T
import qualified Proto.Types.Transaction_Fields as T
import Tendermint.SDK.Codec (HasCodec(..))
import qualified Data.ProtoLens                 as P

-- | The basic message format embedded in any transaction.
data Msg msg = Msg
  { msgAuthor :: Address
  , msgData   :: msg
  , msgType   :: Text
  }

instance Functor Msg where
  fmap f msg@Msg{msgData} = msg {msgData = f msgData}

data TypedMessage = TypedMessage
  { typedMsgData :: ByteString
  , typedMsgType :: Text
  }

instance Wrapped TypedMessage where
  type Unwrapped TypedMessage = T.TypedMessage

  _Wrapped' = iso t f
   where
    t TypedMessage {..} =
      P.defMessage
        & T.data' .~ typedMsgData
        & T.type' .~ typedMsgType
    f message = TypedMessage
      { typedMsgData = message ^. T.data'
      , typedMsgType = message ^. T.type'
      }

instance HasCodec TypedMessage where
  encode = P.encodeMessage . view _Wrapped'
  decode = bimap cs (view $ from _Wrapped') . P.decodeMessage

-- | This is a general error type, primarily accomodating protobuf messages being parsed
-- | by either the [proto3-wire](https://hackage.haskell.org/package/proto3-wire)
-- | or the [proto-lens](https://hackage.haskell.org/package/proto-lens) libraries.
data MessageParseError =
    -- | A 'WireTypeError' occurs when the type of the data in the protobuf
    -- binary format does not match the type encountered by the parser.
    WireTypeError Text
    -- | A 'BinaryError' occurs when we can't successfully parse the contents of
    -- the field.
  | BinaryError Text
    -- | An 'EmbeddedError' occurs when we encounter an error while parsing an
    -- embedded message.
  | EmbeddedError Text (Maybe MessageParseError)
    -- | Unknown or unstructured parsing error.
  | OtherParseError Text

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

-- | Used during message validation to indicate that although the message has parsed
-- | correctly, it fails certain sanity checks.
data MessageSemanticError =
    -- | Used to indicate that the message signer does not have the authority to send
    -- | this message.
    PermissionError Text
    -- | Used to indicate that a field isn't valid, e.g. enforces non-negative quantities
    -- | or nonempty lists.
  | InvalidFieldError Text
    -- Catchall for other erors
  | OtherSemanticError Text

formatMessageSemanticError
  :: MessageSemanticError
  -> Text
formatMessageSemanticError err =
    let (context, msg) = case err of
          PermissionError m    -> ("Permission Error", m)
          InvalidFieldError m  -> ("Invalid Field Error", m)
          OtherSemanticError m -> ("Other Error", m)
    in "Semantic Error [" <> context <> "]:" <> msg

class ValidateMessage msg where
  validateMessage :: Msg msg -> V.Validation [MessageSemanticError] ()

nonEmptyCheck
  :: Eq a
  => Monoid a
  => Text
  -> a
  -> V.Validation [MessageSemanticError] ()
nonEmptyCheck fieldName x
  | x == mempty = V._Failure # [InvalidFieldError $ fieldName <> " must be nonempty."]
  | otherwise = V.Success ()

isAuthorCheck
  :: Text
  -> Msg msg
  -> (msg -> Address)
  -> V.Validation [MessageSemanticError] ()
isAuthorCheck fieldName Msg{msgAuthor, msgData} getAuthor
  | getAuthor msgData /= msgAuthor = V._Failure # [PermissionError $ fieldName <> " must be message author."]
  | otherwise = V.Success ()
