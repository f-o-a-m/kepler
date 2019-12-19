# Message

## Message Types

The `Message` module is ultimately a small state machine used for processing messages. Each module must define what messages it accepts, if any. Like many other types found in the SDK, this message class must implement the `HasCodec` class. We recommend using a protobuf serialization format for messages using either the `proto3-suite` or `proto-lens` libraries, though in theory you could use anything (e.g. `JSON`).

### `proto3-suite`
The advantages of using the `proto3-suite` library are that it has support for generics and that you can generate a `.proto` file from your haskell code for export to other applications. This is particularly useful when prototyping or when you have control over the message specification. 
The disadvantage is that `proto3-suite` doesn't act as a `protoc` plugin, and instead uses it's own protobuf parser. This means that you do not have access to the full protobuf specs when parsing `.proto` files.

### `proto-lens`
The advantages of using `proto-lens` are that it can parse and generate types for pretty much any `.proto` file. 
The disadvantage is that the generated code is a bit strange, and may require you to create wrapper types to avoid depending directly on the generated code. An additional disadvantage is that you cannot generate `.proto` files from haskell code.

All in all, neither is really difficult to work with, and depending on what stage you're at in development you might chose one over the other.

## Tutorial.Nameservice.Message

~~~ haskell
module Tutorial.Nameservice.Message where

import Data.Bifunctor (first)
import Data.Foldable (sequenceA_)
import Data.String.Conversions (cs)
import Data.Text (Text)
import GHC.Generics (Generic)
import Nameservice.Modules.Nameservice.Types (Name(..))
import Nameservice.Modules.Token (Amount)
import Nameservice.Modules.TypedMessage (TypedMessage(..))
import Proto3.Suite (Named, Message, fromByteString, toLazyByteString)
import Tendermint.SDK.Types.Address (Address)
import Tendermint.SDK.Types.Message (Msg(..), ValidateMessage(..),
                                     isAuthorCheck, nonEmptyCheck,
                                     coerceProto3Error, formatMessageParseError)
import Tendermint.SDK.Codec (HasCodec(..))
~~~

### Message Definitions

For the puroposes of the tutorial, we will use the `proto3-suite` for the message codecs:


~~~ haskell
data SetName = SetName
  { setNameName  :: Name
  , setNameOwner :: Address
  , setNameValue :: Text
  } deriving (Eq, Show, Generic)

instance Message SetName
instance Named SetName

instance HasCodec SetName where
  encode = cs . toLazyByteString
  decode = first (formatMessageParseError . coerceProto3Error) . fromByteString

data DeleteName = DeleteName
  { deleteNameOwner :: Address
  , deleteNameName  :: Name
  } deriving (Eq, Show, Generic)

instance Message DeleteName
instance Named DeleteName

instance HasCodec DeleteName where
  encode = cs . toLazyByteString
  decode = first (formatMessageParseError . coerceProto3Error) . fromByteString

data BuyName = BuyName
  { buyNameBid   :: Amount
  , buyNameName  :: Name
  , buyNameValue :: Text
  , buyNameBuyer :: Address
  } deriving (Eq, Show, Generic)

instance Message BuyName
instance Named BuyName

instance HasCodec BuyName where
  encode = cs . toLazyByteString
  decode = first (formatMessageParseError . coerceProto3Error) . fromByteString
~~~

We want a sum type that covers all possible messages the module can receive. As `protobuf` is a schemaless format, parsing is sometimes ambiguous if two types are the same up to field names, or one is a subset of the other. For this reason we defined a type called `TypedMessage`:

~~~ haskell ignore
data TypedMessage = TypedMessage
  { typedMessageType     :: Text
  , typedMessageContents :: BS.ByteString
  } deriving (Eq, Show, Generic)

instance Message TypedMessage
instance Named TypedMessage

instance HasCodec TypedMessage where
  encode = cs . toLazyByteString
  decode = first (formatMessageParseError . coerceProto3Error) . fromByteString
~~~

This allows us to disambiguated messages based on the `type` field, so that for example we can distinguish `DeleteName` from a submessage of `BuyName`. With that out of the way, we can define the module level (sum) message type:

~~~ haskell
data NameserviceMessage =
    NSetName SetName
  | NBuyName BuyName
  | NDeleteName DeleteName
  deriving (Eq, Show, Generic)

instance HasCodec NameserviceMessage where
  decode bs = do
    TypedMessage{..} <- decode bs
    case typedMessageType of
      "SetName" -> NSetName <$> decode typedMessageContents
      "DeleteName" -> NDeleteName <$> decode typedMessageContents
      "BuyName" -> NBuyName <$> decode typedMessageContents
      _ -> Left . cs $ "Unknown Nameservice message type " ++ cs typedMessageType
  encode = \case
    NSetName msg -> encode msg
    NBuyName msg -> encode msg
    NDeleteName msg -> encode msg
~~~

## Message Validation

Message validation is an important part of the transaction life cycle. When a `checkTx` message comes in, Tendermint is asking whether a transaction bytestring from the mempool is potentially runnable. At the very least this means that 

1. The transaction parses to a known message
2. The message passes basic signature authentication, if any is required.
3. The message author has enough funds for the gas costs, if any.
4. The message can be successfully routed to a module without handling.

On top of this you might wish to ensure other static properties of the message, such as that the author of the message is the owner of the funds being transfered. For this we have a `ValidateMessage` class:

~~~ haskell ignore
data MessageSemanticError =
    PermissionError Text
  | InvalidFieldError Text
  | OtherSemanticError Text

class ValidateMessage msg where
  validateMessage :: Msg msg -> Validation [MessageSemanticError] ()
~~~

We're using the applicative functor [`Data.Validation.Validation`](https://hackage.haskell.org/package/validation-1.1/docs/Data-Validation.html#t:Validation) to perform valdiation because it is capable of reporting all errors at once, rather than the first that occurs as in ther case with something like `Either`.

Here's what the `isAuthor` check looks like, that was described above:

~~~ haskell ignore
isAuthorCheck
  :: Text
  -> Msg msg
  -> (msg -> Address)
  -> V.Validation [MessageSemanticError] ()
isAuthorCheck fieldName Msg{msgAuthor, msgData} getAuthor
  | getAuthor msgData /= msgAuthor = 
      _Failure # [PermissionError $ fieldName <> " must be message author."]
  | otherwise = Success ()
~~~

It is also possible to run dynamic checks on the transaction, i.e. checks that need to query state in order to succeed or fail. We will say more on this later.

Here are the validation instances for our message types, which use some of the combinators defined in the SDK 

~~~ haskell
instance ValidateMessage SetName where
  validateMessage msg@Msg{..} =
    let SetName{setNameName, setNameValue} = msgData
        Name name = setNameName
    in sequenceA_
        [ nonEmptyCheck "Name" name
        , nonEmptyCheck "Value" setNameValue
        , isAuthorCheck "Owner" msg setNameOwner
        ]

instance ValidateMessage DeleteName where
  validateMessage msg@Msg{..} =
    let DeleteName{deleteNameName} = msgData
        Name name = deleteNameName
    in sequenceA_
       [ nonEmptyCheck "Name" name
       , isAuthorCheck "Owner" msg deleteNameOwner
       ]

instance ValidateMessage BuyName where
  validateMessage msg@Msg{..} =
    let BuyName{buyNameName, buyNameValue} = msgData
        Name name = buyNameName
    in sequenceA_
        [ nonEmptyCheck "Name" name
        , nonEmptyCheck "Value" buyNameValue
        , isAuthorCheck "Owner" msg buyNameBuyer
        ]
~~~

Finally we can define a `ValidateMessage` instance for our top level message type by dispatching on the message type:

~~~ haskell
instance ValidateMessage NameserviceMessage where
  validateMessage m@Msg{msgData} = case msgData of
    NBuyName msg    -> validateMessage m {msgData = msg}
    NSetName msg    -> validateMessage m {msgData = msg}
    NDeleteName msg -> validateMessage m {msgData = msg}
~~~

[Next: Keeper](Keeper.md)
