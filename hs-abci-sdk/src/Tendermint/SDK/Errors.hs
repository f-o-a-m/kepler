module Tendermint.SDK.Errors
  ( AppError(..)
  , IsAppError(..)
  , queryAppError
  , checkTxAppError
  , deliverTxAppError
  , SDKError(..)
  , throwSDKError
  , MessageParseError(..)
  , MessageSemanticError(..)
  ) where

import           Control.Exception                    (Exception)
import           Control.Lens                         (Lens', lens)
import           Data.Text                            (Text, intercalate)
import           Data.Word                            (Word32)
import qualified Network.ABCI.Types.Messages.Response as Response
import           Polysemy
import           Polysemy.Error                       (Error, throw)

-- | This type represents a common error response for the query, checkTx,
-- | and deliver tx abci-messages.
data AppError = AppError
  { appErrorCode      :: Word32
  , appErrorCodespace :: Text
  , appErrorMessage   :: Text
  } deriving Show

instance Exception AppError

-- | Allows for custom application error types to be coerced into the
-- standard error resposne.
class IsAppError e where
  makeAppError :: e -> AppError

-- | This class is used to set the 'AppError' data into the appropriate
-- | response fields for the query abci-message.
queryAppError :: Lens' Response.Query AppError
queryAppError = lens g s
  where
    g Response.Query{..} = AppError
      { appErrorCode = queryCode
      , appErrorCodespace = queryCodespace
      , appErrorMessage = queryLog
      }
    s query AppError{..} = query
      { Response.queryCode = appErrorCode
      , Response.queryCodespace  = appErrorCodespace
      , Response.queryLog = appErrorMessage
      }

-- | This class is used to set the 'AppError' data into the appropriate
-- | response fields for the checkTx abci-message.
checkTxAppError :: Lens' Response.CheckTx AppError
checkTxAppError = lens g s
  where
    g Response.CheckTx{..} = AppError
      { appErrorCode = checkTxCode
      , appErrorCodespace = checkTxCodespace
      , appErrorMessage = checkTxLog
      }
    s checkTx AppError{..} = checkTx
      { Response.checkTxCode = appErrorCode
      , Response.checkTxCodespace  = appErrorCodespace
      , Response.checkTxLog = appErrorMessage
      }

-- | This class is used to set the 'AppError' data into the appropriate
-- | response fields for the deliverTx abci-message.
deliverTxAppError :: Lens' Response.DeliverTx AppError
deliverTxAppError = lens g s
  where
    g Response.DeliverTx{..} = AppError
      { appErrorCode = deliverTxCode
      , appErrorCodespace = deliverTxCodespace
      , appErrorMessage = deliverTxLog
      }
    s deliverTx AppError{..} = deliverTx
      { Response.deliverTxCode = appErrorCode
      , Response.deliverTxCodespace  = appErrorCodespace
      , Response.deliverTxLog = appErrorMessage
      }

--------------------------------------------------------------------------------
-- Stock SDK Errors
--------------------------------------------------------------------------------

-- | These errors originate from the SDK itself. The "sdk" namespace is reserved
-- | for this error type and should not be used in modules or applications.
data SDKError =
    InternalError
  -- ^ Something went wrong and we have no idea what.
  | ParseError Text
  -- ^ Parsing errors for SDK specific types, e.g. 'RawTransaction' or 'Msg', etc.
  | UnmatchedRoute Text
  -- ^ The name of the route that failed to match.
  | OutOfGasException
  | MessageValidation [Text]
  -- ^ Message validation errors.

-- | As of right now it's not expected that one can recover from an 'SDKError',
-- | so we are throwing them as 'AppError's directly.
throwSDKError
  :: Member (Error AppError) r
  => SDKError
  -> Sem r a
throwSDKError = throw . makeAppError

instance IsAppError SDKError where
  makeAppError InternalError = AppError
    { appErrorCode = 1
    , appErrorCodespace = "sdk"
    , appErrorMessage = "Internal Error"
    }

  makeAppError (ParseError msg) = AppError
    { appErrorCode = 2
    , appErrorCodespace = "sdk"
    , appErrorMessage = msg
    }

  makeAppError (UnmatchedRoute route) = AppError
    { appErrorCode = 3
    , appErrorCodespace = "sdk"
    , appErrorMessage = "Route not recognized: " <> route <> "."
    }

  makeAppError OutOfGasException = AppError
    { appErrorCode = 4
    , appErrorCodespace = "sdk"
    , appErrorMessage = "Out of gas exception"
    }

  makeAppError (MessageValidation errors) = AppError
    { appErrorCode = 5
    , appErrorCodespace = "sdk"
    , appErrorMessage = "Message failed validation: " <> intercalate "\n" errors
    }

--------------------------------------------------------------------------------
-- Message Errors
--------------------------------------------------------------------------------

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
