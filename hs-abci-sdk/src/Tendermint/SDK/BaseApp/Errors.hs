module Tendermint.SDK.BaseApp.Errors
  ( AppError(..)
  , IsAppError(..)
  , queryAppError
  , txResultAppError
  , SDKError(..)
  , throwSDKError
  ) where

import           Control.Exception                    (Exception)
import           Control.Lens                         (Lens', lens)
import           Data.String.Conversions              (cs)
import           Data.Text                            (Text, intercalate)
import           Data.Word                            (Word32, Word64)
import qualified Network.ABCI.Types.Messages.Response as Response
import           Polysemy
import           Polysemy.Error                       (Error, throw)
import           Tendermint.SDK.Types.TxResult        (TxResult (..))

-- | This type represents a common error response for the query, checkTx,
-- | and deliver tx abci-messages.
data AppError = AppError
  { appErrorCode      :: Word32
  , appErrorCodespace :: Text
  , appErrorMessage   :: Text
  } deriving (Eq, Show)

instance Exception AppError

-- | Allows for custom application error types to be coerced into the
-- standard error resposne.
class IsAppError e where
  makeAppError :: e -> AppError

-- | This lens is used to set the 'AppError' data into the appropriate
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

-- | This lens is used to set the 'AppError' data into the appropriate
-- | response fields for the checkTx/deliverTx abci-message.
txResultAppError :: Lens' TxResult AppError
txResultAppError = lens g s
  where
    g TxResult{..} = AppError
      { appErrorCode = _txResultCode
      , appErrorCodespace = _txResultCodespace
      , appErrorMessage = _txResultLog
      }
    s txResult AppError{..} = txResult
      { _txResultCode = appErrorCode
      , _txResultCodespace  = appErrorCodespace
      , _txResultLog = appErrorMessage
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
  | SignatureRecoveryError Text
  | NonceException Word64 Word64

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
  makeAppError (SignatureRecoveryError msg) = AppError
    { appErrorCode = 6
    , appErrorCodespace = "sdk"
    , appErrorMessage = "Signature Recovery Error: " <> msg
    }

  makeAppError (NonceException expected found) = AppError
    { appErrorCode = 7
    , appErrorCodespace = "sdk"
    , appErrorMessage = "Incorrect Transaction Nonce: Expected " <> (cs . show $ toInteger expected) <>
         " but got " <> (cs . show $ toInteger found) <> "."
    }
