module Tendermint.SDK.Errors
  ( AppError(..)
  , IsAppError(..)
  , HasAppError(..)
  , SDKError(..)
  , throwSDKError
  ) where

import           Control.Exception                    (Exception)
import           Control.Lens                         (Lens', lens)
import           Data.Text                            (Text)
import           Data.Word                            (Word32)
import qualified Network.ABCI.Types.Messages.Response as Response
import           Polysemy
import           Polysemy.Error                       (Error, throw)

data AppError = AppError
  { appErrorCode      :: Word32
  , appErrorCodespace :: Text
  , appErrorMessage   :: Text
  } deriving Show

instance Exception AppError

class IsAppError e where
  makeAppError :: e -> AppError

class HasAppError resp where
  appError :: Lens' resp AppError

-- NOTE: We're not actually using this right now as Query messages
-- are handled by a special router.
instance HasAppError Response.Query where
  appError = lens g s
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

instance HasAppError Response.CheckTx where
  appError = lens g s
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

instance HasAppError Response.DeliverTx where
  appError = lens g s
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

data SDKError =
    InternalError
  | ParseError Text


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

