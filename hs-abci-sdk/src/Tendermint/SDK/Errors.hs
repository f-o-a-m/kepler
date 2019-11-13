module Tendermint.SDK.Errors
  ( AppError(..)
  , IsAppError(..)
  , HasAppError(..)
  , catchAppError
  ) where

import           Control.Lens                         (Lens', lens, (&), (.~))
import           Data.Default.Class                   (Default (..))
import           Data.Text                            (Text)
import           Data.Word                            (Word32)
import qualified Network.ABCI.Types.Messages.Response as Response
import           Polysemy
import           Polysemy.Error                       (Error, runError)

data AppError = AppError
  { appErrorCode      :: Word32
  , appErrorCodespace :: Text
  , appErrorMessage   :: Text
  }

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

catchAppError
  :: HasAppError resp
  => Default resp
  => Sem (Error AppError ': r) resp
  -> Sem r resp
catchAppError action = do
  eRes <- runError action
  case eRes of
    Right res -> pure res
    Left err  -> pure $ def & appError .~ err
