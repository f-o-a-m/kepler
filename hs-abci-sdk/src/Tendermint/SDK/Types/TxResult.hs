{-# LANGUAGE TemplateHaskell #-}
module Tendermint.SDK.Types.TxResult where

import           Control.Lens                           (Iso', iso)
import           Control.Lens.TH                        (makeLenses)
import           Data.ByteArray.Base64String            (Base64String)
import           Data.Default.Class                     (Default (..))
import           Data.Int                               (Int64)
import           Data.Text                              (Text)
import           Data.Word                              (Word32)
import           Network.ABCI.Types.Messages.FieldTypes (Event, WrappedVal (..))
import qualified Network.ABCI.Types.Messages.Response   as Response

-- | This type represents a common transaction result for the CheckTx
-- | and DeliverTx abci-messages.
data TxResult = TxResult
  { _txResultData      :: Base64String
  , _txResultInfo      :: Text
  , _txResultGasWanted :: Int64
  , _txResultGasUsed   :: Int64
  , _txResultEvents    :: [Event]
  , _txResultCode      :: Word32
  , _txResultLog       :: Text
  , _txResultCodespace :: Text
  } deriving Show

makeLenses ''TxResult

instance Default TxResult where
  def = TxResult
    { _txResultData = ""
    , _txResultInfo = ""
    , _txResultGasWanted = 0
    , _txResultGasUsed  = 0
    , _txResultEvents   = []
    , _txResultCode = 0
    , _txResultLog = ""
    , _txResultCodespace = ""
    }

-- | This class is used to set the 'TxResult' data into the appropriate
-- | response fields for the CheckTx abci-message.
checkTxTxResult :: Iso' Response.CheckTx TxResult
checkTxTxResult = iso g s
  where
    g Response.CheckTx{..} = TxResult
      { _txResultData = checkTxData
      , _txResultInfo = checkTxInfo
      , _txResultGasWanted = unWrappedVal checkTxGasWanted
      , _txResultGasUsed = unWrappedVal checkTxGasUsed
      , _txResultEvents = checkTxEvents
      , _txResultCode = checkTxCode
      , _txResultLog = checkTxLog
      , _txResultCodespace = checkTxCodespace
      }
    s TxResult{..} = Response.CheckTx
      { Response.checkTxData = _txResultData
      , Response.checkTxInfo  = _txResultInfo
      , Response.checkTxGasWanted = WrappedVal _txResultGasWanted
      , Response.checkTxGasUsed = WrappedVal _txResultGasUsed
      , Response.checkTxEvents = _txResultEvents
      , Response.checkTxCode = _txResultCode
      , Response.checkTxCodespace = _txResultCodespace
      , Response.checkTxLog = _txResultLog
      }

-- | This class is used to set the 'TxResult' data into the appropriate
-- | response fields for the DeliverTx abci-message.
deliverTxTxResult :: Iso' Response.DeliverTx TxResult
deliverTxTxResult = iso g s
  where
    g Response.DeliverTx{..} = TxResult
      { _txResultData = deliverTxData
      , _txResultInfo = deliverTxInfo
      , _txResultGasWanted = unWrappedVal deliverTxGasWanted
      , _txResultGasUsed = unWrappedVal deliverTxGasUsed
      , _txResultEvents = deliverTxEvents
      , _txResultCode = deliverTxCode
      , _txResultLog = deliverTxLog
      , _txResultCodespace = deliverTxCodespace
      }
    s TxResult{..} = Response.DeliverTx
      { Response.deliverTxData = _txResultData
      , Response.deliverTxInfo  = _txResultInfo
      , Response.deliverTxGasWanted = WrappedVal _txResultGasWanted
      , Response.deliverTxGasUsed = WrappedVal _txResultGasUsed
      , Response.deliverTxEvents = _txResultEvents
      , Response.deliverTxCode = _txResultCode
      , Response.deliverTxCodespace = _txResultCodespace
      , Response.deliverTxLog = _txResultLog
      }
