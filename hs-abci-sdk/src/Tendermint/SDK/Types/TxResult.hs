{-# LANGUAGE TemplateHaskell #-}
module Tendermint.SDK.Types.TxResult where

import           Control.Lens                           (Lens', lens)
import           Control.Lens.TH                        (makeLenses)
import           Data.ByteArray.Base64String            (Base64String)
import           Data.Default.Class                     (Default (..))
import           Data.Int                               (Int64)
import           Data.Text                              (Text)
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
  } deriving Show

makeLenses ''TxResult

instance Default TxResult where
  def = TxResult
    { _txResultData = ""
    , _txResultInfo = ""
    , _txResultGasWanted = 0
    , _txResultGasUsed  = 0
    , _txResultEvents   = []
    }

-- | This class is used to set the 'TxResult' data into the appropriate
-- | response fields for the CheckTx abci-message.
checkTxTxResult :: Lens' Response.CheckTx TxResult
checkTxTxResult = lens g s
  where
    g Response.CheckTx{..} = TxResult
      { _txResultData = checkTxData
      , _txResultInfo = checkTxInfo
      , _txResultGasWanted = unWrappedVal checkTxGasWanted
      , _txResultGasUsed = unWrappedVal checkTxGasUsed
      , _txResultEvents = checkTxEvents
      }
    s checkTx TxResult{..} = checkTx
      { Response.checkTxData = _txResultData
      , Response.checkTxInfo  = _txResultInfo
      , Response.checkTxGasWanted = WrappedVal _txResultGasWanted
      , Response.checkTxGasUsed = WrappedVal _txResultGasUsed
      , Response.checkTxEvents = _txResultEvents
      }

-- | This class is used to set the 'TxResult' data into the appropriate
-- | response fields for the DeliverTx abci-message.
deliverTxTxResult :: Lens' Response.DeliverTx TxResult
deliverTxTxResult = lens g s
  where
    g Response.DeliverTx{..} = TxResult
      { _txResultData = deliverTxData
      , _txResultInfo = deliverTxInfo
      , _txResultGasWanted = unWrappedVal deliverTxGasWanted
      , _txResultGasUsed = unWrappedVal deliverTxGasUsed
      , _txResultEvents = deliverTxEvents
      }
    s deliverTx TxResult{..} = deliverTx
      { Response.deliverTxData = _txResultData
      , Response.deliverTxInfo  = _txResultInfo
      , Response.deliverTxGasWanted = WrappedVal _txResultGasWanted
      , Response.deliverTxGasUsed = WrappedVal _txResultGasUsed
      , Response.deliverTxEvents = _txResultEvents
      }
