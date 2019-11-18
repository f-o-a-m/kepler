module Nameservice.Modules.Nameservice.Router where

import           Data.ByteString                          (ByteString)
import           Data.Proxy
import           Nameservice.Modules.Nameservice.Keeper   (HasNameserviceEff,
                                                           buyName, deleteName,
                                                           setName)
import           Nameservice.Modules.Nameservice.Messages (NameserviceMessage (..))
import           Nameservice.Modules.Token                (HasTokenEff)
import           Polysemy                                 (Members, Sem)
import           Polysemy.Error                           (Error)
import           Tendermint.SDK.Auth                      (AuthError)
import           Tendermint.SDK.Crypto                    (Secp256k1)
import           Tendermint.SDK.Errors                    (AppError,
                                                           SDKError (..),
                                                           throwSDKError)
import qualified Tendermint.SDK.TxRouter                  as R
import           Tendermint.SDK.Types.Message             (DecodingOption (..),
                                                           Msg (..),
                                                           ParseMessage (..),
                                                           formatMessageParseError)
import           Tendermint.SDK.Types.Transaction         (Tx (..))

router
  :: Members [Error AppError, Error AuthError] r
  => HasTokenEff r
  => HasNameserviceEff r
  => Tx Secp256k1 ByteString
  -> Sem r ()
router tx@Tx{txMsg} =
  case decodeMessage (Proxy @'Proto3Suite) $ msgData txMsg of
    Left parseErrMsg -> throwSDKError . ParseError . formatMessageParseError $ parseErrMsg
    Right msg -> handler $ tx {txMsg = txMsg {msgData = msg}}
  where
    handler
      :: HasTokenEff r
      => HasNameserviceEff r
      => R.Handler r NameserviceMessage
    handler Tx{txMsg} =
      let Msg{msgData=msg} = txMsg
      in case msg of
           NSetName txMsg    -> setName txMsg
           NBuyName txMsg    -> buyName txMsg
           NDeleteName txMsg -> deleteName txMsg
