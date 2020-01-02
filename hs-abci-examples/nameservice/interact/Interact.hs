module Interact where

import           Data.Text                            (Text)
import           Nameservice.Modules.Nameservice      (BuyName (..),
                                                       DeleteName (..),
                                                       Name (..), SetName (..))
import           Nameservice.Modules.Token            (Amount (..),
                                                       FaucetAccount (..))
import           Nameservice.Modules.TypedMessage     (TypedMessage (..))
import qualified Network.ABCI.Types.Messages.Response as Response
import           Tendermint.SDK.Codec                 (HasCodec (..))
import           Tendermint.Utils.Request             (getDeliverTxResponse)
import           Tendermint.Utils.User                (User (..), makeUser, mkSignedRawTransactionWithRoute)
--------------------------------------------------------------------------------
-- Users
--------------------------------------------------------------------------------

user1 :: User
user1 = makeUser "f65255094d7773ed8dd417badc9fc045c1f80fdc5b2d25172b031ce6933e039a"

user2 :: User
user2 = makeUser "f65242094d7773ed8dd417badc9fc045c1f80fdc5b2d25172b031ce6933e039a"

--------------------------------------------------------------------------------
-- Actions
--------------------------------------------------------------------------------

createName :: User -> Text -> Text -> IO Response.DeliverTx
createName user strName val = buyName user (Name strName) val 0

buyName :: User -> Name -> Text -> Amount -> IO Response.DeliverTx
buyName User{userAddress, userPrivKey} name newVal amount =
  let msg = TypedMessage "BuyName" (encode $ BuyName amount name newVal userAddress)
      rawTx = mkSignedRawTransactionWithRoute "nameservice" userPrivKey msg
  in getDeliverTxResponse rawTx

deleteName :: User -> Name -> IO Response.DeliverTx
deleteName User{userAddress, userPrivKey} name =
  let msg = TypedMessage "DeleteName" (encode $ DeleteName userAddress name)
      rawTx = mkSignedRawTransactionWithRoute "nameservice" userPrivKey msg
  in getDeliverTxResponse rawTx

setNameValue :: User -> Name -> Text -> IO Response.DeliverTx
setNameValue User{userAddress, userPrivKey} name val =
  let msg = TypedMessage "SetName" (encode $ SetName name userAddress val)
      rawTx = mkSignedRawTransactionWithRoute "nameservice" userPrivKey msg
  in getDeliverTxResponse rawTx

faucetAccount :: User -> IO Response.DeliverTx
faucetAccount User{userAddress, userPrivKey} =
  let msg = TypedMessage "FaucetAccount" (encode $ FaucetAccount userAddress 1000)
      rawTx = mkSignedRawTransactionWithRoute "token" userPrivKey msg
  in getDeliverTxResponse rawTx
