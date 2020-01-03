module Interact where

import           Data.Text                        (Text)
import           Nameservice.Modules.Nameservice  (BuyName (..),
                                                   DeleteName (..), Name (..),
                                                   SetName (..))
import           Nameservice.Modules.Token        (Amount (..),
                                                   FaucetAccount (..))
import           Nameservice.Modules.TypedMessage (TypedMessage (..))
import           Tendermint.SDK.Codec             (HasCodec (..))
import           Tendermint.Utils.Request         (runTransaction_)
import           Tendermint.Utils.User            (User (..), makeUser, mkSignedRawTransactionWithRoute)
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

createName :: User -> Name -> Text -> IO ()
createName user name val = buyName user name val 0

buyName :: User -> Name -> Text -> Amount -> IO ()
buyName User{userAddress, userPrivKey} name newVal amount =
  let msg = TypedMessage "BuyName" (encode $ BuyName amount name newVal userAddress)
      rawTx = mkSignedRawTransactionWithRoute "nameservice" userPrivKey msg
  in runTransaction_ rawTx

deleteName :: User -> Name -> IO ()
deleteName User{userAddress, userPrivKey} name =
  let msg = TypedMessage "DeleteName" (encode $ DeleteName userAddress name)
      rawTx = mkSignedRawTransactionWithRoute "nameservice" userPrivKey msg
  in runTransaction_ rawTx

setName :: User -> Name -> Text -> IO ()
setName User{userAddress, userPrivKey} name val =
  let msg = TypedMessage "SetName" (encode $ SetName name userAddress val)
      rawTx = mkSignedRawTransactionWithRoute "nameservice" userPrivKey msg
  in runTransaction_ rawTx

faucetAccount :: User -> Amount -> IO ()
faucetAccount User{userAddress, userPrivKey} amount =
  let msg = TypedMessage "FaucetAccount" (encode $ FaucetAccount userAddress amount)
      rawTx = mkSignedRawTransactionWithRoute "token" userPrivKey msg
  in runTransaction_ rawTx
