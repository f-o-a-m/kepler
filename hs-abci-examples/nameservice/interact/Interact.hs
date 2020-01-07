module Interact
  ( user1
  , user2
  , faucetAccount
  , actionBlock
  ) where

import           Control.Monad                    (replicateM)
import           Data.String                      (fromString)
import           Data.String.Conversions          (cs)
import           Data.Text                        (Text)
import qualified Faker.Lorem                      as Lorem
import qualified Faker.Name                       as Name
import qualified Faker.Utils                      as Utils
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

actionBlock :: IO ()
actionBlock = do
  name <- genName
  genCVal <- genWords
  genBVal <- genWords
  genBAmt <- genAmount
  genSVal <- genWords
  createName user1 name genCVal
  buyName user2 name genBVal genBAmt
  setName user2 name genSVal
  deleteName user2 name

faucetAccount :: User -> Amount -> IO ()
faucetAccount User{userAddress, userPrivKey} amount =
  let msg = TypedMessage "FaucetAccount" (encode $ FaucetAccount userAddress amount)
      rawTx = mkSignedRawTransactionWithRoute "token" userPrivKey msg
  in runTransaction_ rawTx

--------------------------------------------------------------------------------
-- Users
--------------------------------------------------------------------------------

user1 :: User
user1 = makeUser "f65255094d7773ed8dd417badc9fc045c1f80fdc5b2d25172b031ce6933e039a"

user2 :: User
user2 = makeUser "f65242094d7773ed8dd417badc9fc045c1f80fdc5b2d25172b031ce6933e039a"

--------------------------------------------------------------------------------
-- Generation
--------------------------------------------------------------------------------

-- should be as unique as possible to avoid Tx clashing

genWords :: IO Text
genWords = do
  numWords <- Utils.randomNum (1, 25)
  ws <- replicateM numWords Lorem.word
  return . cs . unwords $ ws

genName :: IO Name
genName = do
  name <- Name.name
  return . fromString $ name

genAmount :: IO Amount
genAmount = do
  genAmt <- Utils.randomNum (1, 1000)
  return . fromInteger . toInteger $ genAmt
