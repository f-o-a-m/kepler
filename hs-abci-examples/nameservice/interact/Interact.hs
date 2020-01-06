module Interact
  ( genNames
  , genVals
  , createNames
  , buyNames
  , setNames
  , deleteNames
  , faucetAccount
  , user1
  , user2
  ) where

import           Data.String                      (fromString)
import           Data.String.Conversions          (cs)
import           Data.Text                        (Text)
import qualified Faker.Lorem                      as Lorem
import qualified Faker.Name                       as Name
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

genNames :: Int -> IO [Name]
genNames 0 = return []
genNames x = do
  genName <- Name.name
  let aName = fromString genName
  names <- genNames (x - 1)
  return (aName:names)

genVals :: Int -> IO [Text]
genVals 0 = return []
genVals x = do
  genVal <- Lorem.word
  vals <- genVals (x - 1)
  return (cs genVal:vals)

createName :: User -> Name -> Text -> IO ()
createName user name val = buyName user name val 0

createNames :: User -> [(Name, Text)] -> IO ()
createNames _ [] = return ()
createNames user ((name, val):rst) = do
  createName user name val
  createNames user rst

buyName :: User -> Name -> Text -> Amount -> IO ()
buyName User{userAddress, userPrivKey} name newVal amount =
  let msg = TypedMessage "BuyName" (encode $ BuyName amount name newVal userAddress)
      rawTx = mkSignedRawTransactionWithRoute "nameservice" userPrivKey msg
  in runTransaction_ rawTx

buyNames :: User -> [(Name, Text, Amount)] -> IO ()
buyNames _ [] = return ()
buyNames user ((name, val, amt):rst) = do
  buyName user name val amt
  buyNames user rst

deleteName :: User -> Name -> IO ()
deleteName User{userAddress, userPrivKey} name =
  let msg = TypedMessage "DeleteName" (encode $ DeleteName userAddress name)
      rawTx = mkSignedRawTransactionWithRoute "nameservice" userPrivKey msg
  in runTransaction_ rawTx

deleteNames :: User -> [Name] -> IO ()
deleteNames _ [] = return ()
deleteNames user (name:names) = do
  deleteName user name
  deleteNames user names

setName :: User -> Name -> Text -> IO ()
setName User{userAddress, userPrivKey} name val =
  let msg = TypedMessage "SetName" (encode $ SetName name userAddress val)
      rawTx = mkSignedRawTransactionWithRoute "nameservice" userPrivKey msg
  in runTransaction_ rawTx

setNames :: User -> [(Name, Text)] -> IO ()
setNames _ [] = return ()
setNames user ((name, val):rst) = do
  setName user name val
  setNames user rst

faucetAccount :: User -> Amount -> IO ()
faucetAccount User{userAddress, userPrivKey} amount =
  let msg = TypedMessage "FaucetAccount" (encode $ FaucetAccount userAddress amount)
      rawTx = mkSignedRawTransactionWithRoute "token" userPrivKey msg
  in runTransaction_ rawTx
