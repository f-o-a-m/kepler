module Interact
  ( actionBlock
  ) where

import           Control.Monad                    (replicateM)
import           Data.ByteString                  (ByteString)
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

faucetAccount :: User -> Amount -> IO ()
faucetAccount user@User{userAddress} amount =
  runAction_ user "token" "FaucetAccount" (FaucetAccount userAddress amount)

createName :: User -> Name -> Text -> IO ()
createName user name val = buyName user name val 0

buyName :: User -> Name -> Text -> Amount -> IO ()
buyName user@User{userAddress} name newVal amount =
  runAction_ user "nameservice" "BuyName" (BuyName amount name newVal userAddress)

deleteName :: User -> Name -> IO ()
deleteName user@User{userAddress} name =
  runAction_ user "nameservice" "DeleteName" (DeleteName userAddress name)

setName :: User -> Name -> Text -> IO ()
setName user@User{userAddress} name val =
  runAction_ user "nameservice" "SetName" (SetName name userAddress val)

runAction_
  :: HasCodec a
  => User
  -> ByteString
  -> Text
  -> a
  -> IO ()
runAction_ user bs t msg = runTransaction_ =<<
  mkSignedRawTransactionWithRoute bs user (TypedMessage t (encode msg))

actionBlock :: IO ()
actionBlock = do
  name <- genName
  genCVal <- genWords
  genBVal <- genWords
  genBAmt <- genAmount
  genSVal <- genWords
  faucetAccount user2 genBAmt
  createName user1 name genCVal
  buyName user2 name genBVal genBAmt
  setName user2 name genSVal
  deleteName user2 name

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

genWords :: IO Text
genWords = do
  numWords <- Utils.randomNum (1, 10)
  ws <- replicateM numWords Lorem.word
  return . cs . unwords $ ws

genName :: IO Name
genName = do
  name <- Name.name
  return . fromString $ name

genAmount :: IO Amount
genAmount = do
  genAmt <- Utils.randomNum (1, 100)
  return . fromInteger . toInteger $ genAmt
