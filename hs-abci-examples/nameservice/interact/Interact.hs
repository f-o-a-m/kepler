module Interact
  ( -- actionBlock
  -- , 
    makeRandomUsers
  ) where

import           Control.Monad                    (replicateM)

import           Data.Char                        (isHexDigit)
import           Data.String                      (fromString)
import           Data.String.Conversions          (cs)
import           Data.Text                        (Text)
import qualified Faker.Lorem                      as Lorem
import qualified Faker.Name                       as Name
import qualified Faker.Utils                      as Utils
import qualified Nameservice.Modules.Nameservice  as N
import qualified Nameservice.Modules.Token        as T

import           Test.RandomStrings               (onlyWith, randomASCII,
                                                   randomString)
import           Tendermint.Utils.Client           (ClientConfig (..),
                                                    EmptyTxClient (..),
                                                    HasQueryClient (..),
                                                    HasTxClient (..),
                                                    QueryClientResponse (..),
                                                    Signer (..),
                                                    TxClientResponse (..),
                                                    TxOpts (..),
                                                    defaultClientTxOpts)
import           Tendermint.Utils.User             (makeSignerFromUser,
                                                    makeUser)
import           Tendermint.SDK.Application.Module (AppQueryRouter (QApi),
                                                    AppTxRouter (TApi))
import           Data.Proxy
import           Servant.API                       ((:<|>) (..))
import           Control.Monad.Reader              (ReaderT, runReaderT)
import qualified Tendermint.SDK.Modules.Auth       as Auth
import           Nameservice.Application
import qualified Network.Tendermint.Client         as RPC
import           Tendermint.SDK.BaseApp.Query      (QueryArgs (..),
                                                    QueryResult (..))
import           Tendermint.Utils.ClientUtils      (rpcConfig, assertTx)
import           Tendermint.SDK.BaseApp.Errors     (AppError (..))
import           Tendermint.SDK.Types.Address      (Address)
import           Data.Default.Class                (def)
import Control.Monad (void)
--------------------------------------------------------------------------------
-- Actions
--------------------------------------------------------------------------------

faucetAccount :: Signer -> T.Amount -> IO ()
faucetAccount s@(Signer addr _) amount =
  void . assertTx . runTxClientM $
    let msg = T.FaucetAccount addr amount
        opts = TxOpts
          { txOptsGas = 0
          , txOptsSigner = s
          }
    in faucet opts msg

-- createName :: User -> Name -> Text -> IO ()
-- createName user name val = buyName user name val 0

buyName :: Signer -> N.Name -> Text -> T.Amount -> IO ()
buyName s@(Signer addr _) name newVal amount =
  void . assertTx . runTxClientM $
    let msg = N.BuyName amount name newVal addr
        opts = TxOpts
          { txOptsGas = 0
          , txOptsSigner = s
          }
    in buy opts msg
--   runAction_ user "nameservice" "BuyName" (BuyName amount name newVal userAddress)

-- deleteName :: User -> Name -> IO ()
-- deleteName user@User{userAddress} name =
--   runAction_ user "nameservice" "DeleteName" (DeleteName userAddress name)

-- setName :: User -> Name -> Text -> IO ()
-- setName user@User{userAddress} name val =
--   runAction_ user "nameservice" "SetName" (SetName name userAddress val)

-- runAction_
--   :: HasCodec a
--   => User
--   -> ByteString
--   -> Text
--   -> a
--   -> IO ()
-- runAction_ user bs t msg = runTransaction_ =<<
--   mkSignedRawTransactionWithRoute bs user (TypedMessage t (encode msg))

-- actionBlock :: (User, User) -> IO ()
-- actionBlock (user1, user2) = do
--   name <- genName
--   genCVal <- genWords
--   genBVal <- genWords
--   genBAmt <- genAmount
--   genSVal <- genWords
--   faucetAccount user2 genBAmt
--   createName user1 name genCVal
--   buyName user2 name genBVal genBAmt
--   setName user2 name genSVal
--   deleteName user2 name

--------------------------------------------------------------------------------
-- Users
--------------------------------------------------------------------------------

makeRandomUsers :: IO (Signer, Signer)
makeRandomUsers = do
  str1 <- randomString (onlyWith isHexDigit randomASCII) 64
  str2 <- randomString (onlyWith isHexDigit randomASCII) 64
  return $ (makeSignerFromUser . makeUser $ str1
           ,makeSignerFromUser . makeUser $ str2
           )

--------------------------------------------------------------------------------
-- Query Client
--------------------------------------------------------------------------------

getAccount
  :: QueryArgs Address
  -> RPC.TendermintM (QueryClientResponse Auth.Account)

_ :<|> _ :<|> getAccount =
  genClientQ (Proxy :: Proxy m) queryApiP def
  where
    queryApiP :: Proxy (QApi NameserviceModules)
    queryApiP = Proxy
  
 --------------------------------------------------------------------------------
-- Tx Client
--------------------------------------------------------------------------------

txClientConfig :: ClientConfig
txClientConfig =
  let getNonce addr = do
        resp <- RPC.runTendermintM rpcConfig $ getAccount $
          QueryArgs
            { queryArgsHeight = -1
            , queryArgsProve = False
            , queryArgsData = addr
            }
        case resp of
          QueryError e ->
            if appErrorCode e == 2
              then pure 0
              else error $ "Unknown nonce error: " <> show (appErrorMessage e)
          QueryResponse QueryResult {queryResultData} ->
            pure $ Auth.accountNonce queryResultData

  in ClientConfig
       { clientGetNonce = getNonce
       , clientRPC = rpcConfig
       }

type TxClientM = ReaderT ClientConfig IO

runTxClientM :: TxClientM a -> IO a
runTxClientM m = runReaderT m txClientConfig

-- Nameservice Client
buy
  :: TxOpts
  -> N.BuyName
  -> TxClientM (TxClientResponse () ())

set
  :: TxOpts
  -> N.SetName
  -> TxClientM (TxClientResponse () ())

delete
  :: TxOpts
  -> N.DeleteName
  -> TxClientM (TxClientResponse () ())

-- Token Client
faucet
  :: TxOpts
  -> T.FaucetAccount
  -> TxClientM (TxClientResponse () ())

(buy :<|> set :<|> delete) :<|>
  (_ :<|> _ :<|> faucet) :<|>
  EmptyTxClient =
    genClientT (Proxy @TxClientM) txApiP defaultClientTxOpts
    where
      txApiP :: Proxy (TApi NameserviceModules)
      txApiP = Proxy


--------------------------------------------------------------------------------
-- Generation
--------------------------------------------------------------------------------

genWords :: IO Text
genWords = do
  numWords <- Utils.randomNum (1, 10)
  ws <- replicateM numWords Lorem.word
  return . cs . unwords $ ws

genName :: IO N.Name
genName = do
  name <- Name.name
  return . fromString $ name

genAmount :: IO T.Amount
genAmount = do
  genAmt <- Utils.randomNum (1, 1000)
  return . fromInteger . toInteger $ genAmt
