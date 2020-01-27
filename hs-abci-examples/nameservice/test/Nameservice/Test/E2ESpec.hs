module Nameservice.Test.E2ESpec (spec) where

import           Control.Monad                     (forM_, void)
import           Control.Monad.Reader              (ReaderT, runReaderT)
import           Data.Default.Class                (def)
import           Data.Proxy
import           Nameservice.Application
import qualified Nameservice.Modules.Nameservice   as N
import qualified Nameservice.Modules.Token         as T
import           Nameservice.Test.EventOrphans     ()
import qualified Network.Tendermint.Client         as RPC
import           Servant.API                       ((:<|>) (..))
import           Tendermint.SDK.Application.Module (AppQueryRouter (QApi),
                                                    AppTxRouter (TApi))
import           Tendermint.SDK.BaseApp.Errors     (AppError (..))
import           Tendermint.SDK.BaseApp.Query      (QueryArgs (..))
import qualified Tendermint.SDK.Modules.Auth       as Auth
import           Tendermint.SDK.Types.Address      (Address)
import           Tendermint.Utils.Client           (ClientConfig (..),
                                                    EmptyTxClient (..),
                                                    HasQueryClient (..),
                                                    HasTxClient (..),
                                                    QueryClientResponse (..),
                                                    Signer (..),
                                                    TxClientResponse (..),
                                                    TxOpts (..),
                                                    defaultClientTxOpts)
import           Tendermint.Utils.ClientUtils      (assertQuery, assertTx,
                                                    deliverTxEvents,
                                                    ensureResponseCodes,
                                                    rpcConfig)
import           Tendermint.Utils.User             (User (..),
                                                    makeSignerFromUser,
                                                    makeUser)
import           Test.Hspec

spec :: Spec
spec = do
  let satoshi = N.Name "satoshi"
      faucetAmount = 1000

  beforeAll (forM_ [user1, user2] $ faucetUser faucetAmount) $ do

    describe "Nameservice Spec" $ do
      it "Can query /health to make sure the node is alive" $ do
        resp <- RPC.runTendermintM rpcConfig $ RPC.health
        resp `shouldBe` RPC.ResultHealth

      it "Can query account balances" $ do
        let queryReq = QueryArgs
              { queryArgsProve = False
              , queryArgsData = signerAddress user1
              , queryArgsHeight = -1
              }
        void . assertQuery . RPC.runTendermintM rpcConfig $
          getBalance queryReq

      it "Can create a name" $ do
        let val = "hello world"
            msg = N.BuyName
              { buyNameBid = 0
              , buyNameName = satoshi
              , buyNameValue = val
              , buyNameBuyer = signerAddress user1
              }
            claimedLog = N.NameClaimed
              { nameClaimedOwner = signerAddress user1
              , nameClaimedName = satoshi
              , nameClaimedValue = val
              , nameClaimedBid = 0
              }
            opts = TxOpts
              { txOptsSigner = user1
              , txOptsGas = 0
              }
        resp <- assertTx . runTxClientM $ buyName opts msg
        ensureResponseCodes (0,0) resp
        (errs, es) <- deliverTxEvents (Proxy @N.NameClaimed) resp
        errs `shouldBe` []
        filter ((==) claimedLog) es `shouldBe` [claimedLog]

      it "Can query for a name" $ do
        let queryReq = QueryArgs
              { queryArgsProve = False
              , queryArgsHeight = -1
              , queryArgsData = satoshi
              }
            expected = N.Whois
              { whoisValue = "hello world"
              , whoisOwner = signerAddress user1
              , whoisPrice = 0
              }
        foundWhois <- fmap fst . assertQuery . RPC.runTendermintM rpcConfig $
          getWhois queryReq
        foundWhois `shouldBe` expected

  --    it "Can query for a name that doesn't exist" $ do
  --      let nope = Name "nope"
  --          queryReq = defaultQueryWithData nope
  --      ClientResponse{ clientResponseData, clientResponseRaw } <- runRPC $ getWhois queryReq
  --      let queryRespCode = clientResponseRaw ^. Response._queryCode
  --      -- storage failure
  --      queryRespCode `shouldBe` 2
  --      clientResponseData `shouldBe` Nothing

  --    it "Can set a name value (success 0)" $ do
  --      let oldVal = "hello world"
  --          newVal = "goodbye to a world"
  --          msg = SetName satoshi addr1 newVal
  --          remappedLog = NameRemapped satoshi oldVal newVal
  --      deliverResp <- mkSignedRawTransactionWithRoute "nameservice" user1 msg >>= getDeliverTxResponse
  --      ensureDeliverResponseCode deliverResp 0
  --      ensureEventLogged deliverResp "NameRemapped" remappedLog
  --      -- check for changes
  --      let queryReq = defaultQueryWithData satoshi
  --      foundWhois <- getQueryResponseSuccess $ getWhois queryReq
  --      foundWhois `shouldBe` Whois "goodbye to a world" addr1 0

  --    it "Can fail to set a name (failure 2)" $ do
  --      -- try to set a name without being the owner
  --      let msg =  SetName satoshi addr2 "goodbye to a world"
  --      ensureCheckAndDeliverResponseCodes (0,2) =<< mkSignedRawTransactionWithRoute "nameservice" user2 msg

  --    it "Can buy an existing name (success 0)" $ do
  --      balance1 <- getQueryResponseSuccess $ getBalance $ defaultQueryWithData addr1
  --      balance2 <- getQueryResponseSuccess $ getBalance $ defaultQueryWithData addr2
  --      Whois{whoisPrice} <- getQueryResponseSuccess $ getWhois $ defaultQueryWithData satoshi
  --      let purchaseAmount = whoisPrice + 1
  --          newVal = "hello (again) world"
  --          msg = BuyName purchaseAmount satoshi newVal addr2
  --          claimedLog = NameClaimed addr2 satoshi newVal purchaseAmount
  --      deliverResp <- mkSignedRawTransactionWithRoute "nameservice" user2 msg >>= getDeliverTxResponse
  --      ensureDeliverResponseCode deliverResp 0
  --      ensureEventLogged deliverResp "NameClaimed" claimedLog
  --      -- check for updated balances - seller: addr1, buyer: addr2
  --      let sellerQueryReq = defaultQueryWithData addr1
  --      sellerFoundAmount <- getQueryResponseSuccess $ getBalance sellerQueryReq
  --      sellerFoundAmount `shouldBe` (balance1 + purchaseAmount)
  --      let buyerQueryReq = defaultQueryWithData addr2
  --      buyerFoundAmount <- getQueryResponseSuccess $ getBalance buyerQueryReq
  --      buyerFoundAmount `shouldBe` (balance2 - purchaseAmount)
  --      -- check for ownership changes
  --      let queryReq = defaultQueryWithData satoshi
  --      foundWhois <- getQueryResponseSuccess $ getWhois queryReq
  --      foundWhois `shouldBe` Whois "hello (again) world" addr2 purchaseAmount

  --    -- @NOTE: this is possibly a problem with the go application too
  --    -- https://cosmos.network/docs/tutorial/buy-name.html#msg
  --    it "Can buy self-owned names and make a profit (success 0)" $ do
  --      -- check balance before
  --      let queryReq = defaultQueryWithData addr2
  --      beforeBuyAmount <- getQueryResponseSuccess $ getBalance queryReq
  --      -- buy
  --      let val = "hello (again) world"
  --          msg = BuyName 500 satoshi val addr2
  --          claimedLog = NameClaimed addr2 satoshi val 500
  --      deliverResp <- mkSignedRawTransactionWithRoute "nameservice" user2 msg >>= getDeliverTxResponse
  --      ensureDeliverResponseCode deliverResp 0
  --      ensureEventLogged deliverResp "NameClaimed" claimedLog
  --      -- check balance after
  --      afterBuyAmount <- getQueryResponseSuccess $ getBalance queryReq
  --      -- owner/buyer still profits
  --      afterBuyAmount `shouldSatisfy` (> beforeBuyAmount)

  --    it "Can fail to buy a name (failure 1)" $ do
  --      -- try to buy at a lower price
  --      let msg = BuyName 100 satoshi "hello (again) world" addr1
  --      mkSignedRawTransactionWithRoute "nameservice" user1 msg >>= ensureCheckAndDeliverResponseCodes (0,1)

  --    it "Can delete names (success 0)" $ do
  --      let msg = DeleteName addr2 satoshi
  --          deletedLog = NameDeleted satoshi
  --      deliverResp <- mkSignedRawTransactionWithRoute "nameservice" user2 msg >>= getDeliverTxResponse
  --      ensureDeliverResponseCode deliverResp 0
  --      ensureEventLogged deliverResp "NameDeleted" deletedLog
  --      -- name shouldn't exist
  --      let queryReq = defaultQueryWithData satoshi
  --      ClientResponse{ clientResponseData, clientResponseRaw } <- runRPC $ getWhois queryReq
  --      let queryRespCode = clientResponseRaw ^. Response._queryCode
  --      -- storage failure
  --      queryRespCode `shouldBe` 2
  --      clientResponseData `shouldBe` Nothing

  --    it "Can fail a transfer (failure 1)" $ do
  --      let senderBeforeQueryReq = defaultQueryWithData addr2
  --      addr2Balance <- getQueryResponseSuccess $ getBalance senderBeforeQueryReq
  --      let tooMuchToTransfer = addr2Balance + 1
  --          msg = Transfer addr2 addr1 tooMuchToTransfer
  --      ensureCheckAndDeliverResponseCodes (0,1) =<< mkSignedRawTransactionWithRoute "token" user2 msg

  --    it "Can transfer (success 0)" $ do
  --      balance1 <- getQueryResponseSuccess $ getBalance $ defaultQueryWithData addr1
  --      balance2 <- getQueryResponseSuccess $ getBalance $ defaultQueryWithData addr2
  --      let transferAmount = 1
  --          msg =
  --            Transfer
  --              { transferFrom = addr1
  --              , transferTo = addr2
  --              , transferAmount = transferAmount
  --              }
  --          transferEvent = TransferEvent
  --            { transferEventAmount = transferAmount
  --            , transferEventTo = addr2
  --            , transferEventFrom = addr1
  --            }
  --      deliverResp <- mkSignedRawTransactionWithRoute "token" user1 msg >>= getDeliverTxResponse
  --      ensureDeliverResponseCode deliverResp 0
  --      ensureEventLogged deliverResp "TransferEvent" transferEvent
  --      -- check balances
  --      balance1' <- getQueryResponseSuccess $ getBalance $ defaultQueryWithData addr1
  --      balance1' `shouldBe` balance1 - transferAmount
  --      balance2' <- getQueryResponseSuccess $ getBalance $ defaultQueryWithData addr2
  --      balance2' `shouldBe` balance2 + transferAmount

faucetUser
  :: T.Amount
  -> Signer
  -> IO ()
faucetUser amount s@(Signer addr _) =
  void . assertTx .runTxClientM $
    let msg = T.FaucetAccount addr amount
        opts = TxOpts
          { txOptsGas = 0
          , txOptsSigner = s
          }
    in faucet opts msg

--------------------------------------------------------------------------------

user1 :: Signer
user1 = makeSignerFromUser $
  makeUser "f65255094d7773ed8dd417badc9fc045c1f80fdc5b2d25172b031ce6933e039a"

user2 :: Signer
user2 = makeSignerFromUser $
  makeUser "f65242094d7773ed8dd417badc9fc045c1f80fdc5b2d25172b031ce6933e039a"

--------------------------------------------------------------------------------
-- Query Client
--------------------------------------------------------------------------------

getAccount
  :: QueryArgs Address
  -> RPC.TendermintM (QueryClientResponse Auth.Account)

getWhois
  :: QueryArgs N.Name
  -> RPC.TendermintM (QueryClientResponse N.Whois)

getBalance
  :: QueryArgs Address
  -> RPC.TendermintM (QueryClientResponse T.Amount)

getWhois :<|> getBalance :<|> getAccount =
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
          QueryResponse {queryClientResponseData} ->
            pure $ Auth.accountNonce queryClientResponseData

  in ClientConfig
       { clientGetNonce = getNonce
       , clientRPC = rpcConfig
       }

type TxClientM = ReaderT ClientConfig IO

runTxClientM :: TxClientM a -> IO a
runTxClientM m = runReaderT m txClientConfig


-- Nameservice Client
buyName
  :: TxOpts
  -> N.BuyName
  -> TxClientM (TxClientResponse () ())

setName
  :: TxOpts
  -> N.SetName
  -> TxClientM (TxClientResponse () ())

deleteName
  :: TxOpts
  -> N.DeleteName
  -> TxClientM (TxClientResponse () ())

-- Token Client
burn
  :: TxOpts
  -> T.Burn
  -> TxClientM (TxClientResponse () ())

transfer
  :: TxOpts
  -> T.Transfer
  -> TxClientM (TxClientResponse () ())

faucet
  :: TxOpts
  -> T.FaucetAccount
  -> TxClientM (TxClientResponse () ())

(buyName :<|> setName :<|> deleteName) :<|>
  (burn :<|> transfer :<|> faucet) :<|>
  EmptyTxClient =
    genClientT (Proxy @TxClientM) txApiP defaultClientTxOpts
    where
      txApiP :: Proxy (TApi NameserviceModules)
      txApiP = Proxy
