module Nameservice.Test.E2ESpec (spec) where

import           Control.Monad                     (forM_, void)
import           Control.Monad.Reader              (ReaderT, runReaderT)
import           Data.Default.Class                (def)
import           Data.Proxy
import           Nameservice.Application
import qualified Nameservice.Modules.Nameservice   as N
import           Nameservice.Test.EventOrphans     ()
import qualified Network.Tendermint.Client         as RPC
import           Servant.API                       ((:<|>) (..))
import           Tendermint.SDK.Application.Module (AppQueryRouter (QApi),
                                                    AppTxRouter (TApi))
import           Tendermint.SDK.BaseApp.Errors     (AppError (..))
import           Tendermint.SDK.BaseApp.Query      (QueryArgs (..),
                                                    QueryResult (..),
                                                    defaultQueryArgs)
import qualified Tendermint.SDK.Modules.Auth       as Auth
import qualified Tendermint.SDK.Modules.Bank       as B
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
                                                    ensureQueryResponseCode,
                                                    ensureResponseCodes,
                                                    rpcConfig)
import           Tendermint.Utils.User             (makeSignerFromUser,
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
        void . assertQuery . RPC.runTendermintM rpcConfig $
          getBalance (signerAddress user1) "nameservice"

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
        filter (claimedLog ==) es `shouldBe` [claimedLog]

      it "Can query for a name" $ do
        let expected = N.Whois
              { whoisValue = "hello world"
              , whoisOwner = signerAddress user1
              , whoisPrice = 0
              }
        foundWhois <- fmap queryResultData . assertQuery . RPC.runTendermintM rpcConfig $
          getWhois defaultQueryArgs { queryArgsData = satoshi }
        foundWhois `shouldBe` expected

      it "Can query for a name that doesn't exist" $ do
        let nope = N.Name "nope"
        resp <- RPC.runTendermintM rpcConfig $
          getWhois defaultQueryArgs { queryArgsData = nope }
        ensureQueryResponseCode 2 resp

      it "Can set a name value" $ do
        let oldVal = "hello world"
            newVal = "goodbye to a world"
            msg = N.SetName
              { setNameName = satoshi
              , setNameOwner = signerAddress user1
              , setNameValue = newVal
              }
            remappedLog = N.NameRemapped
              { nameRemappedName = satoshi
              , nameRemappedOldValue = oldVal
              , nameRemappedNewValue = newVal
              }
            opts = TxOpts
              { txOptsSigner = user1
              , txOptsGas = 0
              }
        resp <- assertTx . runTxClientM $ setName opts msg
        ensureResponseCodes (0,0) resp
        (errs, es) <- deliverTxEvents (Proxy @N.NameRemapped) resp
        errs `shouldBe` []
        filter (remappedLog ==) es `shouldBe` [remappedLog]

        let expected = N.Whois
              { whoisValue = "goodbye to a world"
              , whoisOwner = signerAddress user1
              , whoisPrice = 0
              }
        foundWhois <- fmap queryResultData . assertQuery . RPC.runTendermintM rpcConfig $
          getWhois defaultQueryArgs { queryArgsData = satoshi }
        foundWhois `shouldBe` expected

      it "Can fail to set a name" $ do
        -- try to set a name without being the owner
        let msg = N.SetName
              { setNameName = satoshi
              , setNameOwner = signerAddress user2
              , setNameValue = "goodbye to a world"
              }
            opts = TxOpts
              { txOptsSigner = user2
              , txOptsGas = 0
              }
        resp <- assertTx . runTxClientM $ setName opts msg
        ensureResponseCodes (0,2) resp

      it "Can buy an existing name" $ do
        balance1 <- getUserBalance user1
        balance2 <- getUserBalance user2
        N.Whois{whoisPrice} <- fmap queryResultData . assertQuery . RPC.runTendermintM rpcConfig $
          getWhois defaultQueryArgs { queryArgsData = satoshi }
        let purchaseAmount = whoisPrice + 1
            newVal = "hello (again) world"
            msg = N.BuyName
              { buyNameBid = purchaseAmount
              , buyNameName = satoshi
              , buyNameValue = newVal
              , buyNameBuyer = signerAddress user2
              }
            claimedLog = N.NameClaimed
              { nameClaimedOwner = signerAddress user2
              , nameClaimedName = satoshi
              , nameClaimedValue = newVal
              , nameClaimedBid = purchaseAmount
              }
            opts = TxOpts
              { txOptsSigner = user2
              , txOptsGas = 0
              }

        resp <- assertTx . runTxClientM $ buyName opts msg
        ensureResponseCodes (0,0) resp
        (errs, es) <- deliverTxEvents (Proxy @N.NameClaimed) resp
        errs `shouldBe` []
        filter (claimedLog ==) es `shouldBe` [claimedLog]

        -- check for updated balances - seller: addr1, buyer: addr2
        sellerFoundAmount <- getUserBalance user1
        sellerFoundAmount `shouldBe` (balance1 + purchaseAmount)
        buyerFoundAmount <- getUserBalance user2
        buyerFoundAmount `shouldBe` (balance2 - purchaseAmount)

        let expected = N.Whois
              { whoisValue = "hello (again) world"
              , whoisOwner = signerAddress user2
              , whoisPrice = purchaseAmount
              }
        foundWhois <- fmap queryResultData . assertQuery . RPC.runTendermintM rpcConfig $
          getWhois defaultQueryArgs { queryArgsData = satoshi }
        foundWhois `shouldBe` expected

      -- @NOTE: this is possibly a problem with the go application too
      -- https://cosmos.network/docs/tutorial/buy-name.html#msg
      it "Can buy self-owned names and make a profit " $ do
        -- check balance before
        beforeBuyAmount <- getUserBalance user2
        -- buy
        let val = "hello (again) world"
            msg = N.BuyName
              { buyNameBid = 500
              , buyNameName = satoshi
              , buyNameValue = val
              , buyNameBuyer = signerAddress user2
              }
            claimedLog = N.NameClaimed
              { nameClaimedOwner = signerAddress user2
              , nameClaimedName = satoshi
              , nameClaimedValue = val
              , nameClaimedBid = 500
              }
            opts = TxOpts
              { txOptsSigner = user2
              , txOptsGas = 0
              }

        resp <- assertTx . runTxClientM $ buyName opts msg
        ensureResponseCodes (0,0) resp
        (errs, es) <- deliverTxEvents (Proxy @N.NameClaimed) resp
        errs `shouldBe` []
        filter (claimedLog ==) es `shouldBe` [claimedLog]

        -- check balance after
        afterBuyAmount <- getUserBalance user2
        -- owner/buyer still profits
        afterBuyAmount `shouldSatisfy` (> beforeBuyAmount)

      it "Can fail to buy a name" $ do
        -- try to buy at a lower price
        let msg = N.BuyName
              { buyNameBid = 100
              , buyNameName = satoshi
              , buyNameValue = "hello (again) world"
              , buyNameBuyer = signerAddress user1
              }
            opts = TxOpts
              { txOptsSigner = user1
              , txOptsGas = 0
              }

        resp <- assertTx . runTxClientM $ buyName opts msg
        ensureResponseCodes (0,1) resp

      it "Can delete names" $ do
        let msg = N.DeleteName
              { deleteNameOwner = signerAddress user2
              , deleteNameName = satoshi
              }
            deletedLog = N.NameDeleted
              { nameDeletedName = satoshi
              }

            opts = TxOpts
              { txOptsSigner = user2
              , txOptsGas = 0
              }

        resp <- assertTx . runTxClientM $ deleteName opts msg
        ensureResponseCodes (0,0) resp
        (errs, es) <- deliverTxEvents (Proxy @N.NameDeleted) resp
        errs `shouldBe` []
        filter (deletedLog ==) es `shouldBe` [deletedLog]

        respQ <- RPC.runTendermintM rpcConfig $
          getWhois defaultQueryArgs { queryArgsData = satoshi }
        ensureQueryResponseCode 2 respQ


      it "Can fail a transfer" $ do
        addr2Balance <- getUserBalance user2
        let tooMuchToTransfer = addr2Balance + 1
            msg = B.Transfer
              { transferFrom = signerAddress user2
              , transferTo = signerAddress user1
              , transferCoinId = "nameservice"
              , transferAmount = tooMuchToTransfer
              }
            opts = TxOpts
              { txOptsSigner = user2
              , txOptsGas = 0
              }

        resp <- assertTx . runTxClientM $ transfer opts msg
        ensureResponseCodes (0,1) resp

      it "Can transfer" $ do
        balance1 <- getUserBalance user1
        balance2 <- getUserBalance user2
        let transferAmount = 1
            msg =
              B.Transfer
                { transferFrom = signerAddress user1
                , transferTo = signerAddress user2
                , transferCoinId = "nameservice"
                , transferAmount = transferAmount
                }
            transferLog = B.TransferEvent
              { transferEventAmount = transferAmount
              , transferEventCoinId = "nameservice"
              , transferEventTo = signerAddress user2
              , transferEventFrom = signerAddress user1
              }
            opts = TxOpts
              { txOptsSigner = user1
              , txOptsGas = 0
              }

        resp <- assertTx . runTxClientM $ transfer opts msg
        ensureResponseCodes (0,0) resp
        (errs, es) <- deliverTxEvents (Proxy @B.TransferEvent) resp
        errs `shouldBe` []
        filter (transferLog ==) es `shouldBe` [transferLog]

        -- check balances
        balance1' <- getUserBalance user1
        balance1' `shouldBe` balance1 - transferAmount
        balance2' <- getUserBalance user2
        balance2' `shouldBe` balance2 + transferAmount

faucetUser
  :: Auth.Amount
  -> Signer
  -> IO ()
faucetUser amount s@(Signer addr _) =
  void . assertTx .runTxClientM $
    let msg = N.FaucetAccount addr "nameservice" amount
        opts = TxOpts
          { txOptsGas = 0
          , txOptsSigner = s
          }
    in faucet opts msg

getUserBalance
  :: Signer
  -> IO Auth.Amount
getUserBalance usr = fmap (Auth.coinAmount . queryResultData) . assertQuery . RPC.runTendermintM rpcConfig $
  getBalance (signerAddress usr) "nameservice"

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
  :: Address
  -> Auth.CoinId
  -> RPC.TendermintM (QueryClientResponse Auth.Coin)

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

-- Bank Client
transfer
  :: TxOpts
  -> B.Transfer
  -> TxClientM (TxClientResponse () ())

faucet
  :: TxOpts
  -> N.FaucetAccount
  -> TxClientM (TxClientResponse () ())

(buyName :<|> setName :<|> deleteName :<|> faucet) :<|>
  (_ :<|> transfer) :<|>
  EmptyTxClient =
    genClientT (Proxy @TxClientM) txApiP defaultClientTxOpts
    where
      txApiP :: Proxy (TApi NameserviceModules)
      txApiP = Proxy
