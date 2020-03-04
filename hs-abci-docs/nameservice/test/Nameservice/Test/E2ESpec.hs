module Nameservice.Test.E2ESpec (spec) where

import           Control.Concurrent                (forkIO)
import           Control.Concurrent.MVar           (MVar, modifyMVar_, newMVar,
                                                    readMVar)
import           Control.Lens                      ((^?))
import           Control.Monad                     (forM_, void)
import           Control.Monad.Reader              (ReaderT, runReaderT)
import qualified Data.Aeson                        as A
import           Data.Aeson.Lens                   (key)
import           Data.Default.Class                (def)
import           Data.HashSet                      (fromList)
import           Data.Proxy
import           Data.String.Conversions           (cs)
import           Data.Text                         (Text)
import           Nameservice.Application
import qualified Nameservice.Modules.Nameservice   as N
import           Nameservice.Test.EventOrphans     ()
import qualified Network.Tendermint.Client         as RPC
import           Servant.API                       ((:<|>) (..))
import qualified Tendermint.SDK.Application.Module as M
import           Tendermint.SDK.BaseApp.Errors     (AppError (..))
import           Tendermint.SDK.BaseApp.Events     (Event (..), ToEvent (..))
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
import           Tendermint.Utils.Events           (FromEvent (..))
import           Tendermint.Utils.User             (makeSignerFromUser,
                                                    makeUser)

import           Test.Hspec



spec :: Spec
spec = do
  let satoshi = "satoshi"
      faucetAmount = 1000

  beforeAll (testInit faucetAmount) $ do

    describe "Nameservice Spec" $ do
      it "Can query /health to make sure the node is alive" $ const $ do
        resp <- RPC.runTendermintM rpcConfig $ RPC.health
        resp `shouldBe` RPC.ResultHealth

      it "Can query account balances" $ const $ do
        void . assertQuery . RPC.runTendermintM rpcConfig $
          let queryArgs = defaultQueryArgs { queryArgsData = signerAddress user1 }
          in getBalance queryArgs N.nameserviceCoinId

      it "Can create a name" $ \tenv -> do
        let val = "hello world"
            msg = N.BuyNameMsg
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

        -- Add event to be monitored and later checked for
        addEventToCheck tenv claimedLog

        resp <- assertTx . runTxClientM $ buyName opts msg
        ensureResponseCodes (0,0) resp
        [evclaimedLog] <- deliverTxEvents (Proxy @N.NameClaimed) resp
        fromEvent evclaimedLog `shouldBe` Right claimedLog

      it "Can query for a name" $ const $ do
        let expected = N.Whois
              { whoisValue = "hello world"
              , whoisOwner = signerAddress user1
              , whoisPrice = 0
              }
        foundWhois <- fmap queryResultData . assertQuery . RPC.runTendermintM rpcConfig $
          getWhois defaultQueryArgs { queryArgsData = N.Name satoshi }
        foundWhois `shouldBe` expected

      it "Can query for a name that doesn't exist" $ const $ do
        let nope = "nope"
        resp <- RPC.runTendermintM rpcConfig $
          getWhois defaultQueryArgs { queryArgsData = N.Name nope }
        ensureQueryResponseCode 2 resp

      it "Can set a name value" $ \tenv -> do
        let oldVal = "hello world"
            newVal = "goodbye to a world"
            msg = N.SetNameMsg
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

        -- Add event to be monitored and later checked for
        addEventToCheck tenv remappedLog

        resp <- assertTx . runTxClientM $ setName opts msg
        ensureResponseCodes (0,0) resp
        [evremappedLog] <- deliverTxEvents (Proxy @N.NameRemapped) resp
        fromEvent evremappedLog `shouldBe` Right remappedLog

        let expected = N.Whois
              { whoisValue = "goodbye to a world"
              , whoisOwner = signerAddress user1
              , whoisPrice = 0
              }
        foundWhois <- fmap queryResultData . assertQuery . RPC.runTendermintM rpcConfig $
          getWhois defaultQueryArgs { queryArgsData = N.Name satoshi }
        foundWhois `shouldBe` expected

      it "Can fail to set a name" $ const $ do
        -- try to set a name without being the owner
        let msg = N.SetNameMsg
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

      it "Can buy an existing name" $ \tenv -> do
        balance1 <- getUserBalance user1
        balance2 <- getUserBalance user2
        N.Whois{whoisPrice} <- fmap queryResultData . assertQuery . RPC.runTendermintM rpcConfig $
          getWhois defaultQueryArgs { queryArgsData = N.Name satoshi }
        let purchaseAmount = whoisPrice + 1
            newVal = "hello (again) world"
            msg = N.BuyNameMsg
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
            transferLog = B.TransferEvent
              { transferEventAmount = purchaseAmount
              , transferEventCoinId = N.nameserviceCoinId
              , transferEventTo = signerAddress user1
              , transferEventFrom = signerAddress user2
              }
            opts = TxOpts
              { txOptsSigner = user2
              , txOptsGas = 0
              }

        -- Add event to be monitored and later checked for
        addEventToCheck tenv transferLog
        addEventToCheck tenv claimedLog

        resp <- assertTx . runTxClientM $ buyName opts msg
        ensureResponseCodes (0,0) resp
        [evtransferLog, evclaimedLog] <- deliverTxEvents (Proxy @N.NameClaimed) resp
        fromEvent evtransferLog `shouldBe` Right transferLog
        fromEvent evclaimedLog `shouldBe` Right claimedLog

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
          getWhois defaultQueryArgs { queryArgsData = N.Name satoshi }
        foundWhois `shouldBe` expected

      -- @NOTE: this is possibly a problem with the go application too
      -- https://cosmos.network/docs/tutorial/buy-name.html#msg
      it "Can buy self-owned names and make a profit " $ \tenv -> do
        -- check balance before
        beforeBuyAmount <- getUserBalance user2
        -- buy
        let bid = 500
            val = "hello (again) world"
            msg = N.BuyNameMsg
              { buyNameBid = bid
              , buyNameName = satoshi
              , buyNameValue = val
              , buyNameBuyer = signerAddress user2
              }
            claimedLog = N.NameClaimed
              { nameClaimedOwner = signerAddress user2
              , nameClaimedName = satoshi
              , nameClaimedValue = val
              , nameClaimedBid = bid
              }
            transferLog = B.TransferEvent
              { transferEventAmount = bid
              , transferEventCoinId = N.nameserviceCoinId
              , transferEventTo = signerAddress user2
              , transferEventFrom = signerAddress user2
              }
            opts = TxOpts
              { txOptsSigner = user2
              , txOptsGas = 0
              }

        -- Add event to be monitored and later checked for
        addEventToCheck tenv transferLog
        addEventToCheck tenv claimedLog

        resp <- assertTx . runTxClientM $ buyName opts msg
        ensureResponseCodes (0,0) resp
        [evtransferLog, evclaimedLog] <- deliverTxEvents (Proxy @N.NameClaimed) resp
        fromEvent evtransferLog `shouldBe` Right transferLog
        fromEvent evclaimedLog `shouldBe` Right claimedLog

        -- check balance after
        afterBuyAmount <- getUserBalance user2
        -- owner/buyer still profits
        afterBuyAmount `shouldSatisfy` (> beforeBuyAmount)

      it "Can fail to buy a name" $ const $ do
        -- try to buy at a lower price
        let msg = N.BuyNameMsg
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

      it "Can delete names" $ \tenv -> do
        let msg = N.DeleteNameMsg
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

        -- Add event to be monitored and later checked for
        addEventToCheck tenv deletedLog

        resp <- assertTx . runTxClientM $ deleteName opts msg
        ensureResponseCodes (0,0) resp
        [evdeletedLog] <- deliverTxEvents (Proxy @N.NameDeleted) resp
        fromEvent evdeletedLog `shouldBe` Right deletedLog

        respQ <- RPC.runTendermintM rpcConfig $
          getWhois defaultQueryArgs { queryArgsData = N.Name satoshi }
        ensureQueryResponseCode 2 respQ


      it "Can fail a transfer" $ const $ do
        addr2Balance <- getUserBalance user2
        let tooMuchToTransfer = addr2Balance + 1
            msg = B.TransferMsg
              { transferFrom = signerAddress user2
              , transferTo = signerAddress user1
              , transferCoinId = N.nameserviceCoinId
              , transferAmount = tooMuchToTransfer
              }
            opts = TxOpts
              { txOptsSigner = user2
              , txOptsGas = 0
              }

        resp <- assertTx . runTxClientM $ transfer opts msg
        ensureResponseCodes (0,1) resp

      it "Can transfer" $ \tenv -> do
        balance1 <- getUserBalance user1
        balance2 <- getUserBalance user2
        let transferAmount = 1
            msg =
              B.TransferMsg
                { transferFrom = signerAddress user1
                , transferTo = signerAddress user2
                , transferCoinId = N.nameserviceCoinId
                , transferAmount = transferAmount
                }
            transferLog = B.TransferEvent
              { transferEventAmount = transferAmount
              , transferEventCoinId = N.nameserviceCoinId
              , transferEventTo = signerAddress user2
              , transferEventFrom = signerAddress user1
              }
            opts = TxOpts
              { txOptsSigner = user1
              , txOptsGas = 0
              }

        -- Add event to be monitored and later checked for
        addEventToCheck tenv transferLog

        resp <- assertTx . runTxClientM $ transfer opts msg
        ensureResponseCodes (0,0) resp
        [evtransferLog] <- deliverTxEvents (Proxy @B.TransferEvent) resp
        fromEvent evtransferLog `shouldBe` Right transferLog

        -- check balances
        balance1' <- getUserBalance user1
        balance1' `shouldBe` balance1 - transferAmount
        balance2' <- getUserBalance user2
        balance2' `shouldBe` balance2 + transferAmount

    it "Can monitor all events" $ \(TestEnv mvex mvres _) -> do
      expected <- readMVar mvex
      res <- readMVar mvres
      fromList expected `shouldBe` fromList res


faucetUser
  :: Auth.Amount
  -> Signer
  -> IO ()
faucetUser amount s@(Signer addr _) =
  void . assertTx .runTxClientM $
    let msg = N.FaucetAccountMsg addr N.nameserviceCoinId amount
        opts = TxOpts
          { txOptsGas = 0
          , txOptsSigner = s
          }
    in faucet opts msg

getUserBalance
  :: Signer
  -> IO Auth.Amount
getUserBalance usr = fmap (Auth.coinAmount . queryResultData) . assertQuery . RPC.runTendermintM rpcConfig $
  let queryArgs = defaultQueryArgs { queryArgsData = signerAddress usr }
  in getBalance queryArgs N.nameserviceCoinId

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
  -> Auth.CoinId
  -> RPC.TendermintM (QueryClientResponse Auth.Coin)

getWhois :<|> getBalance :<|> getAccount =
  genClientQ (Proxy :: Proxy m) queryApiP def
  where
    queryApiP :: Proxy (M.ApplicationQ NameserviceModules)
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
        -- @NOTE: TxNonce should be +1 of accountNonce
        case resp of
          QueryError e ->
            if appErrorCode e == 2
              then pure 1
              else error $ "Unknown nonce error: " <> show (appErrorMessage e)
          QueryResponse QueryResult {queryResultData} ->
            pure $ 1 + Auth.accountNonce queryResultData

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
  -> N.BuyNameMsg
  -> TxClientM (TxClientResponse () ())

setName
  :: TxOpts
  -> N.SetNameMsg
  -> TxClientM (TxClientResponse () ())

deleteName
  :: TxOpts
  -> N.DeleteNameMsg
  -> TxClientM (TxClientResponse () ())

-- Bank Client
transfer
  :: TxOpts
  -> B.TransferMsg
  -> TxClientM (TxClientResponse () ())

faucet
  :: TxOpts
  -> N.FaucetAccountMsg
  -> TxClientM (TxClientResponse () ())

(buyName :<|> setName :<|> deleteName :<|> faucet) :<|>
  (_ :<|> transfer) :<|>
  EmptyTxClient =
    genClientT (Proxy @TxClientM) txApiCP txApiDP defaultClientTxOpts
    where
      txApiCP :: Proxy (M.ApplicationC NameserviceModules)
      txApiCP = Proxy
      txApiDP :: Proxy (M.ApplicationD NameserviceModules)
      txApiDP = Proxy

-- Test Init
data TestEnv = TestEnv (MVar [A.Value]) (MVar [A.Value]) [Text]

testInit :: Auth.Amount -> IO TestEnv
testInit faucetAmount = do
  forM_ [user1, user2] $ faucetUser faucetAmount
  expectedEventsMVar <- newMVar []
  resultEventsMVar <- newMVar []
  pure $ TestEnv expectedEventsMVar resultEventsMVar []

addEventToCheck :: ToEvent a => TestEnv -> a -> IO ()
addEventToCheck (TestEnv mvexpected mvres ses) ev = do
  modifyMVar_ mvexpected $ \es -> pure $ es <> [A.toJSON . toEvent $ ev]
  let evType = eventType (toEvent ev)
  if evType`elem` ses
    then pure ()
    else startNewListener evType
 where
  startNewListener evType =
    let subReq = RPC.RequestSubscribe ("tm.event = 'Tx' AND " <> evType <> " EXISTS")
        forkTendermintM = void . forkIO . void . RPC.runTendermintM rpcConfig
    in forkTendermintM $ RPC.subscribe subReq (handler evType)
  handler evType res = case res ^? txEvents of
    Nothing -> pure ()
    Just v -> case A.fromJSON v of
      A.Error _ -> error ("Failed to parse\n" <> cs (A.encode v) )
      A.Success evs ->
        let filterFn v' = evType == eventType v'
            filteredEvs = filter filterFn evs
        in modifyMVar_ mvres $ \es -> pure $ es <> map A.toJSON filteredEvs
  txEvents = key "result"
           . key "data"
           . key "value"
           . key "TxResult"
           . key "result"
           . key "events"
