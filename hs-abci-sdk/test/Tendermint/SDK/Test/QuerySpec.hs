module Tendermint.SDK.Test.QuerySpec (spec) where

import           Control.Lens                             ((^.))
import           Control.Monad.IO.Class                   (liftIO)
import qualified Data.ByteArray.Base64String              as Base64
import qualified Data.ByteArray.HexString                 as Hex
import           Data.ByteString                          (ByteString)
import           Data.Maybe                               (isJust)
import           Data.Proxy
import           Data.Text                                (Text)
import qualified Network.ABCI.Types.Messages.Request      as Req
import qualified Network.ABCI.Types.Messages.Response     as Resp
import           Polysemy                                 (Sem)
import qualified Tendermint.SDK.Application               as App
import qualified Tendermint.SDK.Application.Module        as M
import qualified Tendermint.SDK.BaseApp                   as BA
import qualified Tendermint.SDK.BaseApp.Logger.Katip      as KL
import           Tendermint.SDK.BaseApp.Query             (QueryApplication,
                                                           serveQueryApplication)
import qualified Tendermint.SDK.BaseApp.Store             as Store
import           Tendermint.SDK.BaseApp.Transaction       (TransactionApplication,
                                                           serveTxApplication)
import           Tendermint.SDK.BaseApp.Transaction.Cache (writeCache)
import           Tendermint.SDK.Codec                     (HasCodec (..))
import qualified Tendermint.SDK.Modules.Auth              as A
import qualified Tendermint.SDK.Modules.Bank              as B
import qualified Tendermint.SDK.Test.SimpleStorage        as SS
import           Tendermint.SDK.Types.Message             (Msg (..))
import           Tendermint.SDK.Types.Transaction         (Tx (..))
import           Test.Hspec

type Ms = '[SS.SimpleStorage, B.Bank, A.Auth]

modules :: App.ModuleList Ms (App.Effs Ms BA.PureCoreEffs)
modules =
  SS.simpleStorageModule App.:+
  B.bankModule App.:+
  A.authModule App.:+
  App.NilModules

cProxy :: Proxy BA.PureCoreEffs
cProxy = Proxy

rProxy :: Proxy (BA.BaseAppEffs BA.PureCoreEffs)
rProxy = Proxy

app :: M.Application (M.ApplicationC Ms) (M.ApplicationD Ms) (M.ApplicationQ Ms)
         (BA.TxEffs BA.:& BA.BaseAppEffs BA.PureCoreEffs) (BA.QueryEffs BA.:& BA.BaseAppEffs BA.PureCoreEffs)
app = M.makeApplication cProxy mempty modules

doQuery :: QueryApplication (Sem (BA.BaseAppEffs BA.PureCoreEffs))
doQuery = serveQueryApplication (Proxy @(M.ApplicationQ Ms)) rProxy $ M.applicationQuerier app

doTx :: TransactionApplication (Sem (BA.BaseAppEffs BA.PureCoreEffs))
doTx = serveTxApplication (Proxy @(M.ApplicationD Ms)) rProxy (Proxy @'Store.Consensus) $ M.applicationTxDeliverer app

spec :: Spec
spec = beforeAll initContext $ do
  describe "Query tests" $ do
    it "Can make a new count and query it with a multiplier" $ \ctx -> do
        let increaseCountMsg = Msg
              { msgAuthor = undefined
              , msgType = "update_count"
              , msgData = encode $ SS.UpdateCountTx 1
              }
            tx = BA.RoutingTx $ Tx
              { txMsg = increaseCountMsg
              , txRoute = "simple_storage"
              , txGas = 0
              , txSignature = undefined
              , txSignBytes = undefined
              , txSigner = undefined
              , txNonce = undefined
              }
        _ <- SS.evalToIO ctx $ do
          (_, mCache) <- doTx tx
          liftIO (mCache `shouldSatisfy` isJust)
          let (Just cache) = mCache
          writeCache cache
          _ <- Store.commit
          Store.commitBlock
        let q = Req.Query
              -- TODO -- this shouldn't require / count
              { queryPath = "/simple_storage/manipulated/1?factor=4"
              , queryData = undefined
              , queryProve = False
              , queryHeight = 0
              }
        Resp.Query{..} <- SS.evalToIO ctx $ doQuery q
        queryCode `shouldBe` 0
        let resultCount = decode (Base64.toBytes queryValue) :: Either Text SS.Count
        resultCount `shouldBe` Right 3

  describe "throw/catch tests" $ do
    it "Can throw and catch a bank error" $ \ctx -> do
        let addr = A.Address . Hex.fromBytes $ ("1234" :: ByteString)

            queryBalance = do
             let path = "/bank/balance?coin_id=" <> B.unCoinId SS.simpleStorageCoinId
                 q1 = Req.Query
                   { queryPath = path
                   , queryData = Base64.fromBytes (addr ^. BA.rawKey)
                   , queryProve = False
                   , queryHeight = 0
                   }
             Resp.Query{..} <- SS.evalToIO ctx $ doQuery q1
             queryCode `shouldBe` 0
             let Right resultBalance = decode (Base64.toBytes queryValue) :: Either Text B.Coin
             pure resultBalance

        initialBalance <- queryBalance

        let increaseCountMsg = Msg
              { msgAuthor = addr
              , msgType = "update_paid_count"
              , msgData = encode $ SS.UpdatePaidCountTx 33 10
              }

            tx = BA.RoutingTx $ Tx
              { txMsg = increaseCountMsg
              , txRoute = "simple_storage"
              , txGas = 0
              , txSignature = undefined
              , txSignBytes = undefined
              , txSigner = undefined
              , txNonce = undefined
              }
        _ <- SS.evalToIO ctx $ do
          (_, mCache) <- doTx tx
          liftIO (mCache `shouldSatisfy` isJust)
          let (Just cache) = mCache
          writeCache cache
          _ <- Store.commit
          Store.commitBlock
        let q = Req.Query
              -- TODO -- this shouldn't require / count
              { queryPath = "/simple_storage/count"
              , queryData = undefined
              , queryProve = False
              , queryHeight = 0
              }
        Resp.Query{..} <- SS.evalToIO ctx $ doQuery q
        queryCode `shouldBe` 0
        let resultCount = decode (Base64.toBytes queryValue) :: Either Text SS.Count
        resultCount `shouldBe` Right 33

        finalBalance <- queryBalance
        finalBalance `shouldBe` initialBalance {B.coinAmount = B.coinAmount initialBalance + 1}


initContext :: IO BA.PureContext
initContext = do
  BA.makePureContext (KL.InitialLogNamespace "test" "spec")
