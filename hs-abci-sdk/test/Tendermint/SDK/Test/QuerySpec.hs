module Tendermint.SDK.Test.QuerySpec (spec) where

import qualified Data.ByteArray.Base64String            as Base64
import           Data.Proxy
import           Data.Text                              (Text)
import qualified Network.ABCI.Types.Messages.Request    as Req
import qualified Network.ABCI.Types.Messages.Response   as Resp
import qualified Tendermint.SDK.Application             as App
import qualified Tendermint.SDK.Application.Module      as M
import qualified Tendermint.SDK.BaseApp                 as BA
import qualified Tendermint.SDK.BaseApp.Logger.Katip    as KL
import qualified Tendermint.SDK.BaseApp.Store           as Store
import qualified Tendermint.SDK.BaseApp.Store.IAVLStore as IAVL
import           Tendermint.SDK.Codec                   (HasCodec (..))
import qualified Tendermint.SDK.Test.SimpleStorage      as SS
import           Tendermint.SDK.Types.Message           (Msg (..))
import           Tendermint.SDK.Types.Transaction       (Tx (..))
import           Test.Hspec

type Effs = SS.SimpleStorage ': BA.TxEffs BA.:& App.BaseApp BA.CoreEffs

type Ms = '[SS.SimpleStorageM Effs]

spec :: Spec
spec = beforeAll initContext $
  describe "Query tests" $ do
    let modules :: App.ModuleList Ms Effs
        modules = SS.simpleStorageModule App.:+ App.NilModules
        rProxy = Proxy @(App.BaseApp BA.CoreEffs)
        app = M.makeApplication rProxy mempty modules
        ssServer = BA.serveQueryApplication (Proxy @(M.ApplicationQ Ms)) rProxy $ M.applicationQuerier app
        updateCount = BA.serveTxApplication (Proxy @(M.ApplicationD Ms)) rProxy (Proxy @'Store.Consensus) $ M.applicationTxDeliverer app
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
        _ <- SS.evalToIO ctx $ updateCount tx
        let q = Req.Query
              -- TODO -- this shouldn't require / count
              { queryPath = "/simple_storage/manipulated/1?factor=4"
              , queryData = undefined
              , queryProve = False
              , queryHeight = 0
              }
        Resp.Query{..} <- SS.evalToIO ctx $ ssServer q
        queryCode `shouldBe` 0
        let resultCount = decode (Base64.toBytes queryValue) :: Either Text SS.Count
        resultCount `shouldBe` Right 3

initContext :: IO BA.Context
initContext = do
  versions <- IAVL.initIAVLVersions
  let grpc = IAVL.GrpcConfig "0.0.0.0" 8090
  BA.makeContext (KL.InitialLogNamespace "test" "spec") Nothing versions grpc
