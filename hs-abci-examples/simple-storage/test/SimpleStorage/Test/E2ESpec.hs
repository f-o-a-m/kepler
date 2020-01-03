module SimpleStorage.Test.E2ESpec (spec) where

import           Control.Lens                         ((^.))
import           Control.Monad.Reader                 (ReaderT)
import           Crypto.Secp256k1                     (SecKey,
                                                       exportCompactRecSig,
                                                       secKey)
import           Data.Aeson                           (ToJSON)
import           Data.Aeson.Encode.Pretty             (encodePretty)
import qualified Data.ByteArray.Base64String          as Base64
import qualified Data.ByteArray.HexString             as Hex
import           Data.ByteString                      (ByteString)
import           Data.Default.Class                   (def)
import           Data.Maybe                           (fromJust)
import           Data.Proxy
import qualified Data.Serialize                       as Serial
import           Data.String                          (fromString)
import           Data.String.Conversions              (cs)
import qualified Network.ABCI.Types.Messages.Request  as Req
import qualified Network.ABCI.Types.Messages.Response as Resp
import qualified Network.Tendermint.Client            as RPC
import           Servant.API                          ((:>))
import qualified SimpleStorage.Modules.SimpleStorage  as SS
import           Tendermint.SDK.BaseApp.Query         (QueryArgs (..))
import           Tendermint.SDK.BaseApp.Query.Client  (ClientResponse (..),
                                                       HasClient (..),
                                                       RunClient (..))
import           Tendermint.SDK.Codec                 (HasCodec (..))
import           Tendermint.SDK.Crypto                (Secp256k1)
import           Tendermint.SDK.Types.Transaction     (RawTransaction (..),
                                                       signRawTransaction)
import           Test.Hspec


spec :: Spec
spec = do
  describe "SimpleStorage E2E - via hs-tendermint-client" $ do

    it "Can query /health to make sure the node is alive" $ do
      resp <- runRPC RPC.health
      resp `shouldBe` RPC.ResultHealth

    --it "Can query the count and make sure its initialized to 0" $ do
    --  let queryReq = QueryArgs
    --        { queryArgsData = SS.CountKey
    --        , queryArgsHeight = 0
    --        , queryArgsProve = False
    --        }
    --  ClientResponse{clientResponseData = Just foundCount} <- runQueryRunner $ getCount queryReq
    --  foundCount `shouldBe` SS.Count 0

    it "Can submit a tx synchronously and make sure that the response code is 0 (success)" $ do
      let txMsg = SS.UpdateCount $ SS.UpdateCountTx "irakli" 4
          tx = mkSignedRawTransaction (userPrivKey user1) txMsg
          txReq = RPC.RequestBroadcastTxCommit
                    { RPC.requestBroadcastTxCommitTx = Base64.fromBytes . encode $ tx
                    }
      deliverResp <- fmap RPC.resultBroadcastTxCommitDeliverTx . runRPC $ RPC.broadcastTxCommit txReq
      let deliverRespCode = deliverResp ^. Resp._deliverTxCode
      deliverRespCode `shouldBe` 0

    it "can make sure the synchronous tx transaction worked and the count is now 4" $ do
      let queryReq = QueryArgs
            { queryArgsData = SS.CountKey
            , queryArgsHeight = 0
            , queryArgsProve = False
            }
      ClientResponse{clientResponseData = Just foundCount} <- runQueryRunner $ getCount queryReq
      foundCount `shouldBe` SS.Count 4

runRPC :: forall a. RPC.TendermintM a -> IO a
runRPC = RPC.runTendermintM rpcConfig
  where
    rpcConfig :: RPC.Config
    rpcConfig =
      let RPC.Config baseReq _ _ = RPC.defaultConfig "localhost" 26657
          prettyPrint :: forall b. ToJSON b => String -> b -> IO ()
          prettyPrint prefix a = putStrLn $ prefix <> "\n" <> (cs . encodePretty $ a)
      in RPC.Config baseReq (prettyPrint "RPC Request") (prettyPrint "RPC Response")

newtype QueryRunner a = QueryRunner
  {_runQueryRunner :: ReaderT RPC.Config IO a}
  deriving (Functor, Applicative, Monad)

runQueryRunner :: QueryRunner a -> IO a
runQueryRunner = runRPC . _runQueryRunner

instance RunClient QueryRunner where
  runQuery Req.Query{..} =
    let rpcQ = RPC.RequestABCIQuery
          { RPC.requestABCIQueryPath = Just queryPath
          , RPC.requestABCIQueryData = Hex.fromBytes @ByteString . Base64.toBytes $ queryData
          , RPC.requestABCIQueryHeight = Just $ queryHeight
          , RPC.requestABCIQueryProve  = queryProve
          }
    in RPC.resultABCIQueryResponse <$> QueryRunner (RPC.abciQuery rpcQ)

--------------------------------------------------------------------------------

getCount :: QueryArgs SS.CountKey -> QueryRunner (ClientResponse SS.Count)
getCount =
  let apiP = Proxy :: Proxy ("simple_storage" :> SS.Api)
  in genClient (Proxy :: Proxy QueryRunner) apiP def

-- sign a tx with a user's private key
mkSignedRawTransaction :: SecKey -> SS.SimpleStorageMessage -> RawTransaction
mkSignedRawTransaction privateKey msg = sign unsigned
  where unsigned = RawTransaction { rawTransactionData = encode msg
                                  , rawTransactionRoute = "simple_storage"
                                  , rawTransactionSignature = ""
                                  , rawTransactionGas = 0
                                  }
        sig = signRawTransaction algProxy privateKey unsigned
        sign rt = rt { rawTransactionSignature = Serial.encode $ exportCompactRecSig sig }

data User = User
  { userPrivKey :: SecKey
  }

user1 :: User
user1 = makeUser "f65255094d7773ed8dd417badc9fc045c1f80fdc5b2d25172b031ce6933e039a"

makeUser :: String -> User
makeUser privKeyStr =
  let privateKey = fromJust . secKey . Hex.toBytes . fromString $ privKeyStr
  in User privateKey

algProxy :: Proxy Secp256k1
algProxy = Proxy
