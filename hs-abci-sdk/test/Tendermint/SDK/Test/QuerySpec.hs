module Tendermint.SDK.Test.QuerySpec (spec) where

import qualified Data.ByteArray.Base64String          as Base64
import           Data.Text                            (Text)
import qualified Network.ABCI.Types.Messages.Request  as Req
import qualified Network.ABCI.Types.Messages.Response as Resp
import qualified Tendermint.SDK.Application           as App
import qualified Tendermint.SDK.Application.Module    as M
import qualified Tendermint.SDK.BaseApp               as BA
import qualified Tendermint.SDK.BaseApp.Logger.Katip  as KL
import qualified Tendermint.SDK.BaseApp.Transaction   as T
import           Tendermint.SDK.Codec                 (HasCodec (..))
import qualified Tendermint.SDK.Test.SimpleStorage    as SS
import           Tendermint.SDK.Types.Message         (Msg (..))
import           Tendermint.SDK.Types.Transaction     (PreRoutedTx (..),
                                                       Tx (..))
import           Test.Hspec

type Effs = SS.SimpleStorage ': BA.BaseApp BA.CoreEffs

spec :: Spec
spec = beforeAll (BA.makeContext (KL.InitialLogNamespace "test" "spec") Nothing) $
  describe "Query tests" $ do
    let modules :: App.Modules '[SS.SimpleStorageM Effs] Effs
        modules = SS.simpleStorageModule App.:+ App.NilModules
        ssServer = M.queryRouter modules
        handler = App.moduleTxDeliverer SS.simpleStorageModule
    it "Can make a new count and query it with a multiplier" $ \ctx -> do
        let increaseCountMsg = Msg
              { msgAuthor = undefined
              , msgData = SS.UpdateCount $ SS.UpdateCountTx 1
              }
            tx = PreRoutedTx $ Tx
              { txMsg = increaseCountMsg
              , txRoute = undefined
              , txGas = 0
              , txSignature = undefined
              , txSignBytes = undefined
              , txSigner = undefined
              , txNonce = undefined
              }
        txContext <- T.newTransactionContext tx
        _ <- SS.evalToIO ctx . T.eval txContext $ handler tx
        let q = Req.Query
              -- TODO -- this shouldn't require / count
              { queryPath = "/simple_storage/multiplied?factor=4"
              , queryData = undefined
              , queryProve = False
              , queryHeight = 0
              }
        Resp.Query{..} <- SS.evalToIO ctx $ ssServer q
        queryCode `shouldBe` 0
        let resultCount = decode (Base64.toBytes queryValue) :: Either Text SS.Count
        resultCount `shouldBe` Right 4
