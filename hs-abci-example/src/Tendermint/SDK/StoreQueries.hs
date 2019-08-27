module Tendermint.SDK.StoreQueries where

import Control.Lens ((^.), (&), (.~), from, to)
--import Servant.API
-- import Tendermint.SDK.Routes
import Tendermint.SDK.Store
import Tendermint.SDK.Codec
import Data.Proxy
import Data.Default.Class (def)
import qualified Network.ABCI.Types.Messages.Request as Request
import qualified Network.ABCI.Types.Messages.Response as Response
import Data.ByteArray.HexString (toBytes, fromBytes)


class StoreQueryHandler a store h where
    storeQueryHandler :: Proxy a -> store -> h

instance (HasKey a, HasCodec a contents, Monad m) => StoreQueryHandler a (Store contents m) (Request.Query -> m Response.Query) where
  storeQueryHandler _ store@Store{storeCodecs} query = do
    let key = query ^. Request._queryData . to toBytes . from rawKey :: Key a
        Codec{codecEncode} = getCodec storeCodecs :: Codec a
    mRes <- get (key :: Key a) store
    case mRes of
        Nothing -> pure def
        Just res -> pure $ def & Response._queryValue .~ (fromBytes $ codecEncode res)

class StoreQueryHandlers contents m hs where
    storeQueryHandlers :: Store contents m -> hs

--instance {-# OVERLAPPING #-} HasKey a => StoreQueryHandlers (Store '[a] m) (Response Query where
--    storeQueryHandlers 