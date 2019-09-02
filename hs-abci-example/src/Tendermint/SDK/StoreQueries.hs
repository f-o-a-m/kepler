module Tendermint.SDK.StoreQueries where

import           Control.Lens                         (from, to, (&), (.~),
                                                       (^.))
--import Servant.API
-- import Tendermint.SDK.Routes
import           Data.ByteArray.HexString             (fromBytes, toBytes)
import           Data.Default.Class                   (def)
import           Data.Proxy
import qualified Network.ABCI.Types.Messages.Request  as Request
import qualified Network.ABCI.Types.Messages.Response as Response
import           Tendermint.SDK.Codec
import           Tendermint.SDK.Store


class StoreQueryHandler a store h where
    storeQueryHandler :: Proxy a -> store -> h

instance (HasKey a, ContainsCodec a contents, Monad m)
   => StoreQueryHandler a (Store contents m) (Request.Query -> m Response.Query) where
  storeQueryHandler _ store query = do
    let key = query ^. Request._queryData . to toBytes . from rawKey :: Key a
    mRes <- get (key :: Key a) store
    case mRes of
        Nothing -> pure def
        Just res -> pure $ def & Response._queryValue .~ fromBytes (encode res)

class StoreQueryHandlers contents m hs where
    storeQueryHandlers :: Store contents m -> hs

--instance {-# OVERLAPPING #-} HasKey a => StoreQueryHandlers (Store '[a] m) (Response Query where
--    storeQueryHandlers
