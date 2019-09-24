module SimpleStorage.Modules.SimpleStorage
  (
  -- * Component
    simpleStorageComponent
  , Query(..)
  , Message(..)
  , Api

  -- * Store
  , CountStoreContents
  , CountStore

  -- * Types
  , Count(..)
  , CountKey(..)

  ) where

import           Control.Lens                 (from, iso, (^.))
import           Control.Monad.Trans
import           Crypto.Hash                  (SHA256 (..), hashWith)
import qualified Data.Binary                  as Binary
import           Data.ByteArray               (convert)
import           Data.ByteArray.Base64String  (fromBytes, toBytes)
import           Data.ByteString              (ByteString)
import           Data.Int                     (Int32)
import           Data.Maybe                   (fromJust)
import           Data.Proxy
import           Data.String.Conversions      (cs)
import           Servant.API                  ((:>))
import           Tendermint.SDK.AuthTreeStore
import           Tendermint.SDK.Codec
import           Tendermint.SDK.Module
import           Tendermint.SDK.Router
import           Tendermint.SDK.Store
import           Tendermint.SDK.StoreQueries

--------------------------------------------------------------------------------
-- SimpleStorage Module
--------------------------------------------------------------------------------

data Query a =
    PutCount Count a
  | GetCount (Count -> a)

data Message =
  StoredCount Count

data Action =
  InitializeCount

type CountStoreContents = '[Count]

type CountStore = Store CountStoreContents IO

evalQuery :: forall a m. MonadIO m => Query a -> TendermintM CountStore Action Message m a
evalQuery (PutCount count a) = do
  withState $ \store ->
    liftIO $ put CountKey count store
  raise $ StoredCount count
  pure a
evalQuery (GetCount f) = do
  buyer <- withState $ \store ->
    -- fromJust is safe because the count is initalized to 0
    liftIO $ fromJust <$> get (undefined :: Root) CountKey store
  pure $ f buyer

evalAction :: forall m. MonadIO m => Action -> TendermintM CountStore Action Message m ()
evalAction InitializeCount =
  withState $ \store ->
    liftIO $ put CountKey (Count 0) store


type Api = "count" :> QueryApi CountStoreContents

simpleStorageComponentSpec :: MonadIO m => ComponentSpec CountStore Query Action input Message Api m
simpleStorageComponentSpec = ComponentSpec
  { initialState = const $ do
      rawStore <- liftIO $ mkAuthTreeStore
      pure $ Store
        { storeRawStore = rawStore }
  , eval = evaluator
  , mkServer = hoistRoute (Proxy :: Proxy Api) liftIO . simpleStorageServer
  }
  where
    simpleStorageServer :: CountStore -> RouteT Api IO
    simpleStorageServer = allStoreHandlers

    evaluator = mkEval $ EvalSpec
      { handleAction = evalAction
      , handleQuery = evalQuery
      , receive = const Nothing
      , initialize = Just InitializeCount
      }

simpleStorageComponent :: forall (input :: *) m. MonadIO m => Component Query input Message Api m
simpleStorageComponent = Component simpleStorageComponentSpec

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

newtype Count = Count Int32 deriving (Eq, Show)

data CountKey = CountKey

instance HasCodec Count where
    encode (Count c) = cs . Binary.encode $ c
    decode = Right . Count . Binary.decode . cs

instance HasKey Count where
    type Key Count = CountKey
    rawKey = iso (\_ -> cs countKey) (const CountKey)
      where
        countKey :: ByteString
        countKey = convert . hashWith SHA256 . cs @_ @ByteString $ ("count" :: String)

instance FromQueryData CountKey where
  fromQueryData bs = Right (toBytes bs ^. from rawKey)

instance EncodeQueryResult Count where
  encodeQueryResult = fromBytes . encode

instance Queryable Count where
  type Name Count = "count"
