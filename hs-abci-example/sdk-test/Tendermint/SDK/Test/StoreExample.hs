module Tendermint.SDK.Test.StoreExample where

import           Control.Lens                     (from, iso, (^.))
import           Data.Binary                      (Binary)
import qualified Data.Binary                      as Binary
import           Data.ByteArray.Base64String
import           Data.String.Conversions          (cs)
import           GHC.Generics                     (Generic)
import           Tendermint.SDK.Codec
import           Tendermint.SDK.Store
import           Test.QuickCheck                   (getPrintableString)
import           Test.QuickCheck.Arbitrary         (Arbitrary, arbitrary)
import           Test.QuickCheck.Arbitrary.Generic (genericArbitrary)
import           Test.QuickCheck.Instances         ()
import           Tendermint.SDK.Router.Types

--------------------------------------------------------------------------------
-- Example Store
--------------------------------------------------------------------------------

type AuthTreeRawStore = RawStore IO

--------------------------------------------------------------------------------
-- UserStore
--------------------------------------------------------------------------------
-- | Buyer
data Buyer = Buyer
  { buyerId   :: String
  , buyerName :: String
  } deriving (Eq, Show, Generic)

newtype BuyerKey = BuyerKey String deriving Show

instance Binary Buyer

instance HasCodec Buyer where
    encode = cs . Binary.encode
    decode = Right . Binary.decode . cs

instance HasKey Buyer where
    type Key Buyer = BuyerKey
    rawKey = iso (\(BuyerKey k) -> cs k) (BuyerKey . cs)

instance FromQueryData BuyerKey where
  fromQueryData bs = Right (toBytes bs ^. from rawKey)

instance EncodeQueryResult Buyer where
  encodeQueryResult = fromBytes . encode

instance Queryable Buyer where
  type Name Buyer = "buyer"

-- | Owner
data Owner = Owner
  { ownerId   :: String
  , ownerName :: String
  } deriving (Eq, Show, Generic)

instance Binary Owner

newtype OwnerKey = OwnerKey String deriving Show

instance HasKey Owner where
  type Key Owner = OwnerKey
  rawKey = iso (\(OwnerKey k) -> cs k) (OwnerKey . cs)

instance HasCodec Owner where
  encode = cs . Binary.encode
  decode = Right . Binary.decode . cs

instance EncodeQueryResult Owner where
  encodeQueryResult  = fromBytes . encode

instance FromQueryData OwnerKey where
  fromQueryData bs = Right (toBytes bs ^. from rawKey)

instance Queryable Owner where
  type Name Owner = "owner"

-- | Store
type UserStoreContents = '[Owner, Buyer]

type UserStore = Store UserStoreContents IO

putOwner
  :: OwnerKey
  -> Owner
  -> UserStore
  -> IO ()
putOwner k owner store = put k owner store

putBuyer
  :: BuyerKey
  -> Buyer
  -> UserStore
  -> IO ()
putBuyer k buyer store = put k buyer store

--------------------------------------------------------------------------------
-- DogStore
--------------------------------------------------------------------------------
-- | Hound
data Hound = Hound
  { houndId   :: String
  , houndName :: String
  } deriving (Eq, Show, Generic)

newtype HoundKey = HoundKey String deriving Show

instance Binary Hound

instance HasCodec Hound where
    encode = cs . Binary.encode
    decode = Right . Binary.decode . cs

instance HasKey Hound where
    type Key Hound = HoundKey
    rawKey = iso (\(HoundKey k) -> cs k) (HoundKey . cs)

instance FromQueryData HoundKey where
  fromQueryData hx = Right (toBytes hx ^. from rawKey)

instance EncodeQueryResult Hound where
  encodeQueryResult = fromBytes . encode

instance Queryable Hound where
  type Name Hound = "hound"

-- | Lab
data Lab = Lab
  { labId   :: String
  , labName :: String
  } deriving (Eq, Show, Generic)

instance Binary Lab

newtype LabKey = LabKey String deriving Show

instance HasKey Lab where
  type Key Lab = LabKey
  rawKey = iso (\(LabKey k) -> cs k) (LabKey . cs)

instance HasCodec Lab where
  encode = cs . Binary.encode
  decode = Right . Binary.decode . cs

instance EncodeQueryResult Lab where
  encodeQueryResult  = fromBytes . encode

instance FromQueryData LabKey where
  fromQueryData hx = Right (toBytes hx ^. from rawKey)

instance Queryable Lab where
  type Name Lab = "lab"

-- | DogStore
type DogStoreContents = '[Lab, Hound]

type DogStore = Store DogStoreContents IO

putLab
  :: LabKey
  -> Lab
  -> DogStore
  -> IO ()
putLab k lab store = put k lab store

putHound
  :: HoundKey
  -> Hound
  -> DogStore
  -> IO ()
putHound k hound store = put k hound store
-- userStore :: UserStore
-- userStore = unsafePerformIO $ do
--   rawStore <- mkAuthTreeStore
--   pure $ Store
--     { storeRawStore = rawStore
--     }
-- {-# NOINLINE userStore #-}

-- userApi :: Proxy (QueryApi UserStoreContents)
-- userApi = Proxy

-- userServer :: RouteT (QueryApi UserStoreContents) IO
-- userServer = storeQueryHandlers (Proxy :: Proxy UserStoreContents) userStore

-- serveRoutes :: Application IO
-- serveRoutes = serve userApi (Proxy :: Proxy IO) userServer
--------------------------------------------------------------------------------
instance Arbitrary Buyer where arbitrary = genericArbitrary
instance Arbitrary BuyerKey where arbitrary = BuyerKey . getPrintableString <$> arbitrary
instance Arbitrary Owner where arbitrary = genericArbitrary
instance Arbitrary OwnerKey where arbitrary = OwnerKey . getPrintableString <$> arbitrary
instance Arbitrary Lab where arbitrary = genericArbitrary
instance Arbitrary LabKey where arbitrary = LabKey . getPrintableString <$> arbitrary
instance Arbitrary Hound where arbitrary = genericArbitrary
instance Arbitrary HoundKey where arbitrary = HoundKey . getPrintableString <$> arbitrary
