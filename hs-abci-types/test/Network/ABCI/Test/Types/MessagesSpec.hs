module Network.ABCI.Test.Types.MessagesSpec (spec) where

import           Control.Lens                                    (Iso', from,
                                                                  set, to,
                                                                  traverse,
                                                                  (%~), (&),
                                                                  (^.), _Just)
import           Control.Lens.Wrapped                            (Wrapped (..),
                                                                  _Unwrapped')
import           Data.ProtoLens.Arbitrary                        (ArbitraryMessage (..))
import           Data.ProtoLens.Field                            (HasField)
import           Data.ProtoLens.Message                          (Message (..))
import           Data.Proxy                                      (Proxy (..))
import           Network.ABCI.Test.Types.Messages.Instances      ()
import qualified Network.ABCI.Types.Messages.FieldTypes          as FT
import qualified Network.ABCI.Types.Messages.Request             as Request
import qualified Network.ABCI.Types.Messages.Response            as Response
import qualified Proto.Types                                     as PT
import qualified Proto.Types_Fields                              as PT
import qualified Proto.Vendored.Google.Protobuf.Timestamp        as T
import qualified Proto.Vendored.Google.Protobuf.Timestamp_Fields as T
import           Test.Hspec
import           Test.QuickCheck                                 (Arbitrary,
                                                                  property)

--------------------------------------------------------------------------------
-- Helpers to check instances
--------------------------------------------------------------------------------

isoCheck'
  :: ( Eq wrapped
     , Eq unwrapped
     , Show wrapped
     , Show unwrapped
     , Arbitrary wrapped
     , Message unwrapped
     , Wrapped wrapped
     , Unwrapped wrapped ~ unwrapped
     )
  => Proxy wrapped
  -> String
  -> (wrapped -> wrapped)
  -> (unwrapped -> unwrapped)
  -> SpecWith ()
isoCheck' _ name mf mg = do
  describe name $ do
    it "Wrapped -> Unwrapped -> Wrapped == identity" $ property $ check _Wrapped' mf mg
    it "Unwrapped -> Wrapped -> Unwrapped == identity" $ property $ check _Unwrapped' mg mf . unArbitraryMessage
  where
    check :: forall a b. Eq a => Iso' a b -> (a -> a) -> (b -> b) -> a -> Bool
    check alpha' pre post val = pre val ^. alpha' . to post . from alpha' == pre val

isoCheck
  :: ( Eq wrapped
     , Eq unwrapped
     , Show wrapped
     , Show unwrapped
     , Arbitrary wrapped
     , Message unwrapped
     , Wrapped wrapped
     , Unwrapped wrapped ~ unwrapped
     )
  => Proxy wrapped
  -> String
  -> SpecWith ()
isoCheck p name =
  isoCheck' p name id id

--------------------------------------------------------------------------------
-- Modifiers
--------------------------------------------------------------------------------

clearUnknownFields
  :: Message a
  => a
  -> a
clearUnknownFields = set unknownFields []

scrubTimestamp
  :: T.Timestamp
  -> T.Timestamp
scrubTimestamp ts =
  ts & T.seconds %~ abs
     & T.nanos %~  (`mod` 1000000000) . abs

scrubTimestampFieldMaybe
  :: HasField a "maybe'time" (Maybe T.Timestamp)
  => a
  -> a
scrubTimestampFieldMaybe a =
  a & PT.maybe'time %~ fmap scrubTimestamp

scrubHeaderTimestampFieldMaybe
  :: HasField a "maybe'header" (Maybe PT.Header)
  => a
  -> a
scrubHeaderTimestampFieldMaybe a =
  a & (PT.maybe'header . _Just . PT.maybe'time) %~ fmap scrubTimestamp


scrubByzantineValidatorsTimestampFieldMaybe
  :: HasField a "byzantineValidators" [PT.Evidence]
  => a
  -> a
scrubByzantineValidatorsTimestampFieldMaybe a =
  a & (PT.byzantineValidators . traverse . PT.maybe'time) %~ fmap scrubTimestamp

--------------------------------------------------------------------------------


spec :: Spec
spec = do
  describe "Request" $ do
    isoCheck (Proxy :: Proxy Request.Echo) "echo"
    isoCheck (Proxy :: Proxy Request.Flush) "flush"
    isoCheck (Proxy :: Proxy Request.Info) "info"
    isoCheck (Proxy :: Proxy Request.SetOption) "setOption"
    isoCheck' (Proxy :: Proxy Request.InitChain) "initChain" id scrubTimestampFieldMaybe
    isoCheck (Proxy :: Proxy Request.Query) "query"
    isoCheck' (Proxy :: Proxy Request.BeginBlock) "beginBlock" id $ scrubHeaderTimestampFieldMaybe . scrubByzantineValidatorsTimestampFieldMaybe
    isoCheck (Proxy :: Proxy Request.CheckTx) "checkTx"
    isoCheck (Proxy :: Proxy Request.DeliverTx) "deliverTx"
    isoCheck (Proxy :: Proxy Request.EndBlock) "endBlock"
    isoCheck (Proxy :: Proxy Request.Commit) "commit"
  describe "Response" $ do
    isoCheck (Proxy :: Proxy Response.Echo) "echo"
    isoCheck (Proxy :: Proxy Response.Flush) "flush"
    isoCheck (Proxy :: Proxy Response.Info) "info"
    isoCheck (Proxy :: Proxy Response.SetOption) "setOption"
    isoCheck (Proxy :: Proxy Response.InitChain) "initChain"
    isoCheck (Proxy :: Proxy Response.Query) "query"
    isoCheck (Proxy :: Proxy Response.BeginBlock) "beginBlock"
    isoCheck (Proxy :: Proxy Response.CheckTx) "checkTx"
    isoCheck (Proxy :: Proxy Response.DeliverTx) "deliverTx"
    isoCheck (Proxy :: Proxy Response.EndBlock) "endBlock"
    isoCheck (Proxy :: Proxy Response.Commit) "commit"
    isoCheck (Proxy :: Proxy Response.Exception) "exception"
  describe "FieldTypes" $ do
    isoCheck' (Proxy :: Proxy FT.Timestamp) "Timestamp" id scrubTimestamp
    isoCheck (Proxy :: Proxy FT.BlockParams) "BlockParams"
    isoCheck (Proxy :: Proxy FT.EvidenceParams) "EvidenceParams"
    isoCheck (Proxy :: Proxy FT.ValidatorParams) "ValidatorParams"
    isoCheck (Proxy :: Proxy FT.ConsensusParams) "ConsensusParam"
    isoCheck (Proxy :: Proxy FT.PubKey) "PubKey"
    isoCheck (Proxy :: Proxy FT.ValidatorUpdate) "ValidatorUpdate"
    isoCheck (Proxy :: Proxy FT.Validator) "Validator"
    isoCheck (Proxy :: Proxy FT.VoteInfo) "VoteInfo"
    isoCheck (Proxy :: Proxy FT.LastCommitInfo) "LastCommitInfo"
    isoCheck (Proxy :: Proxy FT.PartSetHeader) "PartSetHeader"
    isoCheck (Proxy :: Proxy FT.BlockID) "BlockID"
    isoCheck (Proxy :: Proxy FT.Version) "Version"
    isoCheck' (Proxy :: Proxy FT.Header) "Header" id $ scrubTimestampFieldMaybe . clearUnknownFields
    isoCheck' (Proxy :: Proxy FT.Evidence) "Evidence" id scrubTimestampFieldMaybe
    isoCheck (Proxy :: Proxy FT.KVPair) "KVPair"
    isoCheck (Proxy :: Proxy FT.Proof) "Proof"
    isoCheck (Proxy :: Proxy FT.ProofOp) "ProofOp"
    isoCheck (Proxy :: Proxy FT.Event) "Event"
