module Network.ABCI.Test.Types.MessagesSpec where

import           Control.Lens                           (Iso', _Just, from, (^.), to, set, iso, view, (%~), (&), traverse)
import           Data.Maybe (fromMaybe)
import           Data.ProtoLens.Message                 (Message(..))
import           Data.Time.Clock (diffTimeToPicoseconds, picosecondsToDiffTime)
import           Network.ABCI.Test.Types.Messages.Instances ()
import           Network.ABCI.Types.Messages.FieldTypes as FT
import           Network.ABCI.Types.Messages.Request    as Request
import           Network.ABCI.Types.Messages.Response   as Response
import           Test.Hspec
import           Data.ProtoLens.Arbitrary              (ArbitraryMessage(..))
import           Test.QuickCheck (Arbitrary, property)
import qualified Lens.Labels
import qualified Proto.Types                                                      as PT
import qualified Proto.Types_Fields                                               as PT
import qualified Proto.Vendored.Google.Protobuf.Timestamp                         as T
import qualified Proto.Vendored.Google.Protobuf.Timestamp_Fields                  as T

main :: IO ()
main = pure () --hspec spec

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
     )
  => String
  -> Iso' wrapped unwrapped
  -> Maybe (wrapped -> wrapped)
  -> Maybe (unwrapped -> unwrapped)
  -> SpecWith ()
isoCheck' name alpha mf mg = do
  describe name $ do
    it "Wrapped -> Unwrapped -> Wrapped == identity" $ property $ check alpha mf mg
    it "Unwrapped -> Wrapped -> Unwrapped == identity" $ property $ check (from alpha) mg mf . unArbitraryMessage
  where
    check :: forall a b. Eq a => Iso' a b -> Maybe (a -> a) -> Maybe (b -> b) -> a -> Bool
    check alpha' mPre mPost val =
      let
        pre = fromMaybe id mPre :: a -> a
        post = fromMaybe id mPost :: b -> b
      in
        pre val ^. alpha' . to post . from alpha' == pre val

isoCheck
  :: ( Eq wrapped
     , Eq unwrapped
     , Show wrapped
     , Show unwrapped
     , Arbitrary wrapped
     , Message unwrapped
     )
  => String
  -> Iso' wrapped unwrapped
  -> SpecWith ()
isoCheck name alpha =
  isoCheck' name alpha Nothing Nothing

--------------------------------------------------------------------------------
-- Modifiers
--------------------------------------------------------------------------------

clearUnkownFields
  :: Message a
  => a
  -> a
clearUnkownFields = set unknownFields []

scrubTimestamp
  :: T.Timestamp
  -> T.Timestamp
scrubTimestamp ts =
  ts & T.seconds %~ abs
     & T.nanos %~  (`mod` 1000000000) . abs

scrubTimestampField
  :: Lens.Labels.HasLens' a "time" T.Timestamp
  => a
  -> a
scrubTimestampField a =
  a & PT.time %~ scrubTimestamp

scrubTimestampFieldMaybe
  :: Lens.Labels.HasLens' a "maybe'time" (Maybe T.Timestamp)
  => a
  -> a
scrubTimestampFieldMaybe a =
  a & PT.maybe'time %~ fmap scrubTimestamp

scrubHeaderTimestampFieldMaybe
  :: Lens.Labels.HasLens' a "maybe'header" (Maybe PT.Header)
  => a
  -> a
scrubHeaderTimestampFieldMaybe a =
  a & (PT.maybe'header . _Just . PT.maybe'time) %~ fmap scrubTimestamp


scrubByzantineValidatorsTimestampFieldMaybe
  :: Lens.Labels.HasLens' a "byzantineValidators" [PT.Evidence]
  => a
  -> a
scrubByzantineValidatorsTimestampFieldMaybe a =
  a & (PT.byzantineValidators . traverse . PT.maybe'time) %~ fmap scrubTimestamp

--------------------------------------------------------------------------------

spec :: Spec
spec = do
  describe "Request" $ do
   isoCheck "echo" Request.echo
   isoCheck "flush" Request.flush
   isoCheck "info" Request.info
   isoCheck "setOption" Request.setOption
   isoCheck' "initChain" Request.initChain Nothing (Just scrubTimestampFieldMaybe)
   isoCheck "query" Request.query
   isoCheck' "beginBlock" Request.beginBlock Nothing (Just $ scrubHeaderTimestampFieldMaybe . scrubByzantineValidatorsTimestampFieldMaybe)
   isoCheck "checkTx" Request.checkTx
   isoCheck "deliverTx" Request.deliverTx
   isoCheck "endBlock" Request.endBlock
   isoCheck "commit" Request.commit
  describe "Response" $ do
     isoCheck "echo" Response.echo
     isoCheck "flush" Response.flush
     isoCheck "info" Response.info
     isoCheck "setOption" Response.setOption
     isoCheck "initChain" Response.initChain
     isoCheck "query" Response.query
     isoCheck "beginBlock" Response.beginBlock
     isoCheck "checkTx" Response.checkTx
     isoCheck "deliverTx" Response.deliverTx
     isoCheck "endBlock" Response.endBlock
     isoCheck "commit" Response.commit
     isoCheck "exception" Response.exception
  describe "FieldTypes" $ do
    isoCheck' "Timestamp" FT.timestamp Nothing (Just scrubTimestamp)
    isoCheck "BlockSizeParams" FT.blockSizeParams
    isoCheck "EvidenceParams" FT.evidenceParams
    isoCheck "ValidatorParams" FT.validatorParams
    isoCheck "ConsensusParam" FT.consensusParams
    isoCheck "PubKey" FT.pubKey
    isoCheck "ValidatorUpdate" FT.validatorUpdate
    isoCheck "Validator" FT.validator
    isoCheck "VoteInfo" FT.voteInfo
    isoCheck "LastCommitInfo" FT.lastCommitInfo
    isoCheck "PartSetHeader" FT.partSetHeader
    isoCheck "BlockID" FT.blockID
    isoCheck "Version" FT.version
    isoCheck' "Header" FT.header Nothing (Just $ scrubTimestampFieldMaybe . clearUnkownFields)
    isoCheck' "Evidence" FT.evidence Nothing (Just scrubTimestampFieldMaybe)
    isoCheck "KVPair" FT.kVPair
    isoCheck "Proof" FT.proof
    isoCheck "ProofOp" FT.proofOp
