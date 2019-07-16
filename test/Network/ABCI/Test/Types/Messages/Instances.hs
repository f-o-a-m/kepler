{-# OPTIONS_GHC -fno-warn-orphans #-}
module Network.ABCI.Test.Types.Messages.Instances () where

import Control.Lens ((&), (.~))
import Test.QuickCheck.Instances ()

import Network.ABCI.Types.Messages.FieldTypes as FieldTypes
import Network.ABCI.Types.Messages.Request    as Request
import Network.ABCI.Types.Messages.Response   as Response
import Test.QuickCheck.Arbitrary.Generic      (genericArbitrary)
import Test.QuickCheck.Arbitrary              (Arbitrary, arbitrary)
import           Data.ProtoLens.Message                 (Message(..))
import           Data.ProtoLens.Arbitrary              (ArbitraryMessage(..))
import qualified Proto.Vendored.Google.Protobuf.Timestamp                         as T
import qualified Proto.Vendored.Google.Protobuf.Timestamp_Fields                  as T

instance Arbitrary FieldTypes.Timestamp where
  arbitrary = do
    Timestamp ts <- genericArbitrary
    pure $ mkTimestamp $ abs ts
{-
instance {-# OVERLAPPING #-} Arbitrary (ArbitraryMessage T.Timestamp) where
  arbitrary = do
    (s,ns) <- arbitrary
    pure . ArbitraryMessage $
      defMessage & T.seconds .~ abs s
                 & T.nanos .~ abs ns `mod` 1000000000
-}

instance Arbitrary FieldTypes.BlockSizeParams where arbitrary = genericArbitrary
instance Arbitrary FieldTypes.EvidenceParams where arbitrary = genericArbitrary
instance Arbitrary FieldTypes.ValidatorParams where arbitrary = genericArbitrary
instance Arbitrary FieldTypes.ConsensusParams where arbitrary = genericArbitrary
instance Arbitrary FieldTypes.PubKey where arbitrary = genericArbitrary
instance Arbitrary FieldTypes.ValidatorUpdate where arbitrary = genericArbitrary
instance Arbitrary FieldTypes.Validator where arbitrary = genericArbitrary
instance Arbitrary FieldTypes.VoteInfo where arbitrary = genericArbitrary
instance Arbitrary FieldTypes.LastCommitInfo where arbitrary = genericArbitrary
instance Arbitrary FieldTypes.PartSetHeader where arbitrary = genericArbitrary
instance Arbitrary FieldTypes.BlockID where arbitrary = genericArbitrary
instance Arbitrary FieldTypes.Version where arbitrary = genericArbitrary
instance Arbitrary FieldTypes.Header where arbitrary = genericArbitrary
instance Arbitrary FieldTypes.Evidence where arbitrary = genericArbitrary
instance Arbitrary FieldTypes.KVPair where arbitrary = genericArbitrary
instance Arbitrary FieldTypes.Proof where arbitrary = genericArbitrary
instance Arbitrary FieldTypes.ProofOp where arbitrary = genericArbitrary

instance Arbitrary Request.Echo where arbitrary = genericArbitrary
instance Arbitrary Request.Flush where arbitrary = genericArbitrary
instance Arbitrary Request.Info where arbitrary = genericArbitrary
instance Arbitrary Request.SetOption where arbitrary = genericArbitrary
instance Arbitrary Request.InitChain where arbitrary = genericArbitrary
instance Arbitrary Request.Query where arbitrary = genericArbitrary
instance Arbitrary Request.BeginBlock where arbitrary = genericArbitrary
instance Arbitrary Request.CheckTx where arbitrary = genericArbitrary
instance Arbitrary Request.DeliverTx where arbitrary = genericArbitrary
instance Arbitrary Request.EndBlock where arbitrary = genericArbitrary
instance Arbitrary Request.Commit where arbitrary = genericArbitrary


instance Arbitrary Response.Echo where arbitrary = genericArbitrary
instance Arbitrary Response.Flush where arbitrary = genericArbitrary
instance Arbitrary Response.Info where arbitrary = genericArbitrary
instance Arbitrary Response.SetOption where arbitrary = genericArbitrary
instance Arbitrary Response.InitChain where arbitrary = genericArbitrary
instance Arbitrary Response.Query where arbitrary = genericArbitrary
instance Arbitrary Response.BeginBlock where arbitrary = genericArbitrary
instance Arbitrary Response.CheckTx where arbitrary = genericArbitrary
instance Arbitrary Response.DeliverTx where arbitrary = genericArbitrary
instance Arbitrary Response.EndBlock where arbitrary = genericArbitrary
instance Arbitrary Response.Commit where arbitrary = genericArbitrary
instance Arbitrary Response.Exception where arbitrary = genericArbitrary