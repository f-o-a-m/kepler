module Network.ABCI.Types.Messages.FieldTypes where

import           Control.Lens
                                                                                   (iso,
                                                                                   traverse,
                                                                                   (&),
                                                                                   (.~),
                                                                                   (^.),
                                                                                   (^..),
                                                                                   (^?),
                                                                                   _Just)
import           Control.Lens.Wrapped
                                                                                   (Wrapped (..),
                                                                                   _Unwrapped')
import           Data.Aeson
                                                                                   (FromJSON (..),
                                                                                   ToJSON (..),
                                                                                   Value (..),
                                                                                   genericParseJSON,
                                                                                   genericToJSON,
                                                                                   withObject,
                                                                                   (.!=),
                                                                                   (.:),
                                                                                   (.:?))
import           Data.ByteArray.Base64String
                                                                                   (Base64String)
import qualified Data.ByteArray.Base64String                                      as Base64
import           Data.ByteArray.HexString
                                                                                   (HexString)
import qualified Data.ByteArray.HexString                                         as Hex
import           Data.Int
                                                                                   (Int32,
                                                                                   Int64)
import           Data.ProtoLens.Message
                                                                                   (Message (defMessage))
import           Data.Text
                                                                                   (Text,
                                                                                   pack,
                                                                                   unpack)
import           Data.Time.Clock
                                                                                   (DiffTime,
                                                                                   diffTimeToPicoseconds,
                                                                                   picosecondsToDiffTime)
import           Data.Time.Format
                                                                                   (defaultTimeLocale,
                            --                                                       formatTime,
                                                                                   parseTimeOrError)
import           Data.Time.Orphans
                                                                                   ()
import           Data.Word
                                                                                   (Word64)
import           GHC.Generics
                                                                                   (Generic)
import           Network.ABCI.Types.Messages.Common
                                                                                   (defaultABCIOptions)
import qualified Proto.Types                                                      as PT
import qualified Proto.Types_Fields                                               as PT
import qualified Proto.Vendored.Google.Protobuf.Timestamp                         as T
import qualified Proto.Vendored.Google.Protobuf.Timestamp_Fields                  as T
import qualified Proto.Vendored.Tendermint.Tendermint.Crypto.Merkle.Merkle        as MT
import qualified Proto.Vendored.Tendermint.Tendermint.Crypto.Merkle.Merkle_Fields as MT
import qualified Proto.Vendored.Tendermint.Tendermint.Libs.Common.Types           as CT
import qualified Proto.Vendored.Tendermint.Tendermint.Libs.Common.Types_Fields    as CT

newtype WrappedInt64 =
  WrappedInt64 { unwrapInt64 :: Int64 } deriving (Eq, Show, Generic, Num)

instance ToJSON WrappedInt64 where
  toJSON (WrappedInt64 n) = String . pack . show $ n

instance FromJSON WrappedInt64 where
  parseJSON (String t) = pure . WrappedInt64 . read . unpack $ t
  parseJSON a          = WrappedInt64 <$> parseJSON a

newtype WrappedWord64 =
  WrappedWord64 { unwrapWord64 :: Word64 } deriving (Eq, Show, Generic, Num)

instance ToJSON WrappedWord64 where
  toJSON (WrappedWord64 n) = String . pack . show $ n

instance FromJSON WrappedWord64 where
  parseJSON (String t) = pure . WrappedWord64 . read . unpack $ t
  parseJSON a          = WrappedWord64 <$> parseJSON a

-- https://github.com/tendermint/tendermint/blob/v0.32.2/types/block.go#L819~
-- https://github.com/tendermint/tendermint/blob/v0.32.2/types/evidence.go#L278
newtype WrappedEvidenceData = WrappedEvidenceData
  { unwrapEvidenceData :: [Evidence]
  } deriving (Eq, Show, Generic)

instance ToJSON WrappedEvidenceData where
  toJSON (WrappedEvidenceData ls) = toJSONList ls
instance FromJSON WrappedEvidenceData where
  parseJSON = withObject "WrappedEvidenceData" $ \v -> WrappedEvidenceData
    <$> v .:? "evidence" .!= []

-- measured in nanoseconds
data Timestamp =
  Timestamp DiffTime deriving (Eq, Show, Generic)

mkTimestamp :: DiffTime -> Timestamp
mkTimestamp ts =
  let
    ps = diffTimeToPicoseconds ts
    tenToThird = 1000
    nsResolution = (ps `div` tenToThird) * tenToThird
  in
    Timestamp $ picosecondsToDiffTime nsResolution

-- parseTimeOrError @DiffTime True defaultTimeLocale "%FT%T%QZ" "2019-11-05T22:49:05.131739Z"
-- formatTime defaultTimeLocale "%FT%T%QZ" someDiffTime
instance ToJSON Timestamp--  where
  -- toJSON (Timestamp dt) = String . pack . ft $ dt
  --   where ft = formatTime defaultTimeLocale "%FT%T%QZ"

parseDiffTimeOrError :: String -> DiffTime
parseDiffTimeOrError = parseTimeOrError True defaultTimeLocale "%FT%T%QZ"

instance FromJSON Timestamp where
  parseJSON (String t) = pure . mkTimestamp . parseDiffTimeOrError . unpack $ t
  parseJSON a          = mkTimestamp . parseDiffTimeOrError <$> parseJSON a

instance Wrapped Timestamp where
  type Unwrapped Timestamp = T.Timestamp

  _Wrapped' = iso t f
    where
      tenToTwelth = 1000000000000
      tenToThird = 1000
      t (Timestamp ts) =
        let
          ps = diffTimeToPicoseconds ts
          s = ps `div` tenToTwelth
          ns = (ps - s * tenToTwelth) `div` tenToThird
        in
          defMessage & T.seconds .~ fromInteger s
                      & T.nanos .~ fromInteger ns
      f ts =
        let
          ps1 = toInteger (ts ^. T.seconds) * tenToTwelth
          ps2 = toInteger (ts ^. T.nanos) * tenToThird
        in
          mkTimestamp . picosecondsToDiffTime $ ps1 + ps2

data BlockParams = BlockParams
  { blockParamsMaxBytes :: WrappedInt64
  -- ^ Max size of a block, in bytes.
  , blockParamsMaxGas   :: WrappedInt64
  -- ^ Max sum of GasWanted in a proposed block.
  } deriving (Eq, Show, Generic)

instance ToJSON BlockParams where
  toJSON = genericToJSON $ defaultABCIOptions "blockParams"
instance FromJSON BlockParams where
  parseJSON = genericParseJSON $ defaultABCIOptions "blockParams"

instance Wrapped BlockParams where
  type Unwrapped BlockParams = PT.BlockParams

  _Wrapped' = iso t f
    where
      t BlockParams{..} =
        defMessage
          & PT.maxBytes .~ unwrapInt64 blockParamsMaxBytes
          & PT.maxGas .~ unwrapInt64 blockParamsMaxGas
      f a =
        BlockParams
          { blockParamsMaxBytes = WrappedInt64 $ a ^. PT.maxBytes
          , blockParamsMaxGas = WrappedInt64 $ a ^. PT.maxGas
          }

data EvidenceParams = EvidenceParams
  { evidenceParamsMaxAge :: WrappedInt64
  -- ^ Max age of evidence, in blocks.
  } deriving (Eq, Show, Generic)

instance ToJSON EvidenceParams where
  toJSON = genericToJSON $ defaultABCIOptions "evidenceParams"
instance FromJSON EvidenceParams where
  parseJSON = genericParseJSON $ defaultABCIOptions "evidenceParams"

instance Wrapped EvidenceParams where
  type Unwrapped EvidenceParams = PT.EvidenceParams

  _Wrapped' = iso t f
    where
      t EvidenceParams{..} =
        defMessage
          & PT.maxAge .~ unwrapInt64 evidenceParamsMaxAge
      f a =
        EvidenceParams
          { evidenceParamsMaxAge = WrappedInt64 $ a ^. PT.maxAge
          }

data ValidatorParams = ValidatorParams
  { validatorParamsPubKeyTypes :: [Text]
  -- ^ List of accepted pubkey types
  } deriving (Eq, Show, Generic)

instance ToJSON ValidatorParams where
  toJSON = genericToJSON $ defaultABCIOptions "validatorParams"
instance FromJSON ValidatorParams where
  parseJSON = withObject "ValidatorParams" $ \v -> ValidatorParams
    <$> v .:? "pubKeyTypes" .!= []

instance Wrapped ValidatorParams where
  type Unwrapped ValidatorParams = PT.ValidatorParams

  _Wrapped' = iso t f
    where
      t ValidatorParams{..} =
        defMessage
          & PT.pubKeyTypes .~ validatorParamsPubKeyTypes
      f a =
        ValidatorParams
          { validatorParamsPubKeyTypes = a ^. PT.pubKeyTypes
          }

data ConsensusParams = ConsensusParams
  { consensusParamsBlockSize :: Maybe BlockParams
  --  ^ Parameters limiting the size of a block and time between consecutive blocks.
  , consensusParamsEvidence  :: Maybe EvidenceParams
  -- ^ Parameters limiting the validity of evidence of byzantine behaviour.
  , consensusParamsValidator :: Maybe ValidatorParams
  -- ^ Parameters limitng the types of pubkeys validators can use.
  } deriving (Eq, Show, Generic)

instance ToJSON ConsensusParams where
  toJSON = genericToJSON $ defaultABCIOptions "consensusParams"
instance FromJSON ConsensusParams where
  parseJSON = genericParseJSON $ defaultABCIOptions "consensusParams"

instance Wrapped ConsensusParams where
  type Unwrapped ConsensusParams =  PT.ConsensusParams

  _Wrapped' = iso t f
    where
      t ConsensusParams{..} =
        defMessage
          & PT.maybe'block .~ consensusParamsBlockSize ^? _Just . _Wrapped'
          & PT.maybe'evidence .~ consensusParamsEvidence ^? _Just . _Wrapped'
          & PT.maybe'validator .~ consensusParamsValidator ^? _Just . _Wrapped'
      f a =
        ConsensusParams
          { consensusParamsBlockSize = a ^? PT.maybe'block . _Just . _Unwrapped'
          , consensusParamsEvidence =  a ^? PT.maybe'evidence . _Just . _Unwrapped'
          , consensusParamsValidator =  a ^? PT.maybe'validator . _Just . _Unwrapped'
          }

data PubKey = PubKey
  { pubKeyType :: Text
  -- ^ Type of the public key.
  , pubKeyData :: Base64String
  -- ^ Public key data.
  } deriving (Eq, Show, Generic)

instance ToJSON PubKey where
  toJSON = genericToJSON $ defaultABCIOptions "pubKey"
instance FromJSON PubKey where
  parseJSON = genericParseJSON $ defaultABCIOptions "pubKey"

instance Wrapped PubKey where
  type Unwrapped PubKey = PT.PubKey

  _Wrapped' = iso t f
    where
      t PubKey{..} =
        defMessage
          & PT.type' .~ pubKeyType
          & PT.data' .~ Base64.toBytes pubKeyData
      f a =
        PubKey
          { pubKeyType =  a ^. PT.type'
          , pubKeyData = Base64.fromBytes (a ^. PT.data')
          }

data ValidatorUpdate = ValidatorUpdate
  { validatorUpdatePubKey :: Maybe PubKey
  -- ^ Public key of the validator
  , validatorUpdatePower  :: WrappedInt64
  -- ^ Voting power of the validator
  } deriving (Eq, Show, Generic)

instance ToJSON ValidatorUpdate where
  toJSON = genericToJSON $ defaultABCIOptions "validatorUpdate"
instance FromJSON ValidatorUpdate where
  parseJSON = genericParseJSON $ defaultABCIOptions "validatorUpdate"

instance Wrapped ValidatorUpdate where
  type Unwrapped ValidatorUpdate = PT.ValidatorUpdate

  _Wrapped' = iso t f
    where
      t ValidatorUpdate{..} =
        defMessage
          & PT.maybe'pubKey .~ validatorUpdatePubKey ^? _Just . _Wrapped'
          & PT.power .~ unwrapInt64 validatorUpdatePower
      f a =
        ValidatorUpdate
          { validatorUpdatePubKey = a ^? PT.maybe'pubKey . _Just . _Unwrapped'
          , validatorUpdatePower = WrappedInt64 $ a ^. PT.power
          }

data Validator = Validator
  { validatorAddress :: HexString
  -- ^ Address of the validator (hash of the public key)
  , validatorPower   :: WrappedInt64
  -- ^ Voting power of the validator
  } deriving (Eq, Show, Generic)

instance ToJSON Validator where
  toJSON = genericToJSON $ defaultABCIOptions "validator"
instance FromJSON Validator where
  parseJSON = genericParseJSON $ defaultABCIOptions "validator"

instance Wrapped Validator where
  type Unwrapped Validator = PT.Validator

  _Wrapped' = iso t f
    where
      t Validator{..} =
        defMessage
          & PT.address .~ Hex.toBytes validatorAddress
          & PT.power .~ unwrapInt64 validatorPower
      f a =
        Validator
          { validatorAddress = Hex.fromBytes (a ^. PT.address)
          , validatorPower = WrappedInt64 $ a ^. PT.power
          }

data VoteInfo = VoteInfo
  { voteInfoValidator       :: Maybe Validator
  -- ^ A validator
  , voteInfoSignedLastBlock :: Bool
  -- ^ Indicates whether or not the validator signed the last block
  } deriving (Eq, Show, Generic)

instance ToJSON VoteInfo where
  toJSON = genericToJSON $ defaultABCIOptions "voteInfo"
instance FromJSON VoteInfo where
  parseJSON = genericParseJSON $ defaultABCIOptions "voteInfo"

instance Wrapped VoteInfo where
  type Unwrapped VoteInfo = PT.VoteInfo

  _Wrapped' = iso t f
    where
      t VoteInfo{..} =
        defMessage
          & PT.maybe'validator .~ voteInfoValidator ^? _Just . _Wrapped'
          & PT.signedLastBlock .~ voteInfoSignedLastBlock
      f voteInfo =
        VoteInfo
          { voteInfoValidator = voteInfo ^? PT.maybe'validator . _Just . _Unwrapped'
          , voteInfoSignedLastBlock = voteInfo ^. PT.signedLastBlock
          }

data LastCommitInfo = LastCommitInfo
  { lastCommitInfoRound :: Int32
  -- ^ Commit round.
  , lastCommitInfoVotes :: [VoteInfo]
  -- ^ List of validators addresses in the last validator set with their voting
  -- power and whether or not they signed a vote.
  } deriving (Eq, Show, Generic)

instance ToJSON LastCommitInfo where
  toJSON = genericToJSON $ defaultABCIOptions "lastCommitInfo"
instance FromJSON LastCommitInfo where
  parseJSON = withObject "LastCommitInfo" $ \v -> LastCommitInfo
    <$> v .: "infoRound"
    <*> v .:? "infoVotes" .!= []

instance Wrapped LastCommitInfo where
  type Unwrapped LastCommitInfo = PT.LastCommitInfo

  _Wrapped' = iso t f
    where
      t LastCommitInfo{..} =
        defMessage
          & PT.round .~ lastCommitInfoRound
          & PT.votes .~ lastCommitInfoVotes ^.. traverse . _Wrapped'
      f a =
        LastCommitInfo
          { lastCommitInfoRound = a ^. PT.round
          , lastCommitInfoVotes = a ^.. PT.votes . traverse . _Unwrapped'
          }

data PartSetHeader = PartSetHeader
  { partSetHeaderTotal :: Int32
  -- ^ total number of pieces in a PartSet
  , partSetHeaderHash  :: HexString
  -- ^ Merkle root hash of those pieces
  } deriving (Eq, Show, Generic)

instance ToJSON PartSetHeader where
  toJSON = genericToJSON $ defaultABCIOptions "partSetHeader"
instance FromJSON PartSetHeader where
  parseJSON = genericParseJSON $ defaultABCIOptions "partSetHeader"

instance Wrapped PartSetHeader where
  type Unwrapped PartSetHeader = PT.PartSetHeader

  _Wrapped' = iso t f
    where
      t PartSetHeader{..} =
        defMessage
          & PT.total .~ partSetHeaderTotal
          & PT.hash .~ Hex.toBytes partSetHeaderHash
      f a =
        PartSetHeader { partSetHeaderTotal = a ^. PT.total
                      , partSetHeaderHash = Hex.fromBytes (a ^. PT.hash)
                      }

data BlockID = BlockID
  { blockIDHash        :: HexString
  -- ^ Hash of the block header
  , blockIDPartsHeader :: Maybe PartSetHeader
  -- ^ PartSetHeader (used internally, for clients this is basically opaque).
  } deriving (Eq, Show, Generic)

instance ToJSON BlockID where
  toJSON = genericToJSON $ defaultABCIOptions "blockID"
instance FromJSON BlockID where
  parseJSON = genericParseJSON $ defaultABCIOptions "blockID"

instance Wrapped BlockID where
  type Unwrapped BlockID = PT.BlockID

  _Wrapped' = iso t f
    where
      t BlockID{..} =
        defMessage
          & PT.hash .~ Hex.toBytes blockIDHash
          & PT.maybe'partsHeader .~ blockIDPartsHeader ^? _Just . _Wrapped'
      f a =
        BlockID
          { blockIDHash = Hex.fromBytes(a ^. PT.hash)
          , blockIDPartsHeader = a ^? PT.maybe'partsHeader . _Just . _Unwrapped'
          }

data Version = Version
  { versionBlock :: WrappedWord64
  -- ^ Protocol version of the blockchain data structures.
  , versionApp   :: WrappedWord64
  -- ^ Protocol version of the application.
  } deriving (Eq, Show, Generic)

instance ToJSON Version where
  toJSON = genericToJSON $ defaultABCIOptions "version"
instance FromJSON Version where
  parseJSON = genericParseJSON $ defaultABCIOptions "version"

instance Wrapped Version where
  type Unwrapped Version = PT.Version

  _Wrapped' = iso t f
    where
      t Version{..} =
        defMessage
          & PT.block .~ unwrapWord64 versionBlock
          & PT.app .~ unwrapWord64 versionApp
      f a =
        Version
          { versionBlock = WrappedWord64 $ a ^. PT.block
          , versionApp = WrappedWord64 $ a ^. PT.app
          }

data Header = Header
  { headerVersion            :: Maybe Version
  -- ^ Version of the blockchain and the application
  , headerChainId            :: Text
  -- ^ ID of the blockchain
  , headerHeight             :: WrappedInt64
  -- ^ Height of the block in the chain
  , headerTime               :: Maybe Timestamp
  -- ^ Time of the previous block
  , headerNumTxs             :: WrappedInt64
  -- ^ Number of transactions in the block
  , headerTotalTxs           :: WrappedInt64
  -- ^ Total number of transactions in the blockchain until now
  , headerLastBlockId        :: Maybe BlockID
  -- ^ Hash of the previous (parent) block
  , headerLastCommitHash     :: HexString
  -- ^ Hash of the previous block's commit
  , headerDataHash           :: HexString
  -- ^ Hash of the validator set for this block
  , headerValidatorsHash     :: HexString
  -- ^ Hash of the validator set for the next block
  , headerNextValidatorsHash :: HexString
  -- ^ Hash of the consensus parameters for this block
  , headerConsensusHash      :: HexString
  -- ^ Hash of the consensus parameters for this block
  , headerAppHash            :: HexString
  -- ^ Data returned by the last call to Commit
  , headerLastResultsHash    :: HexString
  -- ^ Hash of the ABCI results returned by the last block
  , headerEvidenceHash       :: HexString
  -- ^ Hash of the evidence included in this block
  , headerProposerAddress    :: HexString
  -- ^ Original proposer for the block
  } deriving (Eq, Show, Generic)

instance ToJSON Header where
  toJSON = genericToJSON $ defaultABCIOptions "header"
instance FromJSON Header where
  parseJSON = genericParseJSON $ defaultABCIOptions "header"

instance Wrapped Header where
  type Unwrapped Header = PT.Header

  _Wrapped' = iso t f
    where
      t Header{..} =
        defMessage
          & PT.maybe'version .~ headerVersion ^? _Just . _Wrapped'
          & PT.chainId .~ headerChainId
          & PT.height .~ unwrapInt64 headerHeight
          & PT.maybe'time .~ headerTime ^? _Just . _Wrapped'
          & PT.numTxs .~ unwrapInt64 headerNumTxs
          & PT.totalTxs .~ unwrapInt64 headerTotalTxs
          & PT.maybe'lastBlockId .~ headerLastBlockId ^? _Just . _Wrapped'
          & PT.lastCommitHash .~ Hex.toBytes headerLastCommitHash
          & PT.dataHash .~ Hex.toBytes headerDataHash
          & PT.validatorsHash .~ Hex.toBytes headerValidatorsHash
          & PT.nextValidatorsHash .~ Hex.toBytes headerNextValidatorsHash
          & PT.consensusHash .~ Hex.toBytes headerConsensusHash
          & PT.appHash .~ Hex.toBytes headerAppHash
          & PT.lastResultsHash .~ Hex.toBytes headerLastResultsHash
          & PT.evidenceHash .~ Hex.toBytes headerEvidenceHash
          & PT.proposerAddress .~ Hex.toBytes headerProposerAddress
      f a =
        Header
          { headerVersion = a ^? PT.maybe'version . _Just . _Unwrapped'
          , headerChainId = a ^. PT.chainId
          , headerHeight = WrappedInt64 $ a ^. PT.height
          , headerTime = a ^? PT.maybe'time . _Just . _Unwrapped'
          , headerNumTxs = WrappedInt64 $ a ^. PT.numTxs
          , headerTotalTxs = WrappedInt64 $ a ^. PT.totalTxs
          , headerLastBlockId = a ^? PT.maybe'lastBlockId . _Just . _Unwrapped'
          , headerLastCommitHash = Hex.fromBytes $ a ^. PT.lastCommitHash
          , headerDataHash = Hex.fromBytes $ a ^. PT.dataHash
          , headerValidatorsHash = Hex.fromBytes $ a ^. PT.validatorsHash
          , headerNextValidatorsHash = Hex.fromBytes $ a ^. PT.nextValidatorsHash
          , headerConsensusHash = Hex.fromBytes $ a ^. PT.consensusHash
          , headerAppHash = Hex.fromBytes $ a ^. PT.appHash
          , headerLastResultsHash = Hex.fromBytes $ a ^. PT.lastResultsHash
          , headerEvidenceHash = Hex.fromBytes $ a ^. PT.evidenceHash
          , headerProposerAddress = Hex.fromBytes $ a ^. PT.proposerAddress
          }

data Evidence = Evidence
  { evidenceType             :: Text
  -- ^ Type of the evidence.
  , evidenceValidator        :: Maybe Validator
  -- ^ The offending validator
  , evidenceHeight           :: WrappedInt64
  -- ^ Height when the offense was committed
  , evidenceTime             :: Maybe Timestamp
  -- ^ Time of the block at height Height.
  , evidenceTotalVotingPower :: WrappedInt64
  -- ^ Total voting power of the validator set at height Height
  } deriving (Eq, Show, Generic)

instance ToJSON Evidence where
  toJSON = genericToJSON $ defaultABCIOptions "evidence"
instance FromJSON Evidence where
  parseJSON = genericParseJSON $ defaultABCIOptions "evidence"

instance Wrapped Evidence where
  type Unwrapped Evidence = PT.Evidence

  _Wrapped' = iso t f
    where
      t Evidence{..} =
        defMessage
          & PT.type' .~ evidenceType
          & PT.maybe'validator .~ evidenceValidator ^? _Just . _Wrapped'
          & PT.height .~ unwrapInt64 evidenceHeight
          & PT.maybe'time .~ evidenceTime ^? _Just . _Wrapped'
          & PT.totalVotingPower .~ unwrapInt64 evidenceTotalVotingPower
      f a =
        Evidence
          { evidenceType = a ^. PT.type'
          , evidenceValidator = a ^? PT.maybe'validator . _Just . _Unwrapped'
          , evidenceHeight = WrappedInt64 $ a ^. PT.height
          , evidenceTime = a ^? PT.maybe'time . _Just . _Unwrapped'
          , evidenceTotalVotingPower = WrappedInt64 $ a ^. PT.totalVotingPower
          }

data KVPair = KVPair
  { kVPairKey   :: Base64String
  -- ^ key
  , kVPairValue :: Base64String
  -- ^ value
  } deriving (Eq, Show, Generic)

instance ToJSON KVPair where
  toJSON = genericToJSON $ defaultABCIOptions "kVPair"
instance FromJSON KVPair where
  parseJSON = genericParseJSON $ defaultABCIOptions "kVPair"

instance Wrapped KVPair where
  type Unwrapped KVPair = CT.KVPair

  _Wrapped' = iso t f
    where
      t KVPair{..} =
        defMessage
          & CT.key .~ Base64.toBytes kVPairKey
          & CT.value .~ Base64.toBytes kVPairValue
      f a =
        KVPair
          { kVPairKey = Base64.fromBytes $ a ^. CT.key
          , kVPairValue = Base64.fromBytes $ a ^. CT.value
          }

data Proof = Proof
  { proofOps :: [ProofOp]
  -- ^ List of chained Merkle proofs, of possibly different types
  } deriving (Eq, Show, Generic)

instance ToJSON Proof where
  toJSON = genericToJSON $ defaultABCIOptions "proof"
instance FromJSON Proof where
  parseJSON = withObject "Proof" $ \v -> Proof
    <$> v .:? "ops" .!= []

instance Wrapped Proof where
  type Unwrapped Proof = MT.Proof

  _Wrapped' = iso t f
    where
      t Proof{..} =
        defMessage
          & MT.ops .~ proofOps ^.. traverse . _Wrapped'
      f a =
        Proof
          { proofOps = a ^.. MT.ops . traverse . _Unwrapped'
          }


data ProofOp = ProofOp
  { proofOpType :: Text
  -- ^ Type of Merkle proof and how it's encoded.
  , proofOpKey  :: Base64String
  -- ^ Key in the Merkle tree that this proof is for.
  , proofOpData :: Base64String
  -- ^ Encoded Merkle proof for the key.
  } deriving (Eq, Show, Generic)

instance ToJSON ProofOp where
  toJSON = genericToJSON $ defaultABCIOptions "proofOp"
instance FromJSON ProofOp where
  parseJSON = genericParseJSON $ defaultABCIOptions "proofOp"

instance Wrapped ProofOp where
  type Unwrapped ProofOp = MT.ProofOp

  _Wrapped' = iso t f
    where
      t ProofOp{..} =
        defMessage
          & MT.type' .~ proofOpType
          & MT.key .~ Base64.toBytes proofOpKey
          & MT.data' .~ Base64.toBytes proofOpData
      f a =
        ProofOp
          { proofOpType = a ^. MT.type'
          , proofOpKey = Base64.fromBytes $ a ^. MT.key
          , proofOpData = Base64.fromBytes $ a ^. MT.data'
          }

data Event = Event
  { eventType       :: Text
  -- ^ Type of Event
  , eventAttributes :: [KVPair]
  -- ^ Event attributes
  } deriving (Eq, Show, Generic)

instance ToJSON Event where
  toJSON = genericToJSON $ defaultABCIOptions "event"
instance FromJSON Event where
  parseJSON = withObject "Event" $ \v -> Event
    <$> v .: "type"
    <*> v .:? "attributes" .!= []

instance Wrapped Event where
  type Unwrapped Event = PT.Event

  _Wrapped' = iso t f
    where
      t Event{..} =
        defMessage
          & PT.type' .~ eventType
          & PT.attributes .~ eventAttributes ^.. traverse . _Wrapped'
      f a =
        Event
          { eventType = a ^. PT.type'
          , eventAttributes = a ^.. PT.attributes . traverse . _Unwrapped'
          }
