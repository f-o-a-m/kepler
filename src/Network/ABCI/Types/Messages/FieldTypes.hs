module Network.ABCI.Types.Messages.FieldTypes where

import           Control.Lens
                                                                                   (Iso',
                                                                                   iso,
                                                                                   mapped,
                                                                                   over,
                                                                                   traverse,
                                                                                   view,
                                                                                   (%~),
                                                                                   (&),
                                                                                   (.~),
                                                                                   (^.),
                                                                                   (^..),
                                                                                   (^?),
                                                                                   _Just)
import qualified Control.Lens                                                     as Lens
import           Data.ByteString
                                                                                   (ByteString)
import           Data.Int
                                                                                   (Int32,
                                                                                   Int64)
import           Data.ProtoLens.Message
                                                                                   (Message (defMessage))
import           Data.Text
                                                                                   (Text)
import           Data.Time.Clock
                                                                                   (DiffTime,
                                                                                   diffTimeToPicoseconds,
                                                                                   picosecondsToDiffTime)
import           Data.Word
                                                                                   (Word64)
import           GHC.Generics
                                                                                   (Generic)
import qualified Proto.Types                                                      as PT
import qualified Proto.Types_Fields                                               as PT
import qualified Proto.Vendored.Google.Protobuf.Timestamp                         as T
import qualified Proto.Vendored.Google.Protobuf.Timestamp_Fields                  as T
import qualified Proto.Vendored.Tendermint.Tendermint.Crypto.Merkle.Merkle        as MT
import qualified Proto.Vendored.Tendermint.Tendermint.Crypto.Merkle.Merkle_Fields as MT
import qualified Proto.Vendored.Tendermint.Tendermint.Libs.Common.Types           as CT
import qualified Proto.Vendored.Tendermint.Tendermint.Libs.Common.Types_Fields    as CT

data Timestamp =
  Timestamp DiffTime deriving (Eq, Show, Generic)

timestamp :: Iso' Timestamp T.Timestamp
timestamp = iso t f
  where
    tenToTwelth = 1000000000000
    tenToThird = 1000
    t (Timestamp t) =
      let ps = diffTimeToPicoseconds t
          s = ps `div` tenToTwelth
          ns = (ps - s * tenToTwelth) `div` tenToThird
      in defMessage & T.seconds .~ fromInteger s
                    & T.nanos .~ fromInteger ns
    f ts =
      let ps1 = toInteger (ts ^. T.seconds) * tenToTwelth
          ps2 = toInteger (ts ^. T.nanos) * tenToThird
      in Timestamp . picosecondsToDiffTime $ ps1 + ps2

data BlockSizeParams = BlockSizeParams
  { blockSizeParamsMaxBytes :: Int64
  -- ^ Max size of a block, in bytes.
  , blockSizeParamsMaxGas   :: Int64
  -- ^ Max sum of GasWanted in a proposed block.
  } deriving (Eq, Show, Generic)

blockSizeParams :: Iso' BlockSizeParams PT.BlockSizeParams
blockSizeParams = iso t f
  where
    t BlockSizeParams{..} =
      defMessage & PT.maxBytes .~ blockSizeParamsMaxBytes
                 & PT.maxGas .~ blockSizeParamsMaxGas
    f a =
      BlockSizeParams
        { blockSizeParamsMaxBytes = a ^. PT.maxBytes
        , blockSizeParamsMaxGas = a ^. PT.maxGas
        }

data EvidenceParams = EvidenceParams
  { evidenceParamsMaxAge :: Int64
  -- ^ Max age of evidence, in blocks.
  } deriving (Eq, Show, Generic)

evidenceParams :: Iso' EvidenceParams PT.EvidenceParams
evidenceParams = iso t f
  where
    t EvidenceParams{..} =
      defMessage & PT.maxAge .~ evidenceParamsMaxAge
    f a =
      EvidenceParams
        { evidenceParamsMaxAge = a ^. PT.maxAge
        }

data ValidatorParams = ValidatorParams
  { validatorParamsPubKeyTypes :: [Text]
  -- ^ List of accepted pubkey types
  } deriving (Eq, Show, Generic)

validatorParams :: Iso' ValidatorParams PT.ValidatorParams
validatorParams = iso t f
  where
    t ValidatorParams{..} =
      defMessage & PT.pubKeyTypes .~ validatorParamsPubKeyTypes
    f a =
      ValidatorParams
        { validatorParamsPubKeyTypes = a ^. PT.pubKeyTypes
        }

data ConsensusParams = ConsensusParams
  { consensusParamsBlockSize :: Maybe BlockSizeParams
  --  ^ Parameters limiting the size of a block and time between consecutive blocks.
  , consensusParamsEvidence  :: Maybe EvidenceParams
  -- ^ Parameters limiting the validity of evidence of byzantine behaviour.
  , consensusParamsValidator :: Maybe ValidatorParams
  -- ^ Parameters limitng the types of pubkeys validators can use.
  } deriving (Eq, Show, Generic)

consensusParams :: Iso' ConsensusParams PT.ConsensusParams
consensusParams = iso t f
  where
    t ConsensusParams{..} =
      defMessage & PT.maybe'blockSize .~ consensusParamsBlockSize ^? _Just . blockSizeParams
                 & PT.maybe'evidence .~ consensusParamsEvidence ^? _Just . evidenceParams
                 & PT.maybe'validator .~ consensusParamsValidator ^? _Just . validatorParams
    f a =
      ConsensusParams
        { consensusParamsBlockSize = a ^? PT.maybe'blockSize . _Just . Lens.from blockSizeParams
        , consensusParamsEvidence =  a ^? PT.maybe'evidence . _Just . Lens.from evidenceParams
        , consensusParamsValidator =  a ^? PT.maybe'validator . _Just . Lens.from validatorParams
        }

data PubKey = PubKey
  { pubKeyType :: Text
  -- ^ Type of the public key.
  , pubKeyData :: ByteString
  -- ^ Public key data.
  } deriving (Eq, Show, Generic)

pubKey :: Iso' PubKey PT.PubKey
pubKey = iso t f
  where
    t PubKey{..} = defMessage & PT.type' .~ pubKeyType
                               & PT.data' .~ pubKeyData

    f a =
      PubKey
        { pubKeyType = a ^. PT.type'
        , pubKeyData = a ^. PT.data'
        }

data ValidatorUpdate = ValidatorUpdate
  { validatorUpdatePubKey :: Maybe PubKey
  -- ^ Public key of the validator
  , validatorUpdatePower  :: Int64
  -- ^ Voting power of the validator
  } deriving (Eq, Show, Generic)

validatorUpdate :: Iso' ValidatorUpdate PT.ValidatorUpdate
validatorUpdate = iso t f
  where
    t ValidatorUpdate{..} =
      defMessage & PT.maybe'pubKey .~ validatorUpdatePubKey ^? _Just . pubKey
                 & PT.power .~ validatorUpdatePower
    f a =
      ValidatorUpdate
        { validatorUpdatePubKey = a ^? PT.maybe'pubKey . _Just . Lens.from pubKey
        , validatorUpdatePower = a ^. PT.power
        }

data Validator = Validator
  { validatorAddress :: ByteString
  -- ^ Address of the validator (hash of the public key)
  , validatorPower   :: Int64
  -- ^ Voting power of the validator
  } deriving (Eq, Show, Generic)

validator :: Iso' Validator PT.Validator
validator = iso t f
  where
    t Validator{..} =
      defMessage & PT.address .~ validatorAddress
                 & PT.power .~ validatorPower
    f a =
      Validator
        { validatorAddress = a ^. PT.address
        , validatorPower = a ^. PT.power
        }

data VoteInfo = VoteInfo
  { voteInfoValidator       :: Maybe Validator
  -- ^ A validator
  , voteInfoSignedLastBlock :: Bool
  -- ^ Indicates whether or not the validator signed the last block
  } deriving (Eq, Show, Generic)

voteInfo :: Iso' VoteInfo PT.VoteInfo
voteInfo = iso t f
  where
    t VoteInfo{..} =
      defMessage & PT.maybe'validator .~ voteInfoValidator ^? _Just . validator
                 & PT.signedLastBlock .~ voteInfoSignedLastBlock
    f voteInfo =
      VoteInfo
        { voteInfoValidator = voteInfo ^? PT.maybe'validator . _Just . Lens.from validator
        , voteInfoSignedLastBlock = voteInfo ^. PT.signedLastBlock
        }

data LastCommitInfo = LastCommitInfo
  { lastCommitInfoRound :: Int32
  -- ^ Commit round.
  , lastCommitInfoVotes :: [VoteInfo]
  -- ^ List of validators addresses in the last validator set with their voting
  -- power and whether or not they signed a vote.
  } deriving (Eq, Show, Generic)

lastCommitInfo :: Iso' LastCommitInfo PT.LastCommitInfo
lastCommitInfo = iso t f
  where
    t LastCommitInfo{..} =
      defMessage & PT.round .~ lastCommitInfoRound
                 & PT.votes .~ lastCommitInfoVotes ^.. traverse . voteInfo
    f a =
      LastCommitInfo
        { lastCommitInfoRound = a ^. PT.round
        , lastCommitInfoVotes = a ^.. PT.votes . traverse . Lens.from voteInfo
        }

data PartSetHeader = PartSetHeader
  { partSetHeaderTotal :: Int32
  -- ^ total number of pieces in a PartSet
  , partSetHeaderHash  :: ByteString
  -- ^ Merkle root hash of those pieces
  } deriving (Eq, Show, Generic)

partSetHeader :: Iso' PartSetHeader PT.PartSetHeader
partSetHeader = iso t f
  where
    t PartSetHeader{..} =
      defMessage & PT.total .~ partSetHeaderTotal
                 & PT.hash .~ partSetHeaderHash
    f a =
      PartSetHeader { partSetHeaderTotal = a ^. PT.total
                    , partSetHeaderHash = a ^. PT.hash
                    }

data BlockID = BlockID
  { blockIDHash        :: ByteString
  -- ^ Hash of the block header
  , blockIDPartsHeader :: Maybe PartSetHeader
  -- ^ PartSetHeader (used internally, for clients this is basically opaque).
  } deriving (Eq, Show, Generic)

blockID :: Iso' BlockID PT.BlockID
blockID = iso t f
  where
    t BlockID{..} =
      defMessage & PT.hash .~ blockIDHash
                 & PT.maybe'partsHeader .~ blockIDPartsHeader ^? _Just . partSetHeader
    f a =
      BlockID
        { blockIDHash = a ^. PT.hash
        , blockIDPartsHeader = a ^? PT.maybe'partsHeader . _Just . Lens.from partSetHeader
        }

data Version = Version
  { versionBlock :: Word64
  -- ^ Protocol version of the blockchain data structures.
  , versionApp   :: Word64
  -- ^ Protocol version of the application.
  } deriving (Eq, Show, Generic)

version :: Iso' Version PT.Version
version = iso t f
  where
    t Version{..} =
      defMessage & PT.block .~ versionBlock
                 & PT.app .~ versionApp
    f version =
      Version
        { versionBlock = a ^. PT.block
        , versionApp = a ^. PT.app
        }

data Header = Header
  { headerVersion            :: Maybe Version
  -- ^ Version of the blockchain and the application
  , headerChainId            :: Text
  -- ^ ID of the blockchain
  , headerHeight             :: Int64
  -- ^ Height of the block in the chain
  , headerTime               :: Maybe Timestamp
  -- ^ Time of the previous block
  , headerNumTxs             :: Int64
  -- ^ Number of transactions in the block
  , headerTotalTxs           :: Int64
  -- ^ Total number of transactions in the blockchain until now
  , headerLastBlockId        :: BlockID
  -- ^ Hash of the previous (parent) block
  , headerLastCommitHash     :: ByteString
  -- ^ Hash of the previous block's commit
  , headerDataHash           :: ByteString
  -- ^ Hash of the validator set for this block
  , headerValidatorsHash     :: ByteString
  -- ^ Hash of the validator set for the next block
  , headerNextValidatorsHash :: ByteString
  -- ^ Hash of the consensus parameters for this block
  , headerConsensusHash      :: ByteString
  -- ^ Hash of the consensus parameters for this block
  , headerAppHash            :: ByteString
  -- ^ Data returned by the last call to Commit
  , headerLastResultsHash    :: ByteString
  -- ^ Hash of the ABCI results returned by the last block
  , headerEvidenceHash       :: ByteString
  -- ^ Hash of the evidence included in this block
  , headerProposerAddress    :: ByteString
  -- ^ Original proposer for the block
  } deriving (Eq, Show, Generic)


header :: Iso' Header PT.Header
header = iso t f
  where
    t Header{..} =
      defMessage & PT.maybe'version .~ headerVersion ^? _Just . version
                 & PT.chainId .~ headerChainId
                 & PT.height .~ headerHeight
                 & PT.maybe'time .~ headerTime ^? _Just . timestamp
                 & PT.numTxs .~ headerNumTxs
                 & PT.totalTxs .~ headerTotalTxs
                 & PT.lastBlockId .~ headerLastBlockId ^. blockID
                 & PT.lastCommitHash .~ headerLastCommitHash
                 & PT.dataHash .~ headerDataHash
                 & PT.validatorsHash .~ headerValidatorsHash
                 & PT.nextValidatorsHash .~ headerNextValidatorsHash
                 & PT.consensusHash .~ headerConsensusHash
                 & PT.appHash .~ headerAppHash
                 & PT.lastResultsHash .~ headerLastResultsHash
                 & PT.evidenceHash .~ headerEvidenceHash
                 & PT.proposerAddress .~ headerProposerAddress
    f a =
      Header
        { headerVersion = a ^? PT.maybe'version . _Just . Lens.from version
        , headerChainId = a ^. PT.chainId
        , headerHeight = a ^. PT.height
        , headerTime = a ^? PT.maybe'time . _Just . Lens.from timestamp
        , headerNumTxs = a ^. PT.numTxs
        , headerTotalTxs = a ^. PT.totalTxs
        , headerLastBlockId = a ^. PT.lastBlockId. Lens.from blockID
        , headerLastCommitHash = a ^. PT.lastCommitHash
        , headerDataHash = a ^. PT.dataHash
        , headerValidatorsHash = a ^. PT.validatorsHash
        , headerNextValidatorsHash = a ^. PT.nextValidatorsHash
        , headerConsensusHash = a ^. PT.consensusHash
        , headerAppHash = a ^. PT.appHash
        , headerLastResultsHash = a ^. PT.lastResultsHash
        , headerEvidenceHash = a ^. PT.evidenceHash
        , headerProposerAddress = a ^. PT.proposerAddress
        }

data Evidence = Evidence
  { evidenceType             :: Text
  -- ^ Type of the evidence.
  , evidenceValidator        :: Maybe Validator
  -- ^ The offending validator
  , evidenceHeight           :: Int64
  -- ^ Height when the offense was committed
  , evidenceTime             :: Maybe Timestamp
  -- ^ Time of the block at height Height.
  , evidenceTotalVotingPower :: Int64
  -- ^ Total voting power of the validator set at height Height
  } deriving (Eq, Show, Generic)

evidence :: Iso' Evidence PT.Evidence
evidence = iso t f
  where
    to Evidence{..} =
      defMessage & PT.type' .~ evidenceType
                 & PT.maybe'validator .~ evidenceValidator ^? _Just . validator
                 & PT.height .~ evidenceHeight
                 & PT.maybe'time .~ evidenceTime ^? _Just . timestamp
                 & PT.totalVotingPower .~ evidenceTotalVotingPower
    from a =
      Evidence
        { evidenceType = a ^. PT.type'
        , evidenceValidator = a ^? PT.maybe'validator . _Just . Lens.from validator
        , evidenceHeight = a ^. PT.height
        , evidenceTime = a ^? PT.maybe'time . _Just . Lens.from timestamp
        , evidenceTotalVotingPower = a ^. PT.totalVotingPower
        }


data KVPair = KVPair
  { kVPairKey   :: ByteString
  -- ^ key
  , kVPairValue :: ByteString
  -- ^ value
  } deriving (Eq, Show, Generic)

kVPair :: Iso' KVPair CT.KVPair
kVPair = iso t f
  where
    t KVPair{..} =
      defMessage
        & CT.key .~ kVPairKey
        & CT.value .~ kVPairValue
    f a =
      KVPair
        { kVPairKey = a ^. CT.key
        , kVPairValue = a ^. CT.value
        }


data Proof = Proof
  { proofOps :: [ProofOp]
  -- ^ List of chained Merkle proofs, of possibly different types
  } deriving (Eq, Show, Generic)

proof :: Iso' Proof MT.Proof
proof = iso t f
  where
    to Proof{..} =
      defMessage
        & MT.ops .~ proofOps ^.. traverse . proofOp
    f a =
      Proof
        { proofOps = a ^.. MT.ops . traverse . Lens.from proofOp
        }


data ProofOp = ProofOp
  { proofOpType :: Text
  -- ^ Type of Merkle proof and how it's encoded.
  , proofOpKey  :: ByteString
  -- ^ Key in the Merkle tree that this proof is for.
  , proofOpData :: ByteString
  -- ^ Encoded Merkle proof for the key.
  } deriving (Eq, Show, Generic)

proofOp :: Iso' ProofOp MT.ProofOp
proofOp = iso t f
  where
    to ProofOp{..} =
      defMessage
        & MT.type' .~ proofOpType
        & MT.key .~ proofOpKey
        & MT.data' .~ proofOpData
    from a =
      ProofOp
        { proofOpType = a ^. MT.type'
        , proofOpKey = a ^. MT.key
        , proofOpData = a ^. MT.data'
        }

