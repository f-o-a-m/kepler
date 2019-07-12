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
import GHC.Generics (Generic)
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
timestamp = iso to from
  where
    tenToTwelth = 1000000000000
    tenToThird = 1000
    to (Timestamp t) =
      let ps = diffTimeToPicoseconds t
          s = ps `div` tenToTwelth
          ns = (ps - s * tenToTwelth) `div` tenToThird
      in defMessage & T.seconds .~ fromInteger s
                    & T.nanos .~ fromInteger ns
    from ts =
      let ps1 = toInteger (ts ^. T.seconds) * tenToTwelth
          ps2 = toInteger (ts ^. T.nanos) * tenToThird
      in Timestamp . picosecondsToDiffTime $ ps1 + ps2

data BlockSizeParams =
  BlockSizeParams { blockSizeParamsMaxBytes :: Int64
                  , blockSizeParamsMaxGas   :: Int64
                  } deriving (Eq, Show, Generic)

blockSizeParams :: Iso' BlockSizeParams PT.BlockSizeParams
blockSizeParams = iso to from
  where
    to BlockSizeParams{..} = defMessage & PT.maxBytes .~ blockSizeParamsMaxBytes
                                        & PT.maxGas .~ blockSizeParamsMaxGas
    from bsParams = BlockSizeParams { blockSizeParamsMaxBytes = bsParams ^. PT.maxBytes
                                    , blockSizeParamsMaxGas = bsParams ^. PT.maxGas
                                    }

data EvidenceParams =
  EvidenceParams { evidenceParamsMaxAge :: Int64
                 } deriving (Eq, Show, Generic)

evidenceParams :: Iso' EvidenceParams PT.EvidenceParams
evidenceParams = iso to from
  where
    to EvidenceParams{..} = defMessage & PT.maxAge .~ evidenceParamsMaxAge
    from eParams = EvidenceParams { evidenceParamsMaxAge = eParams ^. PT.maxAge
                                  }

data ValidatorParams =
  ValidatorParams { validatorParamsPubKeyTypes :: [Text]
                  } deriving (Eq, Show, Generic)

validatorParams :: Iso' ValidatorParams PT.ValidatorParams
validatorParams = iso to from
  where
    to ValidatorParams{..} = defMessage & PT.pubKeyTypes .~ validatorParamsPubKeyTypes
    from vParams = ValidatorParams { validatorParamsPubKeyTypes = vParams ^. PT.pubKeyTypes
                                   }

data ConsensusParams =
  ConsensusParams { consensusParamsBlockSize :: Maybe BlockSizeParams
                  , consensusParamsEvidence  :: Maybe EvidenceParams
                  , consensusParamsValidator :: Maybe ValidatorParams
                  } deriving (Eq, Show, Generic)

consensusParams :: Iso' ConsensusParams PT.ConsensusParams
consensusParams = iso to from
  where
    to ConsensusParams{..} = defMessage & PT.maybe'blockSize .~ consensusParamsBlockSize ^? _Just . blockSizeParams
                                        & PT.maybe'evidence .~ consensusParamsEvidence ^? _Just . evidenceParams
                                        & PT.maybe'validator .~ consensusParamsValidator ^? _Just . validatorParams
    from cParams = ConsensusParams { consensusParamsBlockSize = cParams ^? PT.maybe'blockSize . _Just . Lens.from blockSizeParams
                                   , consensusParamsEvidence =  cParams ^? PT.maybe'evidence . _Just . Lens.from evidenceParams
                                   , consensusParamsValidator =  cParams ^? PT.maybe'validator . _Just . Lens.from validatorParams
                                   }

data PubKey =
  PubKey { pubKeyType :: Text
         , pubKeyData :: ByteString
         } deriving (Eq, Show, Generic)

pubKey :: Iso' PubKey PT.PubKey
pubKey = iso to from
  where
    to PubKey{..} = defMessage & PT.type' .~ pubKeyType
                               & PT.data' .~ pubKeyData

    from pk = PubKey { pubKeyType = pk ^. PT.type'
                     , pubKeyData = pk ^. PT.data'
                     }

data ValidatorUpdate =
  ValidatorUpdate { validatorUpdatePubKey :: Maybe PubKey
                  , validatorUpdatePower  :: Int64
                  } deriving (Eq, Show, Generic)

validatorUpdate :: Iso' ValidatorUpdate PT.ValidatorUpdate
validatorUpdate = iso to from
  where
    to ValidatorUpdate{..} = defMessage & PT.maybe'pubKey .~ validatorUpdatePubKey ^? _Just . pubKey
                                        & PT.power .~ validatorUpdatePower
    from vu = ValidatorUpdate { validatorUpdatePubKey = vu ^? PT.maybe'pubKey . _Just . Lens.from pubKey
                              , validatorUpdatePower = vu ^. PT.power
                              }

data Validator =
  Validator { validatorAddress :: ByteString
            , validatorPower   :: Int64
            } deriving (Eq, Show, Generic)

validator :: Iso' Validator PT.Validator
validator = iso to from
  where
    to Validator{..} = defMessage & PT.address .~ validatorAddress
                                  & PT.power .~ validatorPower
    from validator = Validator { validatorAddress = validator ^. PT.address
                               , validatorPower = validator ^. PT.power
                               }

data VoteInfo =
  VoteInfo { voteInfoValidator       :: Maybe Validator
           , voteInfoSignedLastBlock :: Bool
           } deriving (Eq, Show, Generic)

voteInfo :: Iso' VoteInfo PT.VoteInfo
voteInfo = iso to from
  where
    to VoteInfo{..} = defMessage & PT.maybe'validator .~ voteInfoValidator ^? _Just . validator
                                 & PT.signedLastBlock .~ voteInfoSignedLastBlock
    from voteInfo = VoteInfo { voteInfoValidator = voteInfo ^? PT.maybe'validator . _Just . Lens.from validator
                             , voteInfoSignedLastBlock = voteInfo ^. PT.signedLastBlock
                             }

data LastCommitInfo =
  LastCommitInfo { lastCommitInfoRound :: Int32
                 , lastCommitInfoVotes :: [VoteInfo]
                 } deriving (Eq, Show, Generic)

lastCommitInfo :: Iso' LastCommitInfo PT.LastCommitInfo
lastCommitInfo = iso to from
  where
    to LastCommitInfo{..} = defMessage & PT.round .~ lastCommitInfoRound
                                       & PT.votes .~ lastCommitInfoVotes ^.. traverse . voteInfo
    from lastCommitInfo =
      LastCommitInfo { lastCommitInfoRound = lastCommitInfo ^. PT.round
                     , lastCommitInfoVotes = lastCommitInfo ^.. PT.votes . traverse . Lens.from voteInfo
                     }

data PartSetHeader =
  PartSetHeader { partSetHeaderTotal :: Int32
                , partSetHeaderHash  :: ByteString
                } deriving (Eq, Show, Generic)

partSetHeader :: Iso' PartSetHeader PT.PartSetHeader
partSetHeader = iso to from
  where
    to PartSetHeader{..} = defMessage & PT.total .~ partSetHeaderTotal
                                      & PT.hash .~ partSetHeaderHash
    from partSetHeader =
      PartSetHeader { partSetHeaderTotal = partSetHeader ^. PT.total
                    , partSetHeaderHash = partSetHeader ^. PT.hash
                    }

data BlockID =
  BlockID { blockIDHash        :: ByteString
          , blockIDPartsHeader :: Maybe PartSetHeader
          } deriving (Eq, Show, Generic)

blockID :: Iso' BlockID PT.BlockID
blockID = iso to from
  where
    to BlockID{..} = defMessage & PT.hash .~ blockIDHash
                                & PT.maybe'partsHeader .~ blockIDPartsHeader ^? _Just . partSetHeader
    from blockID =
      BlockID { blockIDHash = blockID ^. PT.hash
              , blockIDPartsHeader = blockID ^? PT.maybe'partsHeader . _Just . Lens.from partSetHeader
              }

data Version =
  Version { versionBlock :: Word64
          , versionApp   :: Word64
          } deriving (Eq, Show, Generic)

version :: Iso' Version PT.Version
version = iso to from
  where
    to Version{..} = defMessage & PT.block .~ versionBlock
                                & PT.app .~ versionApp
    from version = Version { versionBlock = version ^. PT.block
                           , versionApp = version ^. PT.app
                           }

data Header =
  Header { headerVersion            :: Maybe Version
         , headerChainId            :: Text
         , headerHeight             :: Int64
         , headerTime               :: Maybe Timestamp
         , headerNumTxs             :: Int64
         , headerTotalTxs           :: Int64
         , headerLastBlockId        :: BlockID
         , headerLastCommitHash     :: ByteString
         , headerDataHash           :: ByteString
         , headerValidatorsHash     :: ByteString
         , headerNextValidatorsHash :: ByteString
         , headerConsensusHash      :: ByteString
         , headerAppHash            :: ByteString
         , headerLastResultsHash    :: ByteString
         , headerEvidenceHash       :: ByteString
         , headerProposerAddress    :: ByteString
         } deriving (Eq, Show, Generic)


header :: Iso' Header PT.Header
header = iso to from
  where
    to Header{..} = defMessage & PT.maybe'version .~ headerVersion ^? _Just . version
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
    from header = Header { headerVersion = header ^? PT.maybe'version . _Just . Lens.from version
                         , headerChainId = header ^. PT.chainId
                         , headerHeight = header ^. PT.height
                         , headerTime = header ^? PT.maybe'time . _Just . Lens.from timestamp
                         , headerNumTxs = header ^. PT.numTxs
                         , headerTotalTxs = header ^. PT.totalTxs
                         , headerLastBlockId = header ^. PT.lastBlockId. Lens.from blockID
                         , headerLastCommitHash = header ^. PT.lastCommitHash
                         , headerDataHash = header ^. PT.dataHash
                         , headerValidatorsHash = header ^. PT.validatorsHash
                         , headerNextValidatorsHash = header ^. PT.nextValidatorsHash
                         , headerConsensusHash = header ^. PT.consensusHash
                         , headerAppHash = header ^. PT.appHash
                         , headerLastResultsHash = header ^. PT.lastResultsHash
                         , headerEvidenceHash = header ^. PT.evidenceHash
                         , headerProposerAddress = header ^. PT.proposerAddress
                         }

data Evidence =
  Evidence { evidenceType             :: Text
           , evidenceValidator        :: Maybe Validator
           , evidenceHeight           :: Int64
           , evidenceTime             :: Maybe Timestamp
           , evidenceTotalVotingPower :: Int64
           } deriving (Eq, Show, Generic)

evidence :: Iso' Evidence PT.Evidence
evidence = iso to from
  where
    to Evidence{..} = defMessage & PT.type' .~ evidenceType
                                 & PT.maybe'validator .~ evidenceValidator ^? _Just . validator
                                 & PT.height .~ evidenceHeight
                                 & PT.maybe'time .~ evidenceTime ^? _Just . timestamp
                                 & PT.totalVotingPower .~ evidenceTotalVotingPower
    from evidence =
      Evidence { evidenceType = evidence ^. PT.type'
               , evidenceValidator = evidence ^? PT.maybe'validator . _Just . Lens.from validator
               , evidenceHeight = evidence ^. PT.height
               , evidenceTime = evidence ^? PT.maybe'time . _Just . Lens.from timestamp
               , evidenceTotalVotingPower = evidence ^. PT.totalVotingPower
               }


data KVPair = KVPair
  { kVPairKey   :: ByteString
  , kVPairValue :: ByteString
  } deriving (Eq, Show, Generic)

kVPair :: Iso' KVPair CT.KVPair
kVPair = iso to from
  where
    to KVPair{..} =
      defMessage
        & CT.key .~ kVPairKey
        & CT.value .~ kVPairValue
    from kVPair =
      KVPair
        { kVPairKey = kVPair ^. CT.key
        , kVPairValue = kVPair ^. CT.value
        }


data Proof = Proof
  { proofOps :: [ProofOp]
  } deriving (Eq, Show, Generic)

proof :: Iso' Proof MT.Proof
proof = iso to from
  where
    to Proof{..} =
      defMessage
        & MT.ops .~ proofOps ^.. traverse . proofOp
    from proof =
      Proof
        { proofOps = proof ^.. MT.ops . traverse . Lens.from proofOp
        }


data ProofOp = ProofOp
  { proofOpType :: Text
  , proofOpKey  :: ByteString
  , proofOpData :: ByteString
  } deriving (Eq, Show, Generic)

proofOp :: Iso' ProofOp MT.ProofOp
proofOp = iso to from
  where
    to ProofOp{..} =
      defMessage
        & MT.type' .~ proofOpType
        & MT.key .~ proofOpKey
        & MT.data' .~ proofOpData
    from proofOp =
      ProofOp
        { proofOpType = proofOp ^. MT.type'
        , proofOpKey = proofOp ^. MT.key
        , proofOpData = proofOp ^. MT.data'
        }

