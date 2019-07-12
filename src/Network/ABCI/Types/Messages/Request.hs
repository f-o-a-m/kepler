module Network.ABCI.Types.Messages.Request where

import           Control.Lens                           (Iso', iso, traverse,
                                                         (&), (.~), (^.), (^..),
                                                         (^?), _Just)
import qualified Control.Lens                           as Lens
import           Data.ByteString                        (ByteString)
import           Data.Int                               (Int64)
import           Data.ProtoLens.Message                 (Message (defMessage))
import           Data.Text                              (Text)
import           Data.Word                              (Word64)
import           Network.ABCI.Types.Messages.FieldTypes (ConsensusParams,
                                                         Evidence, Header,
                                                         LastCommitInfo,
                                                         Timestamp,
                                                         ValidatorUpdate,
                                                         consensusParams,
                                                         evidence, header,
                                                         lastCommitInfo,
                                                         timestamp,
                                                         validatorUpdate)
import qualified Proto.Types                            as PT
import qualified Proto.Types_Fields                     as PT

{-
       (ABCIApplication(..), BlockID(), BlockSizeParams(),
        ConsensusParams(), Evidence(), EvidenceParams(), Header(),
        LastCommitInfo(), PartSetHeader(), PubKey(), Request(),
        Request'Value(..), _Request'Echo, _Request'Flush, _Request'Info,
        _Request'SetOption, _Request'InitChain, _Request'Query,
        _Request'BeginBlock, _Request'CheckTx, _Request'DeliverTx,
        _Request'EndBlock, _Request'Commit, RequestBeginBlock(),
        RequestCheckTx(), RequestCommit(), RequestDeliverTx(),
        RequestEcho(), RequestEndBlock(), RequestFlush(), RequestInfo(),
        RequestInitChain(), RequestQuery(), RequestSetOption(), Response(),
        Response'Value(..), _Response'Exception, _Response'Echo,
        _Response'Flush, _Response'Info, _Response'SetOption,
        _Response'InitChain, _Response'Query, _Response'BeginBlock,
        _Response'CheckTx, _Response'DeliverTx, _Response'EndBlock,
        _Response'Commit, ResponseBeginBlock(), ResponseCheckTx(),
        ResponseCommit(), ResponseDeliverTx(), ResponseEcho(),
        ResponseEndBlock(), ResponseException(), ResponseFlush(),
        ResponseInfo(), ResponseInitChain(), ResponseQuery(),
        ResponseSetOption(), Validator(), ValidatorParams(),
        ValidatorUpdate(), Version(), VoteInfo())
-}

{-
data MessageType =
    Echo
  | Flus
  | Info
  | SetOption
  | InitChain
  | Query
  | BeginBlock
  | CheckTx
  | DeliverTx
  | EndBlock
  | Commit
-}

data Echo =
  Echo { echoMessage :: Text }

echo :: Iso' Echo PT.RequestEcho
echo = iso to from
  where
    to Echo{..} = defMessage & PT.message .~ echoMessage
    from requestEcho = Echo { echoMessage = requestEcho ^. PT.message
                            }

data Flush = Flush

flush :: Iso' Flush PT.RequestFlush
flush = iso to from
  where
    to = const defMessage
    from = const Flush

data Info =
  Info { infoVersion      :: Text
       , infoBlockVersion :: Word64
       , infoP2pVersion   :: Word64
       }

info :: Iso' Info PT.RequestInfo
info = iso to from
  where
    to Info{..} = defMessage & PT.version .~ infoVersion
                             & PT.blockVersion .~ infoBlockVersion
                             & PT.p2pVersion .~ infoP2pVersion
    from requestInfo = Info { infoVersion = requestInfo ^. PT.version
                            , infoBlockVersion = requestInfo ^. PT.blockVersion
                            , infoP2pVersion = requestInfo ^. PT.p2pVersion
                            }

data SetOption =
  SetOption { setOptionKey   :: Text
            , setOptionValue :: Text
            }

setOption :: Iso' SetOption PT.RequestSetOption
setOption = iso to from
  where
    to SetOption{..} = defMessage & PT.key .~ setOptionKey
                                  & PT.value .~ setOptionValue
    from requestSetOption = SetOption { setOptionKey = requestSetOption ^. PT.key
                                      , setOptionValue = requestSetOption ^. PT.value
                                      }

data InitChain =
  InitChain { initChainTime            :: Maybe Timestamp
            , initChainChainId         :: Text
            , initChainConsensusParams :: Maybe ConsensusParams
            , initChainValidators      :: [ValidatorUpdate]
            , initChainAppState        :: ByteString
            }

initChain :: Iso' InitChain PT.RequestInitChain
initChain = iso to from
  where
    to InitChain{..} =
      defMessage & PT.maybe'time .~ initChainTime ^? _Just . timestamp
                 & PT.chainId .~ initChainChainId
                 & PT.maybe'consensusParams .~ initChainConsensusParams ^? _Just . consensusParams
                 & PT.validators .~ initChainValidators ^.. traverse . validatorUpdate
                 & PT.appStateBytes .~ initChainAppState
    from requestInitChain =
      InitChain  { initChainTime = requestInitChain ^? PT.maybe'time . _Just . Lens.from timestamp
                 , initChainChainId = requestInitChain ^. PT.chainId
                 , initChainConsensusParams = requestInitChain ^? PT.maybe'consensusParams . _Just . Lens.from consensusParams
                 , initChainValidators = requestInitChain ^.. PT.validators . traverse . Lens.from validatorUpdate
                 , initChainAppState = requestInitChain ^. PT.appStateBytes
                 }

data Query =
  Query { queryData   :: ByteString
        , queryPath   :: Text
        , queryHeight :: Int64
        , queryProve  :: Bool
        }

query :: Iso' Query PT.RequestQuery
query = iso to from
  where
    to Query{..} = defMessage & PT.data' .~ queryData
                              & PT.path .~ queryPath
                              & PT.height .~ queryHeight
                              & PT.prove .~ queryProve
    from requestQuery = Query { queryData = requestQuery ^. PT.data'
                              , queryPath = requestQuery ^. PT.path
                              , queryHeight = requestQuery ^. PT.height
                              , queryProve = requestQuery ^. PT.prove
                              }

data BeginBlock =
  BeginBlock { beginBlockHash                :: ByteString
             , beginBlockHeader              :: Maybe Header
             , beginBlockLastCommitInfo      :: Maybe LastCommitInfo
             , beginBlockByzantineValidators :: [Evidence]
             }

beginBlock :: Iso' BeginBlock PT.RequestBeginBlock
beginBlock = iso to from
  where
    to BeginBlock{..} =
      defMessage & PT.hash .~ beginBlockHash
                 & PT.maybe'header .~ beginBlockHeader ^? _Just . header
                 & PT.maybe'lastCommitInfo .~ beginBlockLastCommitInfo ^? _Just . lastCommitInfo
                 & PT.byzantineValidators .~ beginBlockByzantineValidators ^.. traverse . evidence
    from requestBeginBlock =
      BeginBlock { beginBlockHash = requestBeginBlock ^. PT.hash
                 , beginBlockHeader = requestBeginBlock ^? PT.maybe'header . _Just . Lens.from header
                 , beginBlockLastCommitInfo = requestBeginBlock ^? PT.maybe'lastCommitInfo . _Just . Lens.from lastCommitInfo
                 , beginBlockByzantineValidators = requestBeginBlock ^.. PT.byzantineValidators . traverse . Lens.from evidence
                 }


data CheckTx =
  CheckTx
    { checkTxTx :: ByteString
    }
checkTx :: Iso' CheckTx PT.RequestCheckTx
checkTx = iso to from
  where
    to CheckTx{..} =
      defMessage
        & PT.tx .~ checkTxTx

    from requestCheckTx =
      CheckTx
        { checkTxTx = requestCheckTx ^. PT.tx
        }

data Commit =
  Commit

commit :: Iso' Commit PT.RequestCommit
commit = iso to from
  where
    to Commit =
      defMessage

    from requestCommit =
      Commit

data DeliverTx =
  DeliverTx
    { deliverTxTx :: ByteString
    }

deliverTx :: Iso' DeliverTx PT.RequestDeliverTx
deliverTx = iso to from
  where
    to DeliverTx{..} =
      defMessage
        & PT.tx .~ deliverTxTx

    from requestDeliverTx =
      DeliverTx
        { deliverTxTx = requestDeliverTx ^. PT.tx
        }


data EndBlock  = EndBlock
    { endBlockHeight :: Int64
    }

endBlock :: Iso' EndBlock PT.RequestEndBlock
endBlock = iso to from
  where
    to EndBlock{..} =
      defMessage
        & PT.height .~ endBlockHeight

    from requestEndBlock =
      EndBlock
        { endBlockHeight = requestEndBlock ^. PT.height
        }
