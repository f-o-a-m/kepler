module Network.ABCI.Types.Messages.Request where

import Control.Lens (Iso', iso, (^.), (.~), (&))
import Data.ProtoLens.Message (Message (defMessage, unknownFields), FieldSet)
import Data.Text (Text)
import Data.Word (Word64)
import qualified Proto.Types as PT
import qualified Proto.Types_Fields as PT

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

data Flush =
  Flush { flushUnknownFields :: FieldSet }

flush :: Iso' Flush PT.RequestFlush
flush = iso to from
  where
    to Flush{..} = defMessage & unknownFields .~ flushUnknownFields
    from requestFlush = Flush { flushUnknownFields = requestFlush ^. unknownFields
                              }

data Info =
  Info { infoVersion :: Text
       , infoBlockVersion :: Word64
       , infoP2pVersion :: Word64
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
