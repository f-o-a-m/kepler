module Network.ABCI.Types.Messages.FieldTypes where

import           Control.Lens                                    (Iso', iso,
                                                                  (&), (.~),
                                                                  (^.))
import           Data.Int                                        (Int64)
import           Data.ProtoLens.Message                          (Message (defMessage))
import           Data.Text                                       (Text)
import           Data.Time.Clock                                 (DiffTime, diffTimeToPicoseconds,
                                                                  picosecondsToDiffTime)
import qualified Proto.Types                                     as PT
import qualified Proto.Types_Fields                              as PT
import qualified Proto.Vendored.Google.Protobuf.Timestamp        as T
import qualified Proto.Vendored.Google.Protobuf.Timestamp_Fields as T

data Timestamp =
  Timestamp DiffTime deriving (Eq, Show)

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
                  }

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
                 }

evidenceParams :: Iso' EvidenceParams PT.EvidenceParams
evidenceParams = iso to from
  where
    to EvidenceParams{..} = defMessage & PT.maxAge .~ evidenceParamsMaxAge
    from eParams = EvidenceParams { evidenceParamsMaxAge = eParams ^. PT.maxAge
                                  }


data ValidatorParams =
  ValidatorParams { validatorParamsPubKeyTypes :: [Text]
                  }

validatorParams :: Iso' ValidatorParams PT.ValidatorParams
validatorParams = iso to from
  where
    to ValidatorParams{..} = defMessage & PT.pubKeyTypes .~ validatorParamsPubKeyTypes
    from vParams = ValidatorParams { validatorParamsPubKeyTypes = vParams ^. PT.pubKeyTypes
                                   }
