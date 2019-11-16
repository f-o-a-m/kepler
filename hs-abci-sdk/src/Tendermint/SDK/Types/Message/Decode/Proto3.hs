{-# OPTIONS_GHC -fno-warn-orphans #-}
module Tendermint.SDK.Types.Message.Decode.Proto3 (Proto3Suite) where

import           Data.Bifunctor               (first)
import           Data.String.Conversions      (cs)
import qualified Proto3.Suite                 as Wire
import qualified Proto3.Wire.Decode           as Wire
import           Tendermint.SDK.Types.Message (MessageParseError (..),
                                               ParseMessage (..))

data Proto3Suite

instance Wire.Message msg => ParseMessage Proto3Suite msg where
  decodeMessage _ = first mkErr . Wire.fromByteString
    where
        mkErr (Wire.WireTypeError txt) = WireTypeError (cs txt)
        mkErr (Wire.BinaryError txt) = BinaryError (cs txt)
        mkErr (Wire.EmbeddedError txt merr) = EmbeddedError (cs txt) (mkErr <$> merr)
