{-# OPTIONS_GHC -fno-warn-orphans #-}

module Tendermint.SDK.Types.Message.Decode.ProtoLens (ProtoLens) where

import           Data.Bifunctor               (first)
import qualified Data.ProtoLens               as PL
import           Data.String.Conversions      (cs)
import           Tendermint.SDK.Types.Message (MessageParseError (..),
                                               ParseMessage (..))

data ProtoLens

instance PL.Message msg => ParseMessage ProtoLens msg where
  decodeMessage _ = first (OtherParseError . cs) . PL.decodeMessage
