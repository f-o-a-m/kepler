{-# LANGUAGE TemplateHaskell #-}

module Tendermint.SDK.Logger where

import           Data.String (fromString)
import qualified Katip       as K
import           Polysemy

data Logger m a where
  Log :: String -> Logger m ()

makeSem ''Logger

eval
  :: forall r a.
     K.Katip (Sem r)
  => K.KatipContext (Sem r)
  => Sem (Logger ': r) a
  -> Sem r a
eval = do
  interpret (\case
    Log msg -> K.logFM K.InfoS (fromString msg)
    )
