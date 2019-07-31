{-# LANGUAGE TemplateHaskell #-}
module Network.ABCI.Middleware.RequestLogger
    ( logStdout
    ) where

import           Control.Monad.IO.Class                 (MonadIO, liftIO)
import           Data.Aeson                             (FromJSON (..),
                                                         ToJSON (..),
                                                         genericParseJSON,
                                                         genericToJSON)
import           Data.Aeson.Casing                      (aesonPrefix, camelCase)
import           Katip


import           Network.ABCI.Types.App                 (App (..), Middleware)
import           Network.ABCI.Types.Messages.FieldTypes

-- | Production request logger middleware for ABCI requests
logStdout :: (MonadIO m) => LogEnv -> Namespace -> Middleware m
logStdout env ns (App app) = App $ \ req -> undefined
  -- liftIO (runKatipContextT env req ns $ $(logTM) InfoS "Request Received")
  -- >> app req

