module Tendermint.SDK.Application
  ( MakeApplication(..)
  , createApplication
  ) where

import           Control.Exception
import           Control.Lens                         ((&), (.~))
import           Data.Default.Class                   (Default (..))
import           Data.Proxy
import           Data.String.Conversions              (cs)
import           Network.ABCI.Server.App              (App, MessageType,
                                                       Response (..),
                                                       transformApp)
import qualified Network.ABCI.Types.Messages.Response as Resp
import           Polysemy                             (Sem)

data MakeApplication r e = MakeApplication
  { app         :: App (Sem r)
  , transformer :: forall a. (Sem r) a -> IO a
  , appErrorP   :: Proxy e
  }

transformResponse
  :: forall e r.
     Exception e
  => MakeApplication r e
  -> (forall (t :: MessageType). Sem r (Response t) -> IO (Response t))
transformResponse MakeApplication{transformer} m = do
  eRes :: Either e (Response t) <- try $ transformer m
  case eRes of
    Left e -> pure $ ResponseException $
      def & Resp._exceptionError .~ cs (displayException e)
    Right a -> pure a

createApplication
  :: Exception e
  => MakeApplication r e
  -> App IO
createApplication ma@MakeApplication{app} =
  transformApp (transformResponse ma) app
