module Tendermint.SDK.Application
  ( MakeApplication(..)
  , createApplication
  , defaultHandler
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

data MakeApplication m e = MakeApplication
  { app         :: App m
  , transformer :: forall a. m a -> IO a
  , appErrorP   :: Proxy e
  , initialize  :: [m ()]
  }

defaultHandler
  :: ( Default a
     , Applicative m
     )
  => b
  -> m a
defaultHandler = const $ pure def

transformResponse
  :: forall e m.
     Exception e
  => MakeApplication m e
  -> (forall (t :: MessageType). m (Response t) -> IO (Response t))
transformResponse MakeApplication{transformer} m = do
  eRes :: Either e (Response t) <- try $ transformer m
  case eRes of
    Left e -> pure $ ResponseException $
      def & Resp._exceptionError .~ cs (displayException e)
    Right a -> pure a

createApplication
  :: ( Exception e
     , Monad m
     )
  => MakeApplication m e
  -> IO (App IO)
createApplication ma@MakeApplication{app, transformer, initialize} = do
    transformer $ sequence_ initialize
    pure $ transformApp (transformResponse ma) app
