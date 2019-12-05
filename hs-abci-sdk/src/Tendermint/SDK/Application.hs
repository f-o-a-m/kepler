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
import           Polysemy                             (Member, Sem)
import           Tendermint.SDK.Store                 (MergeScopes, mergeScopes)

data MakeApplication r e = MakeApplication
  { app         :: App (Sem r)
  , transformer :: forall a. (Sem r) a -> IO a
  , appErrorP   :: Proxy e
  , initialize  :: [Sem r ()]
  }

defaultHandler
  :: Default a
  => Applicative m
  => b
  -> m a
defaultHandler = const $ pure def

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
  => Member MergeScopes r
  => MakeApplication r e
  -> IO (App IO)
createApplication ma@MakeApplication{app, transformer, initialize} = do
    transformer $ do
      sequence_ initialize
      mergeScopes
    pure $ transformApp (transformResponse ma) app
