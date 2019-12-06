module Tendermint.SDK.Application.App
  ( createIOApp
  ) where

import           Control.Exception
import           Control.Lens                         ((&), (.~))
import           Data.Default.Class                   (Default (..))
import           Data.String.Conversions              (cs)
import           Network.ABCI.Server.App              (App (..), MessageType,
                                                       Response (..),
                                                       transformApp)
import qualified Network.ABCI.Types.Messages.Response as Resp
import           Polysemy                             (Sem)
import           Tendermint.SDK.BaseApp.Errors        (AppError)

createIOApp
  :: forall r.
     App (Sem r)
  -> (forall a. (Sem r) a -> IO a)
  -> App IO
createIOApp app nat = transformApp transformResponse app
  where
  transformResponse :: (forall (t :: MessageType). Sem r (Response t) -> IO (Response t))
  transformResponse (resp :: Sem r (Response t)) = do
    eRes :: Either AppError (Response t) <- try $ nat $ resp
    case eRes of
      Left e -> pure $ ResponseException $
        def & Resp._exceptionError .~ cs (displayException e)
      Right a -> pure a
