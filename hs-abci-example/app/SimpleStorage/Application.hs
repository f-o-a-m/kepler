module SimpleStorage.Application
  ( AppError(..)
  , AppConfig(..)
  , makeAppConfig
  , Handler
  , defaultHandler
  , transformHandler
  ) where

import           Control.Lens                         ((&), (.~))
import           Control.Monad.Except                 (ExceptT, MonadError,
                                                       runExceptT)
import           Control.Monad.IO.Class               (MonadIO)
import           Control.Monad.Reader                 (MonadReader, ReaderT,
                                                       runReaderT)
import           Data.Default.Class                   (Default (..))
import           Data.Text                            (Text, pack)
import qualified Network.ABCI.Types.Messages.Response as Resp
import           Network.ABCI.Types.Messages.Types    (MessageType (..))
import           SimpleStorage.StateMachine           (initStateMachine)
import qualified Tendermint.SDK.DB                    as DB
import           Tendermint.SDK.Transaction           (TransactionError (..))

data AppConfig = AppConfig
  { countConnection :: DB.Connection "count"
  }

makeAppConfig :: IO AppConfig
makeAppConfig = do
  conn <- initStateMachine
  pure $ AppConfig { countConnection = conn
                   }

data AppError =
    QueryMissError String
  | TxError TransactionError
  | DecodeTxError String
  deriving (Show)

printAppError :: AppError -> Text
printAppError (QueryMissError msg) = pack $ "QueryMissError : " <> msg
printAppError (TxError (TransactionError txError)) = pack $ "TransactionError : " <> txError
printAppError (DecodeTxError msg) = pack $ "DecodeTxError : " <> msg

newtype Handler a = Handler
  { runHandler :: ReaderT AppConfig (ExceptT AppError IO) a }
  deriving (Functor, Applicative, Monad, MonadReader AppConfig, MonadError AppError, MonadIO)

-- NOTE: this should probably go in the library
defaultHandler
  :: ( Default a
     , Applicative m
     )
  => b
  -> m a
defaultHandler = const $ pure def

transformHandler
  :: AppConfig
  -> (forall (t :: MessageType). Handler (Resp.Response t) -> IO (Resp.Response t))
transformHandler cfg m = do
  eRes <- runExceptT $ runReaderT (runHandler m) cfg
  case eRes of
    Left e  -> pure $ Resp.ResponseException $
      def & Resp._exceptionError .~ printAppError e
    Right a -> pure a
