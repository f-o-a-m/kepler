module Tendermint.SDK.BaseApp.Query.Effect
  ( QueryEffs
  , runQuery
  ) where

import           Control.Lens                           ((&), (.~))
import           Data.ByteArray.Base64String            (fromBytes)
import           Data.Default.Class                     (def)
import           Network.ABCI.Types.Messages.FieldTypes (WrappedVal (..))
import qualified Network.ABCI.Types.Messages.Response   as Response
import           Polysemy                               (Member, Sem, subsume)
import           Polysemy.Error                         (Error, runError)
import           Tendermint.SDK.BaseApp.Errors          (AppError,
                                                         queryAppError)
import           Tendermint.SDK.BaseApp.Query.Types
import           Tendermint.SDK.BaseApp.Store           (ReadStore)
import           Tendermint.SDK.Codec                   (HasCodec (..))
import           Tendermint.SDK.Types.Effects           ((:&))

type QueryEffs =
    '[ ReadStore
     , Error AppError
     ]

runQuery
  :: HasCodec a
  => Member ReadStore r
  => Sem (QueryEffs :& r) (QueryResult a)
  -> Sem r Response.Query
runQuery query = do
  eRes <- eval query
  pure $ case eRes of
    Left e -> def & queryAppError .~ e
    Right QueryResult{..}  ->
      def
        & Response._queryIndex .~ WrappedVal queryResultIndex
        & Response._queryKey .~ queryResultKey
        & Response._queryValue .~ fromBytes (encode queryResultData)
        & Response._queryProof .~ queryResultProof
        & Response._queryHeight .~ WrappedVal queryResultHeight

eval
  :: Member ReadStore r
  => Sem (QueryEffs :& r) (QueryResult a)
  -> Sem r (Either AppError (QueryResult a))
eval = runError . subsume
