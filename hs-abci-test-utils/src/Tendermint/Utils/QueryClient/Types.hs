module Tendermint.Utils.QueryClient.Types where

import           Tendermint.SDK.BaseApp.Errors      (AppError)
import           Tendermint.SDK.BaseApp.Query.Types (QueryResult)

-- | Data is Nothing iff Raw includes a non-0 response value
data QueryClientResponse a =
    QueryResponse (QueryResult a)
  | QueryError AppError
  deriving (Eq, Show)
