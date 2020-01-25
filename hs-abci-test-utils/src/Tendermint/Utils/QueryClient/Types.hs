module Tendermint.Utils.QueryClient.Types where

import qualified Network.ABCI.Types.Messages.Response as Resp
import           Tendermint.SDK.BaseApp.Errors        (AppError)

-- | Data is Nothing iff Raw includes a non-0 response value
data QueryClientResponse a =
    QueryResponse
      { queryClientResponseData :: a
      , queryClientResponseRaw  :: Resp.Query
      }
  | QueryError AppError
  deriving (Eq, Show)
