module Tendermint.Utils.QueryClient.Types where

import qualified Network.ABCI.Types.Messages.Response as Resp

-- | Data is Nothing iff Raw includes a non-0 response value
data QueryClientResponse a = QueryClientResponse
  { queryClientResponseData :: Maybe a
  , queryClientResponseRaw  :: Resp.Query
  }
