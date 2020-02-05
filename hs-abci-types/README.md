# hs-abci-types

This module provides haskell bindings for the Tendermint ABCI message types defined in [tendermint/tendermint/abci/types/types.proto](https://github.com/tendermint/tendermint/blob/v0.32.2/abci/types/types.proto#L3).

## Under the hood
We use [proto-lens](https://github.com/google/proto-lens) to generate all the types from files in the [protos](https://github.com/f-o-a-m/kepler/tree/master/hs-abci-types/protos) directory, but the generated code is a bit brutal to use directly. This package defines a more user friendly version using Control.Lens.Wrapped. This way we still use proto-lens for encoding/decoding protocol messages while users can work with nicer types defined here.

## JSON
Again, Tendermint protocol messages are defined with protobuf files and use protobuf codecs. However, we still supply `ToJSON`/`FromJSON` instances of the types for communicating with tendermint json-rpc server and because they are generally useful.

