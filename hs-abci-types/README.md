# hs-abci-types

This module provides haskell bindings for types defined in [tendermint/tendermint/abci/types/types.proto](https://github.com/tendermint/tendermint/blob/62f97a69e97262b5feb57b1c2498f0c1e0e297b3/abci/types/types.proto#L3) and other supporting types.


## Under the hood
Under the hood [proto-lens](https://github.com/google/proto-lens) is used which generates all the types from the `types.proto` file, but generated code is a bit brutal to use directly and this modiule defines more user friendly versions of them with Control.Lens.Wrapped instance. this way we still use proto-lens for encoding/decoding messages used for communicating with ABCI server, but users have to work with nicer types defined in this module.

## Side note
For communicating with tendermint json-rpc server ToJSON/FromJSON instances of this types is used.

