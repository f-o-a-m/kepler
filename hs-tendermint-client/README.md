# hs-tendermint-client

A client implementation of [tendermint's RPC server](https://tendermint.com/rpc).

Implemented methods:

- [/abci_info](https://tendermint.com/rpc/#abciinfo)
- [/health](https://tendermint.com/rpc/#health)
- [/abci_query](https://tendermint.com/rpc/#abciquery)
- [/block](https://tendermint.com/rpc/#block)
- [/tx](https://tendermint.com/rpc/#tx)
- [/broadcast_tx_async](https://tendermint.com/rpc/#broadcasttxasync)
- [/broadcast_tx_sync](https://tendermint.com/rpc/#broadcasttxsync)
- [/broadcast_tx_commit](https://tendermint.com/rpc/#broadcasttxcommit)

## Testing

`./kv-test` contains a minimal test suite for the above mentioned methods using the
[Tendermint KVStore app](https://tendermint.com/docs/app-dev/getting-started.html#kvstore-a-first-example).

To test locally, first start the KVStore app (you may need to 
[install ABCI-CLI](https://tendermint.com/docs/app-dev/abci-cli.html#using-abci-cli)):

```bash
> abci-cli kvstore
```

Then, start a tendermint core node with
```bash
> tendermint init
> tendermint node
```

Finally, run the tests:

```bash
make test-kv-store
```
