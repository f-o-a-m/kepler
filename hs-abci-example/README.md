# hs-abci-example

The example application is meant to test all of the other hs-abci libraries and serve as a demo.
It's a simple application called **Simple Storage** that maintains a shared 32-byte integer and
allows users to update and query the count.

## Running with Docker
There is a `docker-compose.yaml` file in this directory. If you use the `make` command from the project root

```bash
> make deploy-simple-storage
```

it will build an image for simple-storage and launch it in a docker network
with a tendermint-core node. The port for simple-storage is not exposed outside of the docker network --
if you would like to submit transactions or query state you must do it using the tendermint RPC.

## Application Messages
The application uses a protobuf file to define its [transaction messages](https://github.com/f-o-a-m/hs-abci/blob/master/hs-abci-example/protos/simple-storage/messages.proto). Thus if you would like to post transactions to this application via RPC, you will need to first consume
this profobuf file. You can follow the pattern in the test suite using hs-tendermint-client.
