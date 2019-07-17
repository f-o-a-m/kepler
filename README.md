# hs-abci-server

## Introduction
This is the official repository for the Haskell implementation of the ABCI server and
SDK for developing applications backed by the Tendermint replication engine. You can
read more about Tendermint and the ABCI specs in their [documentation](https://tendermint.com/docs/spec/abci/). 

## Protobuf Files and Generated Modules
The protobuf files are all in the `/protos` directory, and we use a custom setup in order
to generate the corrosponding `Proto.*` Haskell modules. If you want to view all of these
generated modules, you can run

```bash
> find .stack-work -path '*autogen/Proto'
``` 

to find the root directory.

## Style Guide
There is a `.stylish-haskell.yaml` file that controls some of the style guide, particularly 
around import statements and some indentation rules. There is also a small guide for things that
fall outside of this in the [style wiki](https://github.com/f-o-a-m/hs-abci-server/wiki/code-style-guide).
If it's not covered by either of these but you think it's really important, mention it in an issue.

## Building documentation
You can build the haddocks for the library code only using

```bash
make build-docs-local
```

This does not build and link documentation for dependencies, useful mostly for testing
documentation formatting.
