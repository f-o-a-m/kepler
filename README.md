# hs-abci-server

## Protobuf Files and Generated Modules
The protobuf files are all in the `/protos` directory, and we use a custom setup in order
to generate the corrosponding `Proto.*` Haskell modules. If you want to view all of these
generated modules, you can run

```bash
> find .stack-work -path '*autogen/Proto'
``` 

to find the root directory.


## Building documentation
You can build the haddocks for the library code only using

```bash
make build-docs-local
```

This does not build and link documentation for dependencies, useful mostly for testing
documentation formatting.
