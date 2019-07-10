# hs-abci-server

## Protobuf Files and Generated Modules
The protobuf files are all in the `/protos` directory, and we use a custom setup in order
to generate the corrosponding `Proto.*` Haskell modules. If you want to view all of these
generated modules, you can run

```bash
> find .stack-work -path '*autogen/Proto'
``` 

to find the root directory.
