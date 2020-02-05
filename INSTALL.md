---
title: Installation
---

## Build

### Prerequisites

#### stack
At the moment the project's build is managed by `stack`. You can find everything you need regarding how to install stack on your machine [here](https://docs.haskellstack.org/en/stable/README/).

#### protoc
We use a custom setup to generate Haskell bindings to the protobuf files, using the proto-lens library from Google. In order for this to work you need to have the protobuf compiler `protoc` on your machine. You can get installation instructions [here](https://google.github.io/proto-lens/installing-protoc.html)

#### libsecp256k1
In order to build with stack you will need this. On MacOS you can use brew:

```
> brew tap cuber/homebrew-libsecp256k1
> brew install libsecp256k1
```

On linux:

```
> sudo add-apt-repository ppa:tah83/secp256k1
> sudo apt-get update
> sudo apt-get install libsecp256k1
```

#### style
You will also need to install `hlint` and `stylish-haskell` for code hygiene during development. In the project root simply run

```bash
> stack install hlint stylish-haskell
```

### Commands
There is a `Makefile` for this project where you can find all of the options for building, testing etc. The `Makefile`
is documented and there is a help menu which you can access via the commands `make` or `make help`.

## Protobuf Files and Generated Modules
The protobuf files are all in the `/protos` directory, and we use a custom setup in order
to generate the corresponding `Proto.*` Haskell modules. If you want to view all of these
generated modules, you can run

```bash
> find hs-abci-types/.stack-work -path '*autogen/Proto'
```

to find the root directory.

## Style Guide
There is a `.stylish-haskell.yaml` file that controls some of the style guide, particularly
around import statements and some indentation rules. There is also a small guide for things that
fall outside of this in the [style wiki](https://github.com/f-o-a-m/hs-abci/wiki/code-style-guide).
If it's not covered by either of these but you think it's really important, mention it in an issue.

## Building documentation
You can build the haddocks for the library code only using

```bash
make build-docs-local
```

This does not build and link documentation for dependencies, useful mostly for testing
documentation formatting.
