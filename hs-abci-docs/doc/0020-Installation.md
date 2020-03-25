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

### Step-by-step instructions

Assuming you have all the prerequisites installed, type `make help` to get a result like this:

```
build-docs-local               Build the haddocks documentation for just this project (no dependencies)
build-site                     Build the tintin site
deploy-nameservice             run the nameservice docker network with elk stack for logging
deploy-nameservice-test-down   remove the nameservice docker network
deploy-nameservice-test        run the nameservice docker network for testing
deploy-simple-storage-docker-down remove the simple storage docker network
deploy-simple-storage-docker   run the simple storage docker network
help                           Ask for help!
hlint                          Run hlint on all haskell projects
install                        Runs stack install to compile library and counter example app
interact-nameservice           Run nameservice interaction script
stylish                        Run stylish-haskell over all haskell projects
test-iavl-client               test the iavl client library basic operation (requires grpc service running on port 8090)
test-kv-store                  Run the test suite for the client interface
test-libraries                 Run the haskell test suite for all haskell libraries
test-nameservice               Run the test suite for the nameservice example application
test-simple-storage            Run the test suite for the simple-storage example application
test-tutorial                  Make sure the tutorial builds
weeder                         look for unused packages and functions
```

We'll start by running

```sh
> make install
```

If you run this for the first time, you'll see a lot of output from `stack` building all the libraries and their dependencies. Although it can take a long time, it will only happen once, unless you modify the app or the library.

Now, let's say we want to run the `simple-storage` sample app. We'll run

```sh
> make deploy-simple-storage-docker
```

You'll also see the docker image being built for the first time. This will also take some time. Eventually you'll see something like

```
Starting test-hs-abci-examples-simple-storage-e2e_tendermint-init_1 ... done
Starting test-hs-abci-examples-simple-storage-e2e_simple-storage_1  ... done
Starting test-hs-abci-examples-simple-storage-e2e_iavl_1            ... done
Starting test-hs-abci-examples-simple-storage-e2e_tendermint_1      ... done
```

Now we can run the test

```sh
> make test-simple-storage
...
Finished in 0.2366 seconds
3 examples, 0 failures
```

Clean up the docker environment by running

```sh
> make deploy-simple-storage-docker-down
```

We can run the nameservice tests by running

```sh
> make deploy-nameservice-test
> make test-nameservice
> make deploy-nameservice-test-down
```

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
fall outside of this in the [style wiki](https://github.com/f-o-a-m/kepler/wiki/code-style-guide).
If it's not covered by either of these but you think it's really important, mention it in an issue.

## Building documentation
You can build the haddocks for the library code only using

```bash
make build-docs-local
```

This does not build and link documentation for dependencies, useful mostly for testing
documentation formatting.

## Including `kepler` in your project

If you're using `stack` you may create a new project using `stack init`

Then, in your `stack.yaml` file, add the following lines:

```yaml
extra-deps:
  - git: https://github.com/f-o-a-m/hs-abci/
    commit: 3b13a569bcc77dcdec154552175f69bf4ea8b8ca
    subdirs:
      - hs-abci-extra
      - hs-abci-sdk
      - hs-abci-server
      - hs-abci-test-utils
      - hs-abci-types
      - hs-tendermint-client
```

but replace the `3b13a` with whatever is the latest hash for the master branch at the time of you reading this.
