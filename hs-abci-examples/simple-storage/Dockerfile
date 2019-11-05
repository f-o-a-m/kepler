FROM haskell:8

RUN apt-get update && apt-get install --assume-yes protobuf-compiler

# Install GHC.
WORKDIR /project
COPY ./stack.yaml /project
RUN stack setup && stack exec -- ghc --version

# Install dependencies.
COPY hs-abci-types hs-abci-types/
COPY hs-tendermint-client hs-tendermint-client/
COPY hs-abci-server hs-abci-server/
COPY hs-abci-extra hs-abci-extra/
COPY hs-abci-sdk hs-abci-sdk/
COPY hs-abci-examples/simple-storage simple-storage/

RUN stack build simple-storage --copy-bins --local-bin-path /usr/local/bin

# Run project.
CMD /usr/local/bin/simple-storage
