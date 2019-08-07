FROM haskell:8

RUN apt-get update && apt-get install --assume-yes protobuf-compiler

# Install GHC.
WORKDIR /project
COPY ./stack.yaml /project
RUN stack setup && stack exec -- ghc --version

# Install dependencies.
COPY ./hs-abci-types /project
RUN stack build hs-abci-types

COPY ./hs-abci-server /project
RUN stack build hs-abci-server

COPY ./hs-abci-extra /project
RUN stack build hs-abci-extra

COPY ./hs-abci-example /project
RUN stack build hs-abci-example --copy-bins --local-bin-path /usr/local/bin

# Run project.
CMD /usr/local/bin/simple-storage