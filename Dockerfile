FROM haskell:8

RUN apt-get update && apt-get install --assume-yes protobuf-compiler libsecp256k1-dev

# Install GHC.
WORKDIR /project
COPY . /project

# Install project to /usr/local/bin
RUN stack setup && stack exec -- ghc --version
RUN stack build --copy-bins --local-bin-path /usr/local/bin

