STATS_PORT ?= 9200
INTERACT_THREAD_COUNT ?= 5

export

# This is useful for copying example app binaries built on a linux machine rather than building in docker
SIMPLE_STORAGE_BINARY := $(shell stack exec -- which simple-storage)

help: ## Ask for help!
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'

# Thank you Apple
UNAME_S := $(shell uname -s)
ifeq ($(UNAME_S),Linux)
        SED=sed -i''
endif
ifeq ($(UNAME_S),Darwin)
        SED=sed -i ''
endif

#####################
# Linting and Styling
#####################

weeder: ## look for unused packages and functions
	weeder . --build

hlint: ## Run hlint on all haskell projects
	stack exec hlint -- -e hs -h .hlint.yaml hs-abci-server \
	hs-tendermint-client \
	hs-abci-extra \
	hs-abci-sdk \
	hs-abci-test-utils \
	hs-abci-docs/simple-storage \
	hs-abci-docs/nameservice \
	hs-iavl-client

stylish: ## Run stylish-haskell over all haskell projects
	find ./hs-abci-types \
	./hs-abci-extra \
	./hs-tendermint-client \
	./hs-abci-examples \
	./hs-abci-sdk \
	./hs-abci-test-utils \
	./hs-abci-server \
	./hs-iavl-client \
	-name "*.hs" | xargs stack exec stylish-haskell -- -c ./.stylish_haskell.yaml -i

###################
# DOCS
###################

build-docs-local: ## Build the haddocks documentation for just this project (no dependencies)
	find . -type f -name "package.yaml" -exec $(SED) -e 's/- -fplugin=Polysemy.Plugin/- -fdefer-type-errors/g' {} + && \
	find . -type f -name "package.yaml" -exec $(SED) -e 's/- -Wall/- -fno-warn-deferred-type-errors/g' {} + && \
	stack haddock --no-haddock-deps

build-site: ## Build the tintin site
	find ./hs-abci-docs/doc/ -type f,l -name "*.md" -exec $(SED) -e 's/~~~ haskell.*/```haskell/g' {} + && \
	find ./hs-abci-docs/doc/ -type f,l -name "*.md" -exec $(SED) -e 's/~~~/```/g' {} + && \
	cd hs-abci-docs && \
	tintin run

#####################
# Core Libraries
#####################

install: ## Runs stack install to compile library and counter example app
	stack install

test-libraries: install ## Run the haskell test suite for all haskell libraries
	stack test hs-abci-types hs-abci-server hs-abci-sdk hs-abci-test-utils

test-iavl-client: ## test the iavl client library basic operation (requires grpc service running on port 8090)
	stack test hs-iavl-client


#####################
# Example Application
#####################

deploy-simple-storage-docker: install ## run the simple storage docker network
	docker-compose -f hs-abci-examples/simple-storage/docker-compose.yaml up --build

deploy-nameservice: install ## run the nameservice docker network with elk stack for logging
	docker-compose -f hs-abci-examples/nameservice/docker-compose.yaml up --build

deploy-nameservice-test: install ## run the nameservice docker network for testing
	docker-compose -f hs-abci-examples/nameservice/docker-compose-test.yaml up --build


#####################
# Tests
#####################

test-kv-store: install ## Run the test suite for the client interface
	stack test hs-tendermint-client

test-simple-storage: install ## Run the test suite for the simple-storage example application
	stack test simple-storage

test-nameservice: install ## Run the test suite for the nameservice example application
	stack test nameservice:nameservice-test

interact-nameservice: install ## Run nameservice interaction script
	INTERACT_THREAD_COUNT=$(INTERACT_THREAD_COUNT) \
	stack exec interact

test-tutorial: install ## Make sure the tutorial builds
	stack test nameservice:tutorial

#####################
# CI Support
#####################
docker-test-prebake: # Precompile all binaries externally and copy them into a docker image to speed up testing instead of building in Docker. Note that this only works on Linux systems.
	mkdir -p .ci-bins
	stack build --copy-bins --local-bin-path .ci-bins
	docker build -t hs-abci:test -f Dockerfile.prebake .
