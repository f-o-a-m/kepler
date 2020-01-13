STATS_PORT ?= 9200
INTERACT_THREAD_COUNT ?= 10

export

# This is useful for copying example app binaries built on a linux machine rather than building in docker
SIMPLE_STORAGE_BINARY := $(shell stack exec -- which simple-storage)

help: ## Ask for help!
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'

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
	hs-abci-examples/simple-storage \
	hs-abci-examples/nameservice \
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
	stack haddock --no-haddock-deps

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
	docker-compose -f hs-abci-examples/simple-storage/docker-compose-elk.yaml up --build

deploy-nameservice-elk-docker: install ## run the nameservice docker network with elk stack for logging
	docker-compose -f hs-abci-examples/nameservice/docker-compose-elk.yaml up --build

deploy-nameservice-docker: install ## run the nameservice docker network
	docker-compose -f hs-abci-examples/nameservice/docker-compose.yaml up --build

deploy-simple-storage-local: install ## run the simple storage locally
	STATS_PORT=$(STATS_PORT) \
	stack exec simple-storage

deploy-nameservice-local: install ## run the nameservice locally
	STATS_PORT=$(STATS_PORT) \
	stack exec nameservice

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
