help: ## Ask for help!
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'

export

# This is useful for copying example app binaries built on a linux machine rather than building in docker
SIMPLE_STORAGE_BINARY := $(shell stack exec -- which simple-storage)

#####################
# Linting and Styling
#####################

hlint: ## Run hlint on all haskell projects
	stack exec hlint -- -h .hlint.yaml hs-abci-server hs-abci-example hs-tendermint-client hs-abci-extra

stylish: ## Run stylish-haskell over all haskell projects
	find ./hs-abci-types ./hs-abci-extra ./hs-tendermint-client ./hs-abci-example ./hs-abci-server -name "*.hs" | xargs stack exec stylish-haskell -- -c ./.stylish_haskell.yaml -i

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
	stack test hs-abci-types hs-abci-server


#####################
# Example Application
#####################

deploy-simple-storage-docker: install ## run the simple storage docker network
	docker-compose -f hs-abci-example/docker-compose.yaml up --build -d

deploy-simple-storage-local: install ## run the simple storage locally
	stack exec simple-storage

test-simple-storage: install ## Run the test suite for the example application
	stack test hs-abci-example
