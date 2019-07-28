help: ## Ask for help!
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'

build-docs-local: ## Build the haddocks documentation for just this project (no dependencies)
	stack haddock --no-haddock-deps

install: ## Runs stack install to compile library and counter example app
	stack install

hlint: ## Run hlint on all haskell projects
	stack exec hlint -- -h .hlint.yaml hs-abci-server
	stack exec hlint -- -h .hlint.yaml hs-abci-extra

test: install ## Run the haskell test suite for all haskell projects
	stack test

stylish: ## Run stylish-haskell over all haskell projects
	find ./hs-abci-server -name "*.hs" | xargs stylish-haskell -c ./.stylish_haskell.yaml -i
	find ./hs-abci-extra -name "*.hs" | xargs stylish-haskell -c ./.stylish_haskell.yaml -i
