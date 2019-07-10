help: ## Ask for help!
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'

install: ## Runs stack install to compile library and counter example app
	stack install

hlint: ## Run hlint on all haskell projects
	stack exec hlint -- -h .hlint.yaml src

test: install ## Run the haskell test suite
	stack test

stylish: ## Run stylish-haskell over all haskell projects
	find ./src -name "*.hs" | xargs stylish-haskell -c ./.stylish_haskell.yaml -i
