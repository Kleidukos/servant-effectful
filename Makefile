deps: ## Install the dependencies
	@cabal build all --only-dependencies

build: ## Build the project in fast mode
	@cabal build all -O0

clean: ## Remove compilation artifacts
	@cabal clean all

test: ## Run the test suite
	@cabal test all

lint: ## Run the code linter (HLint)
	@find test src -name "*.hs" | xargs -P 12 -I {} hlint --refactor-options="-i" --refactor {}
	@cabal-fmt -i servant-effectful.cabal

help:
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | awk 'BEGIN {FS = ":.* ?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'

PROCS := $(shell nproc)

.PHONY: all $(MAKECMDGOALS)

.DEFAULT_GOAL := help
