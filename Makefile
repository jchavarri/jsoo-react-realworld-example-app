project_name = jsoo-react-realworld-example
DUNE = opam exec -- dune
opam_file = $(project_name).opam
current_hash = $(shell git rev-parse HEAD)

.DEFAULT_GOAL := help

.PHONY: help
help: ## Print this help message
	@echo "List of available make commands";
	@echo "";
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | awk 'BEGIN {FS = ":.*?## "}; {printf "  \033[36m%-15s\033[0m %s\n", $$1, $$2}';
	@echo "";

.PHONY: create-switch
create-switch:
	opam switch create . 4.13.1 --deps-only

.PHONY: init
init: install pins ## Configure everything to develop this repository in local
	yarn

.PHONY: pins
pins: ## Pin development dependencies
	opam pin add $(project_name).dev .

.PHONY: install
install: ## Install development dependencies
	opam install . --deps-only --with-test

.PHONY: deps
deps: $(opam_file) ## Alias to update the opam file and install the needed deps

.PHONY: build
build: ## Build the project
	$(DUNE) build @@default

.PHONY: build-prod
build-prod: ## Build the project with prod mode
	$(DUNE) build @@default --profile=prod

.PHONY: start
start: ## Serve the application with a local HTTP server
	yarn server

.PHONY: clean
clean: ## Clean build artifacts and other generated files
	$(DUNE) clean

.PHONY: format
format: ## Format the codebase with ocamlformat
	$(DUNE) build @fmt --auto-promote

.PHONY: format-check
format-check: ## Checks if format is correct
	$(DUNE) build @fmt

.PHONY: watch
watch: ## Watch for the filesystem and rebuild on every change
	$(DUNE) build @@default --watch

.PHONY: publish
publish: ## Publish to gh-pages
	git checkout main && $(DUNE) build --profile=prod @@default && yarn webpack:production \
	&& git checkout gh-pages && cp build/* . && git commit -am "$(current_hash)"

$(opam_file): $(project_name).opam.template dune-project ## Update the package dependencies when new deps are added to dune-project
	opam exec -- dune build @install        # Update the $(project_name).opam file

