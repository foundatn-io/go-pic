COVERFILE=cover.out
COVERHTML=cover.html
COLOUR_NORMAL=$(shell tput sgr0)
COLOUR_RED=$(shell tput setaf 1)
COLOUR_GREEN=$(shell tput setaf 2)
COVERAGE=$(shell cat THRESHOLD)
DOCKER_REGISTRY?=library

default: | clean vendor tidy lint cover
	@if [[ -e .git/rebase-merge ]]; then git --no-pager log -1 --pretty='%h %s'; fi
	@printf '%sSuccess%s\n' "${COLOUR_GREEN}" "${COLOUR_NORMAL}"

define HEADER
  ______  _____  _____  ____  ______
 |   ___|/     \|     ||    ||   ___|
 |   |  ||  O  ||    _||    ||   |__
 |______|\_____/|___|  |____||______|

endef
export HEADER

.PHONY: help
help: ## Prints help text
	@echo "$$HEADER"
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-25s\033[0m %s\n", $$1, $$2}'
	@echo "Coverage Required: ${COLOUR_GREEN}${COVERAGE}%${COLOUR_NORMAL}"

.PHONY: clean
clean: ## Cleans up generated coverage files and binaries
	go clean ./...
	rm -f cover.out
	rm -f cover.html
	rm -rf `find . -type d -name "dist"`

.PHONY: vendor
vendor: ## Cleans up go mod dependencies and vendor's all dependencies
	go mod tidy
	go mod vendor

.PHONY: build
build: clean ## Builds the gopic struct generation tool
	go build -v \
		-o ./dist/gopic \
		cmd/main.go

install: build ## Builds and installs gopic struct generation tool to your GOPATH
	chmod +x ./dist/gopic
	cp ./dist/gopic $(GOPATH)/bin/gopic

.PHONY: tidy
tidy: ## Reorders imports
	goimports -v -w -e . ./cmd/*

.PHONY: lint
lint: ## Runs the golangci-lint checker
	go fmt ./...
	golangci-lint run -v

.PHONY: test
test: ## Runs unit tests and generates a coverage file at coverage.out
	go test `go list ./... | grep -vE "./example|./cmd/pkg/cli"` -race -covermode=atomic -coverprofile=$(COVERFILE)

.PHONY: cover
cover: test ## Runs unit tests and assesses output coverage file
	@echo 'cover'
	@go tool cover -func=$(COVERFILE) | $(CHECK_COVERAGE)

.PHONY: example
eg: install ## Builds & installs the gopic struct generation tool, regenerates example files
	gopic file -d -p main -o example/example.go -i example/ExampleCopybook.txt

.PHONY: example
eg-dir: install ## Builds & installs the gopic struct generation tool, regenerates example files
	gopic dir -d -p main -o example/egdirout -i example/egdirin

define CHECK_COVERAGE
awk \
  -F '[ 	%]+' \
  -v threshold="$(COVERAGE)" \
  '/^total:/ { print; if ($$3 < threshold) { exit 1 } }' || { \
  	printf '%sFAIL - Coverage below %s%%%s\n' \
  	  "$(COLOUR_RED)" "$(COVERAGE)" "$(COLOUR_NORMAL)"; \
    exit 1; \
  }
endef
