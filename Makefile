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

# Clean up project
.PHONY: clean
clean:
	go clean ./...
	rm -f cover.out
	rm -f cover.html
	rm -rf `find . -type d -name "dist"`

# Updates vendor directory and runs go mod tidy
.PHONY: vendor
vendor: ## Cleans up go mod dependencies and vendor's all dependencies
	go mod tidy
	go mod vendor

# Build the project and generate binary file
.PHONY: build
build: clean
	go build -v \
		-o ./dist/gopic \
		cmd/main.go

install: build
	chmod +x ./dist/gopic
	cp ./dist/gopic $(GOPATH)/bin/gopic

# Automated code review for Go
.PHONY: tidy
tidy: ## Reorders imports
	goimports -v -w -e . ./cmd/*

.PHONY: lint
lint: ## Runs the golangci-lint checker
	golangci-lint run -v

# Test/coverage targets #
.PHONY: test
test: # Runs unit tests and generates a coverage file at coverage.out
	go test -covermode=atomic -coverprofile=$(COVERFILE) ./...

.PHONY: cover
cover: test ## Runs unit tests and assesses output coverage file
	@echo 'cover'
	@go tool cover -func=$(COVERFILE) | $(CHECK_COVERAGE)

.PHONY: run
run: build
	chmod +x ./dist/gopic
	./dist/gopic dir -i dummy -o dummyout

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
