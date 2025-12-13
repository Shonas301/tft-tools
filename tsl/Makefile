# TFT-DSL Makefile
# Provides convenient commands for building, testing, and running tsl

.PHONY: help clean build build-macos docker-build docker-build-windows docker-build-all run docker-run test test-docker

# Default target - show help
.DEFAULT_GOAL := help

##@ General

help: ## Display this help message
	@echo "TFT-DSL (tsl) - Available Make Commands"
	@echo "========================================"
	@echo ""
	@awk 'BEGIN {FS = ":.*##"; printf "\nUsage:\n  make \033[36m<target>\033[0m\n"} /^[a-zA-Z_-]+:.*?##/ { printf "  \033[36m%-20s\033[0m %s\n", $$1, $$2 } /^##@/ { printf "\n\033[1m%s\033[0m\n", substr($$0, 5) } ' $(MAKEFILE_LIST)

##@ Cleanup

clean: ## Remove build artifacts and log files (preserves claude.md and progress.md)
	@echo "Cleaning build artifacts and logs..."
	rm -rf dist/
	rm -rf dist-newstyle/
	rm -rf .stack-work/
	rm -rf bin/
	rm -f *.log
	rm -f tft-session-*.log
	rm -f src/*.o
	rm -f src/*.hi
	rm -f src/*.dyn_o
	rm -f src/*.dyn_hi
	rm -f app/*.o
	rm -f app/*.hi
	rm -f test/*.o
	rm -f test/*.hi
	@echo "Clean complete"

##@ Local Build & Run

build: ## Build tsl locally using cabal
	@echo "Building tsl with cabal..."
	cabal build exe:tft-dsl

build-macos: ## Build macOS binary natively and copy to bin/
	@echo "Building macOS binary natively..."
	@mkdir -p bin
	cabal build exe:tft-dsl --ghc-options="-O2"
	@echo "Copying binary to bin/..."
	cp $$(cabal list-bin exe:tft-dsl) ./bin/tft-dsl-macos-$$(uname -m)
	@echo "✓ Built: bin/tft-dsl-macos-$$(uname -m)"
	@ls -lh bin/tft-dsl-macos-$$(uname -m)

run: ## Run tsl locally using cabal
	@echo "Running tsl..."
	cabal run tft-dsl

##@ Docker Build

docker-build: ## Build Linux binary using Docker
	@echo "Building Linux binary with Docker..."
	./build-docker.sh linux

docker-build-windows: ## Build Windows binary using Docker (requires Windows host)
	@echo "Building Windows binary with Docker..."
	@echo "Note: This requires Docker Desktop on Windows with Windows containers enabled"
	./build-docker.sh windows

docker-build-all: ## Build all Docker platform binaries (currently Linux only)
	@echo "Building all Docker platform binaries..."
	./build-docker.sh all

##@ Docker Run

docker-run: ## Run the Linux Docker image
	@echo "Running Linux Docker image..."
	@if [ ! -f bin/tft-dsl-linux-x86_64 ]; then \
		echo "Error: Linux binary not found. Run 'make docker-build' first."; \
		exit 1; \
	fi
	docker run --rm -it tft-dsl-linux /tft-dsl-linux-x86_64

##@ Testing

test: ## Run tests locally using cabal
	@echo "Running tests locally..."
	cabal test

test-docker: ## Run tests in Linux Docker container
	@echo "Running tests in Linux Docker container..."
	docker build -f Dockerfile.linux --target builder -t tft-dsl-test-linux ..
	docker run --rm tft-dsl-test-linux cabal test

test-windows: ## Run tests in Windows Docker container
	@echo "Running tests in Windows Docker container..."
	docker build -f Dockerfile.windows --target builder -t tft-dsl-test-windows ..
	docker run --rm tft-dsl-test-windows cabal test
