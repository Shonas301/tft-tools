#!/bin/bash
# Build tft-dsl binaries using Docker
# This allows building for different platforms without installing toolchains

set -e

echo "=== TFT-DSL Docker Builder ==="
echo ""

# Create output directory
mkdir -p bin

# Function to build and extract binary
build_platform() {
    local platform=$1
    local dockerfile=$2
    local output_binary=$3

    echo "Building for $platform..."
    docker build -f "$dockerfile" -t "tft-dsl-builder:$platform" .

    echo "Extracting binary..."
    # Use a dummy command for scratch-based images
    docker create --name tft-dsl-temp "tft-dsl-builder:$platform" sh 2>/dev/null || \
        docker create --name tft-dsl-temp "tft-dsl-builder:$platform"
    docker cp "tft-dsl-temp:/$output_binary" "./bin/$output_binary"
    docker rm tft-dsl-temp >/dev/null

    echo "✓ Built: bin/$output_binary"
    echo ""
}

# Parse command line arguments
if [ $# -eq 0 ]; then
    echo "Usage: $0 [linux|windows|all]"
    echo ""
    echo "Examples:"
    echo "  $0 linux          # Build Linux binary (works on any Docker host)"
    echo "  $0 windows        # Build Windows binary (requires Windows + Docker Desktop)"
    echo "  $0 all            # Build all Docker-supported platforms (currently just Linux)"
    echo ""
    echo "Note: Platform-specific requirements:"
    echo "  macOS:   Build natively using 'make build-macos'"
    echo "  Windows: Requires Windows host with Docker Desktop (Windows containers mode)"
    exit 1
fi

TARGET=$1

case "$TARGET" in
    linux)
        build_platform "linux" "Dockerfile.linux" "tft-dsl-linux-x86_64"
        ;;
    windows)
        echo "Building Windows binary..."
        echo ""
        echo "Note: Windows Docker builds require:"
        echo "  - Docker Desktop on Windows"
        echo "  - Windows containers enabled (not Linux containers)"
        echo ""
        read -p "Continue with Windows build? (y/n) " -n 1 -r
        echo
        if [[ ! $REPLY =~ ^[Yy]$ ]]; then
            echo "Build cancelled."
            exit 1
        fi
        build_platform "windows" "Dockerfile.windows" "tft-dsl-windows-x86_64.exe"
        ;;
    all)
        echo "Building for all platforms..."
        echo ""
        build_platform "linux" "Dockerfile.linux" "tft-dsl-linux-x86_64"
        echo ""
        echo "Linux build complete!"
        echo ""
        echo "Note: Additional platform builds:"
        echo "  macOS:   make build-macos (native build required)"
        echo "  Windows: ./build-docker.sh windows (requires Windows + Docker Desktop)"
        ;;
    macos)
        echo "Error: macOS Docker builds are not supported."
        echo "Please build natively on macOS using:"
        echo "  make build-macos"
        echo ""
        echo "Or manually:"
        echo "  cabal build exe:tft-dsl --ghc-options=\"-O2\""
        echo "  cp \$(cabal list-bin exe:tft-dsl) ./bin/tft-dsl-macos-\$(uname -m)"
        exit 1
        ;;
    *)
        echo "Unknown platform: $TARGET"
        echo "Valid options: linux, windows, all"
        echo ""
        echo "Note:"
        echo "  macOS builds must be done natively (make build-macos)"
        echo "  Windows builds require Windows host with Docker Desktop"
        exit 1
        ;;
esac

echo "=== Build Complete ==="
echo ""
echo "Binaries available in bin/ directory:"
ls -lh bin/
echo ""
echo "Note: Cross-platform builds may have limitations."
echo "For best results, build natively on the target platform."
