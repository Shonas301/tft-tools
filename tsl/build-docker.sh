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
    docker create --name tft-dsl-temp "tft-dsl-builder:$platform"
    docker cp "tft-dsl-temp:/$output_binary" "./bin/$output_binary"
    docker rm tft-dsl-temp

    echo "✓ Built: bin/$output_binary"
    echo ""
}

# Parse command line arguments
if [ $# -eq 0 ]; then
    echo "Usage: $0 [linux|macos|windows|all]"
    echo ""
    echo "Examples:"
    echo "  $0 linux          # Build Linux binary"
    echo "  $0 all            # Build all platforms"
    exit 1
fi

TARGET=$1

case "$TARGET" in
    linux)
        build_platform "linux" "Dockerfile.linux" "tft-dsl-linux-x86_64"
        ;;
    macos)
        build_platform "macos" "Dockerfile.macos" "tft-dsl-macos-x86_64"
        ;;
    windows)
        build_platform "windows" "Dockerfile.windows" "tft-dsl-windows-x86_64.exe"
        ;;
    all)
        echo "Building for all platforms..."
        echo ""
        build_platform "linux" "Dockerfile.linux" "tft-dsl-linux-x86_64"
        build_platform "macos" "Dockerfile.macos" "tft-dsl-macos-x86_64"
        build_platform "windows" "Dockerfile.windows" "tft-dsl-windows-x86_64.exe"
        ;;
    *)
        echo "Unknown platform: $TARGET"
        echo "Valid options: linux, macos, windows, all"
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
