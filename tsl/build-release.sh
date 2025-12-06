#!/bin/bash
# Build release binaries for tft-dsl
# This script builds optimized binaries for distribution

set -e  # Exit on error

echo "=== TFT-DSL Release Builder ==="
echo ""

# Detect platform
case "$(uname -s)" in
    Linux*)     PLATFORM=linux;;
    Darwin*)    PLATFORM=macos;;
    CYGWIN*|MINGW*|MSYS*) PLATFORM=windows;;
    *)          PLATFORM=unknown;;
esac

echo "Detected platform: $PLATFORM"
echo ""

# Create output directory
mkdir -p bin

# Clean previous builds
echo "Cleaning previous builds..."
cabal clean
echo ""

# Update dependencies
echo "Updating dependencies..."
cabal update
echo ""

# Build optimized release binary
echo "Building optimized release binary..."
if [ "$PLATFORM" = "linux" ]; then
    # Linux: Try to build static binary
    echo "Building static binary for Linux..."
    cabal build exe:tft-dsl-release \
        --enable-executable-static \
        --ghc-options="-optl-static -optl-pthread"

elif [ "$PLATFORM" = "macos" ]; then
    # macOS: Build normal binary (static linking not well supported)
    echo "Building binary for macOS..."
    cabal build exe:tft-dsl-release

elif [ "$PLATFORM" = "windows" ]; then
    # Windows: Build with static runtime
    echo "Building binary for Windows..."
    cabal build exe:tft-dsl-release \
        --ghc-options="-static -optl-static -optl-pthread"
else
    echo "Unknown platform, building normal binary..."
    cabal build exe:tft-dsl-release
fi

# Copy binary to bin directory
echo ""
echo "Copying binary to bin/ directory..."
BINARY_PATH=$(cabal list-bin exe:tft-dsl-release)
BINARY_NAME="tft-dsl-$PLATFORM-$(uname -m)"

# On Windows, add .exe extension
if [ "$PLATFORM" = "windows" ]; then
    BINARY_NAME="$BINARY_NAME.exe"
fi

cp "$BINARY_PATH" "bin/$BINARY_NAME"

# Get binary size
BINARY_SIZE=$(du -h "bin/$BINARY_NAME" | cut -f1)

echo ""
echo "=== Build Complete ==="
echo "Binary: bin/$BINARY_NAME"
echo "Size: $BINARY_SIZE"
echo ""
echo "Test it with:"
echo "  ./bin/$BINARY_NAME --help"
echo "  ./bin/$BINARY_NAME --parse \"5 3-2 50g 100h [] [] []\""
echo ""

# Optional: Strip binary to reduce size
if command -v strip &> /dev/null; then
    echo "Stripping debug symbols..."
    strip "bin/$BINARY_NAME" || true
    STRIPPED_SIZE=$(du -h "bin/$BINARY_NAME" | cut -f1)
    echo "Stripped size: $STRIPPED_SIZE"
    echo ""
fi

# Optional: Compress with UPX if available
if command -v upx &> /dev/null; then
    echo "Compressing with UPX..."
    cp "bin/$BINARY_NAME" "bin/$BINARY_NAME.uncompressed"
    upx --best --lzma "bin/$BINARY_NAME" || true
    COMPRESSED_SIZE=$(du -h "bin/$BINARY_NAME" | cut -f1)
    echo "Compressed size: $COMPRESSED_SIZE"
    echo ""
fi

echo "Done!"
