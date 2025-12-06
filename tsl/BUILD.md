# Building TFT-DSL

This document describes how to build `tft-dsl` binaries for distribution.

## Table of Contents
- [Quick Start](#quick-start)
- [Building Natively](#building-natively)
- [Building with Docker](#building-with-docker)
- [Platform-Specific Notes](#platform-specific-notes)
- [Binary Distribution](#binary-distribution)

---

## Quick Start

### Native Build (Recommended)
```bash
# Install Haskell toolchain (if not already installed)
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh

# Build optimized binary
./build-release.sh

# Binary will be in: bin/tft-dsl-<platform>-<arch>
```

### Docker Build (Cross-platform)
```bash
# Build for Linux
./build-docker.sh linux

# Build for all platforms
./build-docker.sh all
```

---

## Building Natively

### Prerequisites

1. **Install GHCup** (Haskell toolchain manager):
   ```bash
   curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
   ```

2. **Install GHC 9.6.7 and Cabal**:
   ```bash
   ghcup install ghc 9.6.7
   ghcup install cabal latest
   ghcup set ghc 9.6.7
   ```

### Build Commands

#### Development Build
```bash
cabal build
```
Binary location: `$(cabal list-bin exe:tft-dsl)`

#### Optimized Release Build
```bash
cabal build exe:tft-dsl-release
```

This build includes:
- Full optimizations (`-O2`)
- Multi-threading support
- Section splitting for smaller binaries
- Architecture-specific optimizations

#### With Static Linking (Linux only)
```bash
cabal build exe:tft-dsl-release \
    --enable-executable-static \
    --ghc-options="-optl-static -optl-pthread"
```

### Using the Build Script

The `build-release.sh` script automates the build process:

```bash
./build-release.sh
```

What it does:
1. Detects your platform (Linux/macOS/Windows)
2. Builds optimized binary
3. Copies to `bin/` directory with platform-specific name
4. Optionally strips debug symbols
5. Optionally compresses with UPX

---

## Building with Docker

Docker builds allow cross-compilation without installing toolchains.

### Requirements
- Docker installed and running
- Sufficient disk space (~5GB for builder images)

### Build for Specific Platform

```bash
# Linux x86_64
./build-docker.sh linux

# macOS x86_64
./build-docker.sh macos

# Windows x86_64
./build-docker.sh windows

# All platforms
./build-docker.sh all
```

### Manual Docker Build

```bash
# Build image
docker build -f Dockerfile.linux -t tft-dsl-builder:linux .

# Extract binary
docker create --name temp tft-dsl-builder:linux
docker cp temp:/tft-dsl-linux-x86_64 ./bin/
docker rm temp
```

---

## Platform-Specific Notes

### Linux

**Static Binary** (recommended for distribution):
```bash
cabal build exe:tft-dsl-release \
    --enable-executable-static \
    --ghc-options="-optl-static -optl-pthread"
```

This produces a fully static binary that works on any Linux distribution without dependencies.

**Dependencies** (if not building static):
- glibc (usually present)
- libgmp
- libtinfo

### macOS

**Native Build** (required for Apple Silicon):
```bash
# On M1/M2/M3 Mac
cabal build exe:tft-dsl-release
```

**Universal Binary** (Intel + Apple Silicon):
```bash
# Build for both architectures
cabal build exe:tft-dsl-release --ghc-options="-arch x86_64 -arch arm64"
```

Note: Static linking is not well-supported on macOS. The binary will depend on system libraries.

**For Intel Macs**: Use the x86_64 binary, or build natively.

**For Apple Silicon**: Build natively for best performance, or use x86_64 binary via Rosetta 2.

### Windows

**Native Build**:
```bash
cabal build exe:tft-dsl-release --ghc-options="-static -optl-static"
```

**Cross-compilation from Linux**:
Requires MinGW-w64 cross-compiler (see `Dockerfile.windows`).

**Note**: Windows binaries are typically larger due to included runtime.

---

## Binary Distribution

### Recommended Binary Sizes

After optimization and compression:
- **Linux**: 5-10 MB (static)
- **macOS**: 8-15 MB
- **Windows**: 10-20 MB

### Compression with UPX

UPX can reduce binary size by 50-70%:

```bash
# Install UPX
# Linux: apt-get install upx-ucl
# macOS: brew install upx
# Windows: choco install upx

# Compress binary
upx --best --lzma bin/tft-dsl-*
```

**Trade-offs**:
- ✅ Significantly smaller binaries
- ✅ Faster downloads
- ❌ Slightly slower startup time
- ❌ May trigger antivirus false positives

### Verifying the Binary

```bash
# Check it runs
./bin/tft-dsl-* --help

# Test parse mode
./bin/tft-dsl-* --parse "5 3-2 50g 100h [] [] []"

# Test REPL (interactive)
./bin/tft-dsl-*
```

### Distribution Checklist

Before releasing binaries:

- [ ] Test on clean system without Haskell installed
- [ ] Verify CSV data files are accessible
- [ ] Check binary size is reasonable
- [ ] Run smoke tests (--help, --parse, REPL)
- [ ] Document system requirements
- [ ] Include LICENSE and README
- [ ] Create SHA256 checksums

---

## Troubleshooting

### "Cannot find data files"

The binary needs access to CSV data files. Options:

1. **Bundle data in binary** (recommended):
   Use `file-embed` to embed CSV files at compile time.

2. **Install data files**:
   ```bash
   # Linux
   cp -r ../tft-data /usr/local/share/tft-dsl/

   # macOS
   cp -r ../tft-data ~/Library/Application\ Support/tft-dsl/

   # Windows
   cp -r ..\tft-data %APPDATA%\tft-dsl\
   ```

3. **Use relative paths**:
   Keep `tft-data/` directory next to the binary.

### "Library not found" errors

If binary is not static:

```bash
# Check dependencies
ldd bin/tft-dsl-linux-x86_64  # Linux
otool -L bin/tft-dsl-macos-*   # macOS

# Install missing libraries or rebuild with --enable-executable-static
```

### Build fails with "out of memory"

Increase GHC heap size:
```bash
cabal build --ghc-options="+RTS -M4G -RTS"
```

---

## Advanced Build Options

### Profile-Guided Optimization (PGO)

1. Build with profiling:
   ```bash
   cabal build --enable-profiling
   ```

2. Run typical workload to generate profile data

3. Rebuild with profile data:
   ```bash
   cabal build --ghc-options="-fprof-auto"
   ```

### Link-Time Optimization (LTO)

```bash
cabal build exe:tft-dsl-release \
    --ghc-options="-flto -optlo-O3"
```

Requires LLVM backend installed.

### Minimal Binary Size

```bash
cabal build exe:tft-dsl-release \
    --ghc-options="-O2 -split-sections" \
    --enable-executable-stripping

strip bin/tft-dsl-*
upx --best --lzma bin/tft-dsl-*
```

---

## CI/CD Integration

### GitHub Actions Example

```yaml
name: Build Release Binaries

on:
  push:
    tags:
      - 'v*'

jobs:
  build-linux:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3
      - uses: haskell/actions/setup@v2
      - run: ./build-release.sh
      - uses: actions/upload-artifact@v3
        with:
          name: tft-dsl-linux
          path: bin/tft-dsl-linux-*

  build-macos:
    runs-on: macos-latest
    steps:
      - uses: actions/checkout@v3
      - uses: haskell/actions/setup@v2
      - run: ./build-release.sh
      - uses: actions/upload-artifact@v3
        with:
          name: tft-dsl-macos
          path: bin/tft-dsl-macos-*

  build-windows:
    runs-on: windows-latest
    steps:
      - uses: actions/checkout@v3
      - uses: haskell/actions/setup@v2
      - run: .\build-release.sh
      - uses: actions/upload-artifact@v3
        with:
          name: tft-dsl-windows
          path: bin/tft-dsl-windows-*.exe
```

---

## Questions?

For build issues:
- Check [GitHub Issues](https://github.com/yourusername/tft-tools/issues)
- Review [Cabal User Guide](https://cabal.readthedocs.io/)
- Join Haskell community forums
