# TFT Tools

a collection of tools for interacting with Teamfight Tactics (TFT).

## Projects

### [tsl](./tsl)
a Haskell-based domain-specific language (DSL) for encoding and parsing TFT game states in a compact, FEN-like format. includes an interactive REPL for dynamic game state manipulation with tab completion, session logging, and commands for adding/selling champions, managing gold, and more.

### [tft-vod-analysis](./tft-vod-analysis)
a Python package for analyzing TFT gameplay videos using computer vision and deep learning. detects champions from game frames and outputs game state in TSL format. features video frame extraction, champion detection (YOLOv8 and cell-based classification), grid snapping, and synthetic data generation.

### [tft-data](./tft-data)
CSV data files for TFT Set 16 including champions, items, augments, and traits with their shorthands.

### [scripts](./scripts)
Python scripts for generating shorthand codes for champions, items, and augments from the game data.

### [examples](./examples)
example files and reference data.
