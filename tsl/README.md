# TFT-DSL - TeamFight Tactics Domain-Specific Language

A Haskell-based DSL for encoding and parsing TFT game states in a compact, FEN-like format with an interactive REPL for dynamic game state manipulation.

## Overview

This tool provides a concise string representation of TFT game states, similar to how Chess uses FEN (Forsyth-Edwards Notation). It includes both a parse-only mode for one-off decoding and an interactive REPL for exploring and manipulating game states.

## Installation

### Quick Start (Pre-built Binaries)

Download the latest binary for your platform from [Releases](https://github.com/yourusername/tft-tools/releases):

- **Linux**: `tft-dsl-linux-x86_64`
- **macOS**: `tft-dsl-macos-x86_64` (Intel) or `tft-dsl-macos-arm64` (Apple Silicon)
- **Windows**: `tft-dsl-windows-x86_64.exe`

Make executable (Linux/macOS):
```bash
chmod +x tft-dsl-*
./tft-dsl-* --help
```

### Build from Source

**Prerequisites**: [GHCup](https://www.haskell.org/ghcup/) (Haskell toolchain)

```bash
# Install Haskell (if not already installed)
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh

# Clone and build
git clone https://github.com/yourusername/tft-tools.git
cd tft-tools/tsl

# Build using Make (recommended)
make build

# Or using cabal directly
cabal build

# Or build optimized release binary
./build-release.sh
```

**Quick Make Commands:**
```bash
make help              # Show all available commands
make build             # Build locally
make test              # Run tests
make run               # Run the REPL
make clean             # Clean build artifacts
make docker-build      # Build Linux binary with Docker
make docker-build-all  # Build all platform binaries with Docker
```

See [BUILD.md](BUILD.md) for detailed build instructions, Docker builds, and cross-compilation.

## Usage

### Parse Mode (One-off Decoding)

Decode and display a TSL string:

```bash
cabal run tft-dsl -- --parse "<game-state-string>"
```

**Example:**
```bash
cabal run tft-dsl -- --parse "7 3-5 32g 67h [c=2ANI,c=2BLI,c=1VIE,c=3ASH] [i=BFS,i=RB] [a=AA]"
```

Output:
```
=== TFT Game State ===

Player Level: 7
Stage-Round:  3-5
Gold:         32
Health:       67

Board:
  [2★] Anivia (Cost: 1)
      Traits: Freljord, Invoker
  [2★] Blitzcrank (Cost: 1)
      Traits: Zaun, Juggernaut
  [1★] Viego (Cost: 1)
      Traits: Shadow Isles, Quickstriker
  [3★] Ashe (Cost: 2)
      Traits: Freljord, Quickstriker

Items:
  Bf Sword [basic]
  Recurve Bow [basic]

Augments:
  Air Axiom [2-1; 3-2; 4-2]
```

### REPL Mode (Interactive)

Start an interactive REPL session:

```bash
# Empty game state
cabal run tft-dsl

# Or with initial state
cabal run tft-dsl "7 3-5 32g 67h [c=2ANI] [] []"
```

The REPL provides an interactive environment with colored output:

```
╔═══════════════════════════════════════════════════════════╗
║          TFT-DSL Interactive REPL                       ║
║  Type 'help' for available commands, 'quit' to exit     ║
╚═══════════════════════════════════════════════════════════╝

tft>
```

#### REPL Commands

**Display Commands:**
- **`print`** (alias: `p`) - Display the current game state as TSL
- **`help`** (aliases: `h`, `?`) - Show available commands
- **`quit`** (aliases: `exit`, `q`) - Exit the REPL

**Game State Mutation Commands:**
- **`add <entity> [*]`** (alias: `a`) - Add champion/item/augment to game state
  - Examples:
    - `add ani` - Add 1-star Anivia
    - `add 2ani` - Add 2-star Anivia
    - `add ie` - Add Infinity Edge item
    - `add aa` - Add Air Axiom augment
    - `add ani *` - Add Anivia and print the state after
  - The `*` flag automatically prints the game state after adding

- **`upgrade <entity>`** (alias: `u`) - Upgrade champion stars or combine items
  - Examples:
    - `upgrade ani` - Upgrade Anivia from 1★ to 2★ (or 2★ to 3★)
  - Champion upgrades increment star level (max 3★)
  - Item combining not yet fully implemented

- **`sell <champion>`** (alias: `s`) - Sell a champion and gain gold
  - Examples:
    - `sell ani` - Sell Anivia and gain gold
  - Gold formula: `level * championCost - (level == 1 ? 0 : 1)`
  - Updates gold counter automatically

**Game State Modification Commands:**
- **`level [n]`** (alias: `l`) - Set or increment player level
  - Examples:
    - `level` - Increment level by 1 (max 10)
    - `level 5` - Set level to 5
  - Level is clamped to 1-10

- **`round [stage round]`** (alias: `r`) - Set or increment stage-round
  - Examples:
    - `round` - Increment to next round (wraps to next stage after 7)
    - `round 3 2` - Set to stage 3, round 2
  - Automatically advances to next stage after round 7

- **`money <n>`** (alias: `m`) - Set gold amount
  - Examples:
    - `money 50` - Set gold to 50
    - `money 0` - Reset gold to 0
  - Cannot set negative gold

**Search & Discovery Commands:**
- **`find <query>`** (alias: `f`) - Search for champions/items/augments by name or shorthand
  - Examples:
    - `find ani` - Find all entities with "ani" in name or shorthand
    - `find sword` - Find all entities containing "sword"
    - `find ie` - Find by shorthand (Infinity Edge, Invoker Emblem, etc.)
  - Searches across all champions, items, and augments
  - Case-insensitive partial matching
  - Results grouped by type with color coding
  - Displays up to 10 results per category

**Tab Completion:**
The REPL includes intelligent tab completion for:
- Command names (`add`, `upgrade`, `sell`, `find`, etc.)
- Champions, items, and augments (shown as `name (SHORTHAND)`)

Press `Tab` while typing to auto-complete or see suggestions.

Tab completion matches on both full names and shorthands:
- Type `ani` + Tab → `anivia (ANI)`
- Type `ie` + Tab → Shows multiple matches: `infinity edge (IE)`, `invoker emblem (IE1)`, etc.
- Completions display the shorthand in parentheses for easy reference

**Example Session:**
```
# Starting without an initial state creates a default game state
tft> level 5
Created new game state (Level 1, Stage 1-1, 0g, 100h)
Level set to 5

tft> round 3 2
Round set to 3-2

tft> money 50
Gold set to 50

tft> add 2ani *
Added ANI

Current game state (TSL):
5 3-2 50g 100h [c=2ANI] [] []

tft> add ie *
Added IE

Current game state (TSL):
5 3-2 50g 100h [c=2ANI] [i=IE] []

tft> upgrade ani
Upgraded ANI to 3-star

tft> sell ani
Sold ANI for 4 gold
New gold: 54

tft> print
Current game state (TSL):
5 3-2 54g 100h [] [i=IE] []
```

#### Session Logging

Every REPL session is automatically logged to a timestamped file:
```
tft-session-YYYYMMDD-HHMMSS.log
```

The log includes:
- `create [initial-state]` - The initial game state (if provided)
- All commands executed during the session
- `end` - Session termination marker

This allows you to replay or review your session later.

## Testing

Run the test suite:

```bash
cabal test
```

## DSL Format

```
<level> <stage>-<round> <gold>g <health>h [<board>] [<items>] [<augments>]
```

### Components

- **Level**: Player level (1-10)
- **Stage-Round**: Current stage and round (e.g., `3-5`)
- **Gold**: Gold amount with 'g' suffix (e.g., `32g`)
- **Health**: Health amount with 'h' suffix (e.g., `67h`)
- **Board**: Champions in format `[c=<stars><shorthand>, ...]`
  - `stars`: 1, 2, or 3
  - `shorthand`: Champion code from CSV (e.g., ANI for Anivia)
  - Can be empty: `[]`
- **Items**: Items in format `[i=<shorthand>, ...]`
  - Can be empty: `[]`
- **Augments**: Augments in format `[a=<shorthand>, ...]`
  - Can be empty: `[]`

### Shorthand Codes

Champion, item, and augment shorthands are generated systematically:

- **Champions**: 2-4 character codes (ANI=Anivia, KOYU=Kobuko & Yuumi, MF=Miss Fortune)
- **Items**: 2-4 character codes (BFS=Bf Sword, IE=Infinity Edge, RB=Recurve Bow)
- **Augments**: 2-4 character codes (AA=Air Axiom, BOT=Band of Thieves)

All shorthands are available in the CSV files in `../tft-data/`:
- `set_16_champions.csv`
- `set_16_items.csv`
- `set_16_augments.csv`

## Architecture

The project consists of:

### Library Modules
1. **Types.hs**: Core data type definitions
2. **Parser.hs**: Megaparsec-based DSL parser
3. **DataLoader.hs**: CSV data loading and lookup functions
4. **CLI.hs**: Command-line argument parsing
5. **REPL.hs**: Interactive REPL implementation

### Testing
- **ParserSpec.hs**: Parser unit tests
- **REPLSpec.hs**: REPL command tests
- Uses Hspec framework with automated discovery

### Linting
- **HLint**: Static analysis configured in `.hlint.yaml`

## Data Files

The tool reads from CSV files in `tft-data/`:
- Champion data with costs, traits, and shorthands
- Item data with types and shorthands
- Augment data with tags and shorthands

## Development

### Running Tests
```bash
cabal test
```

### Running Linter
```bash
hlint src/ app/ test/
```

### Building
```bash
cabal build
```

## Future Enhancements

Planned improvements:
- Additional REPL commands for manipulating game state
- Item-to-champion assignment (e.g., `c=2ANI{IE,RB}`)
- Board positioning information
- JSON/YAML output formats
- Validation of game state legality
- Round-trip encoding (state → DSL → state)
- Game state diff/comparison commands
- Undo/redo functionality in REPL

## License

MIT
