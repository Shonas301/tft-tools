# TFT-DSL - TeamFight Tactics Domain-Specific Language

A Haskell-based DSL for encoding and parsing TFT game states in a compact, FEN-like format with an interactive REPL for dynamic game state manipulation.

## Overview

This tool provides a concise string representation of TFT game states, similar to how Chess uses FEN (Forsyth-Edwards Notation). It includes both a parse-only mode for one-off decoding and an interactive REPL for exploring and manipulating game states.

## Installation

```bash
cabal build
```

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

- **`print`** (alias: `p`) - Display the current game state as TSL
- **`help`** (aliases: `h`, `?`) - Show available commands
- **`quit`** (aliases: `exit`, `q`) - Exit the REPL

More commands will be added in future updates.

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
