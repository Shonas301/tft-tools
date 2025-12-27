# TFT DSL Schema

## Overview
A FEN-like encoding for TFT game states that allows compact, human-readable representation of board positions.

## Format

```
<level> <stage>-<round> <gold>g <health>h [<board>] [<items>] [<augments>]
```

## Components

### Basic State
- `<level>`: Player level (1-10)
- `<stage>-<round>`: Current stage and round (e.g., `3-2`, `5-7`)
- `<gold>`: Gold amount followed by 'g' (e.g., `45g`)
- `<health>`: Health amount followed by 'h' (e.g., `80h`)

### Board (Champions) - FEN Notation

The board uses a FEN-like notation that encodes champion positions on the board grid and bench.

**Board Structure:**
- Board: 4 rows × 7 columns (row 0-3, column 0-6)
- Bench: 1 row × 9 slots (row 4, column 0-8)
- Total: 5 rows in the encoding

**FEN Format:** `[row0/row1/row2/row3/bench]`

Each row contains cells separated by `|`:
- Champion cell: `<stars><SHORTHAND>` (e.g., `2ANI` for 2-star Anivia)
- Empty cells: A number indicating consecutive empty cells (1-7 for board rows, 1-9 for bench)

**Examples:**

Empty board:
```
[7/7/7/7/9]
```

Champion at position (0,0) - top-left corner:
```
[2ANI|6/7/7/7/9]
```
- Row 0: 2-star Anivia at col 0, then 6 empty cells
- Rows 1-3: All empty (7 each)
- Bench: All empty (9)

Multiple champions in different positions:
```
[1ANI|1|2BLI|4/7/3|1LUX|3/7/9]
```
- Row 0: 1★ Anivia at (0,0), empty at (0,1), 2★ Blitzcrank at (0,2), 4 empty (0,3-0,6)
- Row 1: All empty
- Row 2: 3 empty (2,0-2,2), 1★ Lux at (2,3), 3 empty (2,4-2,6)
- Row 3: All empty
- Bench: All empty

Champion on bench:
```
[7/7/7/7/2|3SEJ|6]
```
- Board rows 0-3: All empty
- Bench: 2 empty slots, then 3★ Sejuani at slot 2, then 6 empty slots

### Legacy Board Format (Backward Compatible)

The parser also supports the legacy format for backward compatibility:

Format: `[c=<star><shorthand>, ...]`

- `<star>`: Champion star level (1, 2, or 3)
- `<shorthand>`: Champion shorthand code (from CSV)

Examples:
- `c=1ANI` - 1-star Anivia
- `c=2BLI` - 2-star Blitzcrank
- `c=3KOYU` - 3-star Kobuko & Yuumi

Multiple champions separated by commas:
`[c=2ANI,c=1BLI,c=3VIE]`

**Note:** When using legacy format, champions are auto-placed in first available positions (left-to-right, top-to-bottom on the board).

### Items
Format: `[i=<shorthand>, ...]`

- `<shorthand>`: Item shorthand code (to be generated)

Examples:
- `i=IE` - Infinity Edge
- `i=RB` - Rabadon's Deathcap

Multiple items:
`[i=IE,i=RB,i=GA]`

### Augments
Format: `[a=<shorthand>, ...]`

- `<shorthand>`: Augment shorthand code (to be generated)

Examples:
- `a=HG` - Hedge Fund
- `a=CB` - Combat Training

Multiple augments (max 3):
`[a=HG,a=CB,a=TR]`

## Complete Example

```
7 3-5 32g 67h [c=2ANI,c=2BLI,c=1VIE,c=3ASH] [i=IE,i=RB,i=GA] [a=HG,a=CB]
```

This represents:
- Level 7 player
- Stage 3, Round 5
- 32 gold
- 67 health
- Board: 2-star Anivia, 2-star Blitzcrank, 1-star Viego, 3-star Ashe
- Items: Infinity Edge, Rabadon's, Guardian Angel
- Augments: Hedge Fund, Combat Training

## Parsing Rules

1. Fields are whitespace-delimited
2. Board, items, and augments are enclosed in square brackets
3. Multiple entries within brackets are comma-separated
4. No spaces within brackets
5. Case-insensitive for shorthands (will be normalized to uppercase)

## Extended Format (Optional)

For more detailed encoding, items can be associated with champions:

```
[c=2ANI{IE,RB},c=2BLI{GA}]
```

This indicates Anivia has IE and RB equipped, Blitzcrank has GA.
