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

### Board (Champions)
Format: `[c=<star><shorthand>, ...]`

- `<star>`: Champion star level (1, 2, or 3)
- `<shorthand>`: Champion shorthand code (from CSV)

Examples:
- `c=1ANI` - 1-star Anivia
- `c=2BLI` - 2-star Blitzcrank
- `c=3KOYU` - 3-star Kobuko & Yuumi

Multiple champions separated by commas:
`[c=2ANI,c=1BLI,c=3VIE]`

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
