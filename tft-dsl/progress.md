# TFT-DSL Development Progress

## Session: Game State Mutation Commands
**Started:** 2025-12-05

### Overview
Adding interactive commands to modify game state in the REPL, including add, upgrade, sell, and find/autocomplete functionality.

---

## Task Breakdown

### Phase 1: Core State Mutation Infrastructure
- [ ] Make GameState mutable in REPL (use IORef or State monad)
- [ ] Add TSL encoder (GameState -> Text)
- [ ] Update REPL to track and modify state
- [ ] Add tests for state mutations

### Phase 2: Add Command (`add|a`)
- [ ] Parse add command with CIA type detection
- [ ] Implement addChampion with star level
- [ ] Implement addItem to items list
- [ ] Implement addAugment to augments list
- [ ] Handle `*` suffix to print state after add
- [ ] Add validation (max augments, board size, etc.)
- [ ] Tests for add command

### Phase 3: Upgrade Command (`upgrade|u`)
- [ ] Implement champion upgrade (increment stars, handle duplicates)
- [ ] Implement item upgrade (combine components -> combined item)
- [ ] Load item combination recipes from CSV
- [ ] Error handling for invalid upgrades
- [ ] Tests for upgrade command

### Phase 4: Sell Command (`sell|s`)
- [ ] Implement sell logic: remove champion, calculate gold
- [ ] Formula: gold += (level * cost - (level == 1 ? 0 : 1))
- [ ] Update game state gold counter
- [ ] Error handling for champion not found
- [ ] Tests for sell command

### Phase 5: Find/Autocomplete (`find|f`)
**Decision Point:** Tab autocomplete vs find command

**Option A: Find Command (Simpler)**
- [ ] Implement fuzzy string matching
- [ ] Search across all CIA data
- [ ] Display matches with type and shorthand
- [ ] Limit results to top 10 matches

**Option B: Tab Autocomplete (More Complex)**
- [ ] Integrate Haskeline tab completion
- [ ] Build completion dictionary from CSV data
- [ ] Implement cyclic tab completion
- [ ] Context-aware completion (after add/upgrade/sell)

**Chosen Approach:** Starting with Find command (Option A), can upgrade to autocomplete later

### Phase 6: Integration & Polish
- [ ] Update help command with new commands
- [ ] Update session logging for all new commands
- [ ] Integration tests for complex workflows
- [ ] Update README with examples
- [ ] Code review and cleanup

---

## Progress Updates

### Design Decisions Made

**Autocomplete:** Both find command AND tab-autocomplete (Option C)
**Add Syntax:** `add ANI` (default 1-star) or `add 2ANI` (explicit stars)
**Print Flag:** `add 2ANI *` (space-separated)
**Item Upgrade:** Track components, auto-assume missing component if one exists

### [Phase 1: Starting]
Implementing mutable state with IORef...
