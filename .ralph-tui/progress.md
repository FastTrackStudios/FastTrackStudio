## Codebase Patterns

- **Sea-query feature flags**: Must enable `with-uuid`, `with-chrono`, `with-json` on both `sea-query` and `sea-query-binder` for Uuid/DateTime/JsonValue conversions to work.
- **Workspace glob resolution**: The `cells/*/*` glob in workspace members auto-discovers new crates — no explicit member listing needed.
- **Worktree submodule gotcha**: `git submodule update --init` can delete recently created directories in worktrees. Write files AFTER submodule initialization.
- **Pre-existing signal-proto warnings**: `signal-proto` has 8 unused import/variable warnings that cause `cargo clippy -p signal-storage -- -D warnings` to fail. The signal-storage code itself is warning-free.
- **Upstream roam-session breakage**: `roam-session` has breaking changes from `facet-path` API changes (`PathAccessError`, `walk_shape`, `ShapeVisitor` removed). Any crate depending on `actions-proto` → `roam` → `roam-session` will fail to compile. The `input` crate avoids this by defining its own `ActionId`, `KeyCode`, `Modifiers` locally.
- **Test structure**: Use `// -- Setup & Fixtures`, `// -- Exec`, `// -- Check` sections. Define `type Result<T> = core::result::Result<T, Box<dyn std::error::Error>>;` in test modules.
- **Region markers**: Use `// region: --- Name` and `// endregion: --- Name` for large code sections.
- **Builder pattern**: Use `with_*` methods on structs for fluent construction (e.g., `ModeDefinition::new(...).with_sticky(true)`).

---

## 2026-02-07 - roam-test-8xs.35
- What was implemented:
  - Created `cells/signal/signal-storage/` crate with sea-query schema and CloudStorage (PostgreSQL)
  - `schema.rs`: Sea-query table definitions for presets, snapshots, rig_configs, module_chunks, sync_metadata with indexes
  - `service.rs`: `StorageService` async trait with preset/snapshot CRUD + migrations + health check
  - `cloud.rs`: `CloudStorage` struct with sqlx PgPool connection pooling, exponential backoff retry logic for transient failures, full `StorageService` implementation with upsert (ON CONFLICT) semantics
  - `config.rs`: `CloudConfig` with pool tuning + `CloudProvider` enum generating connection strings for Supabase, Neon, Railway
  - `error.rs`: `StorageError` enum covering database, not-found, version-conflict, serialization, config, and connection errors
- Files changed:
  - `cells/signal/signal-storage/Cargo.toml` (new)
  - `cells/signal/signal-storage/src/lib.rs` (new)
  - `cells/signal/signal-storage/src/error.rs` (new)
  - `cells/signal/signal-storage/src/schema.rs` (new)
  - `cells/signal/signal-storage/src/service.rs` (new)
  - `cells/signal/signal-storage/src/config.rs` (new)
  - `cells/signal/signal-storage/src/cloud.rs` (new)
  - `Cargo.toml` (added workspace dependency for signal-storage)
- **Learnings:**
  - sea-query requires explicit feature flags for type conversions (uuid, chrono, json)
  - Worktree submodule init can nuke recently created directories — always init submodules first
  - JSON blob storage for complex nested types (Preset with snapshots, block overrides) is pragmatic; avoids complex relational mapping
  - `sea_query::Iden` derive handles CamelCase → snake_case column naming automatically
---

## 2026-02-07 - roam-test-rlo.3
- Implemented ModeStack and mode transitions in `modules/actions/input/src/mode.rs`
- Created the `input` crate from scratch since US-001 hadn't been completed yet
- Files created:
  - `modules/actions/input/Cargo.toml` (edition 2024, depends only on `tracing`)
  - `modules/actions/input/src/lib.rs` (re-exports all public types)
  - `modules/actions/input/src/key.rs` (KeyChord, KeyCode, Modifiers - local copies)
  - `modules/actions/input/src/event.rs` (InputEvent, KeyEvent, MouseEvent, ScrollEvent)
  - `modules/actions/input/src/command.rs` (InputCommand, InputArgs, ActionId - local copy)
  - `modules/actions/input/src/mode.rs` (ModeId, ModeDefinition, ModeStack with 10 tests)
- Root `Cargo.toml` updated: added workspace member and dependency for `input`
- **Learnings:**
  - `actions-proto` and `actions-keybindings` cannot be depended on due to upstream `roam-session` breakage from `facet-path` API changes
  - Defined local `ActionId`, `KeyCode`, `Modifiers` types to decouple from broken upstream chain
  - ModeStack uses owned `HashMap<ModeId, ModeDefinition>` instead of references to avoid lifetime propagation
  - `switch_base()` exits modes top-to-bottom (sub-modes first), then enters the new base
  - `pop()` is a safe no-op when only the base mode remains (stack.len() <= 1)
---

## 2026-02-08 - roam-test-d78.3
- Implemented `LayoutHistory` for undo/redo of dock layout changes
- Created `cells/dock/dock-proto/src/history.rs` with snapshot-based undo/redo
  - `LayoutHistory::new()` / `with_max_depth(n)` — configurable depth (default 50, min 1)
  - `push(before, after)` — records change, clears redo stack
  - `undo(current) -> bool` — restores before-state, moves entry to redo stack
  - `redo(current) -> bool` — restores after-state, moves entry back to undo stack
  - `can_undo()`, `can_redo()`, `undo_depth()`, `redo_depth()`, `clear()`
- Files changed:
  - `cells/dock/dock-proto/src/history.rs` (new — 230 lines, 10 unit tests + 1 doc-test)
  - `cells/dock/dock-proto/src/lib.rs` (added `pub mod history` + `pub use LayoutHistory`)
- 10 tests covering: split+undo, undo+redo, mutation-clears-redo, depth cap, empty stack edge cases, multiple undo/redo, clear, min depth clamp
- All quality gates pass: `cargo check`, `cargo clippy -- -D warnings`, `cargo test` (31 tests + 2 doc-tests)
- **Learnings:**
  - `DockLayout` doesn't derive `PartialEq` — structural comparison via panel containment checks works for tests
  - Snapshot-based history (storing `before`/`after` clones) is pragmatic when the diff system (US-002) isn't yet implemented; public API is designed to be refactored to diff-based without breaking changes
  - `drain(..excess)` removes oldest entries efficiently for depth trimming
  - US-002 (LayoutDiff) is a prerequisite per PRD but not yet implemented; history works independently with clones
---

## 2026-02-08 - roam-test-rlo.1 (verification only)
- Already implemented in previous iteration (roam-test-rlo.3, commit 0104792)
- Verified: `cargo check -p input`, `cargo clippy -p input -- -D warnings`, `cargo test -p input` all pass
- No changes needed — all acceptance criteria met
- **Learnings:**
  - When a bead's work was completed by a prior iteration (rlo.3 created the crate that rlo.1 specifies), verify and close immediately
---
