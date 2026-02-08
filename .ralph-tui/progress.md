## Codebase Patterns

- **Sea-query feature flags**: Must enable `with-uuid`, `with-chrono`, `with-json` on both `sea-query` and `sea-query-binder` for Uuid/DateTime/JsonValue conversions to work.
- **Workspace glob resolution**: The `cells/*/*` glob in workspace members auto-discovers new crates — no explicit member listing needed.
- **Worktree submodule gotcha**: `git submodule update --init` can delete recently created directories in worktrees. Write files AFTER submodule initialization.
- **Pre-existing signal-proto warnings**: `signal-proto` has 8 unused import/variable warnings that cause `cargo clippy -p signal-storage -- -D warnings` to fail. The signal-storage code itself is warning-free.
- **Upstream roam-session breakage**: `roam-session` has breaking changes from `facet-path` API changes (`PathAccessError`, `walk_shape`, `ShapeVisitor` removed). Any crate depending on `actions-proto` → `roam` → `roam-session` will fail to compile. The `input` crate avoids this by defining its own `ActionId`, `KeyCode`, `Modifiers` locally.
- **Test structure**: Use `// -- Setup & Fixtures`, `// -- Exec`, `// -- Check` sections. Define `type Result<T> = core::result::Result<T, Box<dyn std::error::Error>>;` in test modules.
- **Region markers**: Use `// region: --- Name` and `// endregion: --- Name` for large code sections.
- **Builder pattern**: Use `with_*` methods on structs for fluent construction (e.g., `ModeDefinition::new(...).with_sticky(true)`).
- **Let-chains in Rust 2024**: Clippy enforces collapsing nested `if let` into let-chains (`if let Some(x) = y && let Foo(z) = x.bar`). Use this pattern instead of nested ifs.
- **Removing unused imports**: When removing an import from production code, check if tests use `super::*` and rely on that import — add it to the test module's `use` block instead.

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

## 2026-02-08 - roam-test-rlo.5
- What was implemented:
  - `InputProcessor` core state machine in `modules/actions/input/src/processor.rs`
  - `KeyTrie` prefix tree for multi-key sequence lookup in `src/trie.rs`
  - `ActionContext` lightweight context for when-clause evaluation in `src/context.rs`
  - `SequenceState` accumulator for pending key sequences (internal to processor)
  - Full Escape handling: cancels pending sequences, pops sub-modes, or switches to Normal
  - Insert mode passthrough: unmatched character keys in passthrough modes → InsertText
  - Mode transition side effects: SwitchMode/PushMode/PopMode commands trigger ModeStack transitions and emit on_enter/on_exit actions
  - Timeout support: `timeout_expired()`, `needs_timeout()`, `pending_display()`
  - 11 unit tests for processor (single key, two-key sequence, mode switch, insert passthrough, escape, timeout, non-key events, unmatched keys, sequence miss)
  - 4 unit tests for trie (single key, multi-key, modifiers, mode switch binding)
- Files created:
  - `modules/actions/input/src/processor.rs` (InputProcessor, SequenceState, 11 tests)
  - `modules/actions/input/src/trie.rs` (KeyTrie, TrieLookup, 4 tests)
  - `modules/actions/input/src/context.rs` (ActionContext)
- Files modified:
  - `modules/actions/input/src/lib.rs` (added module declarations and re-exports)
- **Learnings:**
  - Clippy in Rust 2024 enforces let-chains — nested `if let` patterns must be collapsed
  - `super::*` in test modules inherits production imports; removing an import from production code can break tests
  - Trie with `Clone`-based matching (returning owned values) avoids lifetime propagation through the processor
  - `execute_command` returns the original SwitchMode/PushMode command PLUS the on_enter/on_exit actions, so the caller gets the full command stream
---
