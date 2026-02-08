## Codebase Patterns

- **Sea-query feature flags**: Must enable `with-uuid`, `with-chrono`, `with-json` on both `sea-query` and `sea-query-binder` for Uuid/DateTime/JsonValue conversions to work.
- **Workspace glob resolution**: The `cells/*/*` glob in workspace members auto-discovers new crates — no explicit member listing needed.
- **Worktree submodule gotcha**: `git submodule update --init` can delete recently created directories in worktrees. Write files AFTER submodule initialization.
- **Pre-existing signal-proto warnings**: `signal-proto` has 8 unused import/variable warnings that cause `cargo clippy -p signal-storage -- -D warnings` to fail. The signal-storage code itself is warning-free.
- **Upstream roam-session breakage**: `roam-session` has breaking changes from `facet-path` API changes (`PathAccessError`, `walk_shape`, `ShapeVisitor` removed). Any crate depending on `actions-proto` → `roam` → `roam-session` will fail to compile. The `input` crate avoids this by defining its own `ActionId`, `KeyCode`, `Modifiers` locally.
- **Test structure**: Use `// -- Setup & Fixtures`, `// -- Exec`, `// -- Check` sections. Define `type Result<T> = core::result::Result<T, Box<dyn std::error::Error>>;` in test modules.
- **Region markers**: Use `// region: --- Name` and `// endregion: --- Name` for large code sections.
- **Builder pattern**: Use `with_*` methods on structs for fluent construction (e.g., `ModeDefinition::new(...).with_sticky(true)`).
- **Edition 2024 match ergonomics**: No explicit `ref` in patterns matching on `&T` — the compiler infers borrows automatically. Use `Some(KeyTrie::Leaf(LeafAction::Operator(s)))` not `ref s`.
- **Lost work across worktrees**: Bead marked complete doesn't mean the code exists on current branch. Always verify files exist before depending on prior beads.
- **Workspace test workaround**: `cargo test -p dock-proto` may fail due to broken workspace members (sync-proto missing src). Use `--manifest-path` to test isolated crates.
- **PanelId string bridge**: `PanelId::as_str()` returns stable kebab-case identifiers, `PanelId::from_str_id()` does reverse lookup, `PanelId::register_all()` seeds a PanelRegistry with all built-in panels.
- **SQLite UUID storage**: SQLite has no native UUID type — UUIDs are stored as TEXT strings. When reading back, use `row.get::<String>("id")` then `.parse::<Uuid>()`, not `row.get::<Uuid>("id")`.
- **FTS5 content-sync triggers**: FTS5 `content=table` requires INSERT/UPDATE/DELETE triggers to keep the index in sync. The special `'delete'` command removes entries: `INSERT INTO fts(fts, rowid, ...) VALUES ('delete', old.rowid, ...)`.
- **SqlitePool max_connections=1**: SQLite works best with a single writer connection. Set `max_connections(1)` on `SqlitePoolOptions` to avoid SQLITE_BUSY contention.

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

## 2026-02-08 - roam-test-rlo.4
- What was implemented:
  - US-004: Sequence accumulator with timeout for multi-key bindings
  - Also implemented US-002's KeyTrie (prerequisite lost due to worktree race)
  - `trie.rs`: `KeyTrie` enum (Leaf/Node), `TrieNode` with insert/get/merge, `LeafAction` with 8 variants (Action, SwitchMode, PushMode, Operator, Motion, TextObject, Sequence, Unbind)
  - `sequence.rs`: `SequenceState` accumulator with feed/timeout_expired/is_pending/pending_display/reset. `SequenceResult` enum: Matched/Pending/NoMatch/Timeout
  - `format_chord()` helper for human-readable key display (e.g., `<C-g>`)
- Files changed:
  - `modules/actions/input/src/trie.rs` (new — 340+ lines with 10 tests)
  - `modules/actions/input/src/sequence.rs` (new — 260+ lines with 10 tests)
  - `modules/actions/input/src/lib.rs` (added trie + sequence modules and re-exports)
  - `cells/sync/sync-ui/src/lib.rs` (stub — fixed broken workspace member)
  - `cells/sync/sync/src/lib.rs` (stub — fixed broken workspace member)
- **Learnings:**
  - Edition 2024 disallows explicit `ref` in patterns matching on `&T` — compiler infers borrows automatically
  - Bead "closed" doesn't mean code exists on current branch — worktree workers operate on separate branches, code can be lost if not merged
  - `SequenceResult` uses lifetime `'a` for `Pending(&'a TrieNode)` to avoid cloning trie nodes; `Timeout` variant uses `'static` since it only carries owned `Vec<KeyChord>`
  - Broken workspace members (sync-ui, sync missing src/) need stub lib.rs for `cargo clippy -p input` to work since it resolves the full workspace manifest
---

## 2026-02-08 - roam-test-d78.4
- What was implemented:
  - US-004: Dynamic panel registry trait for dock-proto
  - `PanelConstraints` struct with builder API (min/max/preferred width/height, resize priority)
  - `DockPosition` enum (Left, Right, Bottom, Center, Float)
  - `PanelDescriptor` struct with builder API (id, display_name, icon, category, default_position, constraints)
  - `PanelRegistry` struct: HashMap-backed with register(), unregister(), get(), all(), all_ids(), by_category(), contains(), len(), is_empty()
  - `PanelId::as_str()` / `from_str_id()` for string-based ID bridge
  - `PanelId::register_all()` seeds registry with all 15 built-in panels
  - `From<PanelId> for String` and `Display for PanelId` impls
- Files changed:
  - `cells/dock/dock-proto/src/registry.rs` (new — 450+ lines with 15 tests)
  - `cells/dock/dock-proto/src/panel.rs` (added as_str, from_str_id, register_all, Display, Into<String>)
  - `cells/dock/dock-proto/src/lib.rs` (added registry module + re-exports)
- **Learnings:**
  - Migrating `TabGroup.panels: Vec<PanelId>` to `Vec<String>` would break serde compatibility and the entire dock-dioxus layer — better to keep type-safe PanelId internally and bridge via `Into<String>`
  - `cargo test -p dock-proto` fails in workspace mode due to broken sync-proto member; use `--manifest-path` instead
  - PanelId categories: Session (5), Signal (6), DAW (2), Utility (2) — matches the existing enum comment groupings
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

## 2026-02-08 - roam-test-8xs.31
- What was implemented:
  - US-026: LocalStorage with SQLite backend for fast local persistence
  - `local_config.rs`: `LocalConfig` with `dirs` crate for platform-appropriate paths (Linux/macOS/Windows), WAL mode, busy timeout config
  - `local.rs`: `LocalStorage` struct implementing full `StorageService` trait via `SqliteQueryBuilder`
  - FTS5 full-text search: virtual table `presets_fts` with content-sync triggers for INSERT/UPDATE/DELETE
  - `list_presets_fts()` method uses FTS5 MATCH + rank ordering for fast text search
  - Row extraction helpers for SQLite (UUID as TEXT, JSON as TEXT)
  - 15 unit tests: CRUD, upsert, soft-delete, list filtering, FTS5 name/description search, FTS5 deleted filter
- Files changed:
  - `cells/signal/signal-storage/src/local.rs` (new — 500+ lines with 15 tests)
  - `cells/signal/signal-storage/src/local_config.rs` (new — 95 lines with 4 tests)
  - `cells/signal/signal-storage/src/lib.rs` (added local + local_config modules and re-exports)
  - `cells/signal/signal-storage/Cargo.toml` (added `dirs = "6.0"` dependency)
- **Learnings:**
  - SQLite stores UUIDs as TEXT — must parse with `String::parse::<Uuid>()` on read, not `row.get::<Uuid>()`
  - FTS5 content-sync (`content=presets`) requires manual triggers; the special `'delete'` command syntax is `INSERT INTO fts(fts, rowid, ...) VALUES ('delete', ...)`
  - `SqlitePool::max_connections(1)` avoids SQLITE_BUSY errors — SQLite is single-writer
  - `SqliteConnectOptions` with `.create_if_missing(true)` and `.journal_mode(Wal)` is the correct setup pattern
  - sea-query `SqliteQueryBuilder` renders `json_binary()` as `TEXT` — JSON is stored as strings in SQLite
  - Acceptance criteria mentioned SeaORM but codebase uses sea-query + sqlx directly — followed existing patterns
---
