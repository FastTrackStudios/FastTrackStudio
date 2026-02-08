## Codebase Patterns

- **Sea-query feature flags**: Must enable `with-uuid`, `with-chrono`, `with-json` on both `sea-query` and `sea-query-binder` for Uuid/DateTime/JsonValue conversions to work.
- **Workspace glob resolution**: The `cells/*/*` glob in workspace members auto-discovers new crates — no explicit member listing needed.
- **Worktree submodule gotcha**: `git submodule update --init` can delete recently created directories in worktrees. Write files AFTER submodule initialization.
- **Pre-existing signal-proto warnings**: `signal-proto` has 8 unused import/variable warnings that cause `cargo clippy -p signal-storage -- -D warnings` to fail. The signal-storage code itself is warning-free.

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
