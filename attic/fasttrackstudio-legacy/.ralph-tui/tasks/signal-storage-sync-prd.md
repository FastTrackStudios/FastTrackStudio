# PRD: Signal Storage & Cloud Sync System

A database-backed storage system for presets, snapshots, and rig configurations with cloud sync and preset sharing capabilities. Uses sea-query for database abstraction to support SQLite (local) and PostgreSQL/MySQL (cloud).

## Background

Users need to:
- **Store locally**: Save presets and snapshots to a local database for fast access
- **Sync across devices**: Keep presets synchronized between studio, laptop, and live rig
- **Share presets**: Publish presets to a community library, discover others' presets
- **Backup/restore**: Never lose configurations, even if hardware fails

### Why sea-query?

[sea-query](https://github.com/SeaQL/sea-query) provides:
- **Database agnostic**: Same code works with SQLite, PostgreSQL, MySQL, MariaDB
- **Type-safe query building**: Compile-time checked SQL queries
- **No ORM overhead**: Direct query building without heavy abstractions
- **Async support**: Works with sqlx, tokio-postgres, etc.

### Architecture

```
┌─────────────────────────────────────────────────────────────┐
│  Signal UI (Dioxus)                                         │
│  └─ PresetBrowser, CloudSync UI                             │
└──────────────────────┬──────────────────────────────────────┘
                       │
┌──────────────────────▼──────────────────────────────────────┐
│  signal-storage (New Crate)                                 │
│  ├─ StorageService trait                                    │
│  ├─ LocalStorage (SQLite via sea-query + sqlx)              │
│  ├─ CloudStorage (PostgreSQL via sea-query + sqlx)          │
│  └─ SyncEngine (conflict resolution, delta sync)            │
└──────────────────────┬──────────────────────────────────────┘
                       │
┌──────────────────────▼──────────────────────────────────────┐
│  Database Layer                                             │
│  ├─ Local: ~/.signal/signal.db (SQLite)                     │
│  └─ Cloud: PostgreSQL on Supabase/Neon/self-hosted          │
└─────────────────────────────────────────────────────────────┘
```

## Quality Gates

These commands must pass for every user story:
- `cargo check -p signal-storage` - Type checking
- `cargo clippy -p signal-storage -- -D warnings` - Linting
- `cargo test -p signal-storage` - Unit and integration tests
- `cargo build -p fts-control-desktop` - Desktop app builds

## User Stories

---

### US-025: Create signal-storage crate with sea-query schema

**Description:** As a developer, I need a new crate for database operations with sea-query schema definitions for all storage entities.

**Acceptance Criteria:**
- [ ] Create `cells/signal/signal-storage/` crate
- [ ] Add dependencies: `sea-query`, `sea-query-binder`, `sqlx` (with sqlite + postgres features)
- [ ] Define schema using sea-query `TableCreateStatement`:
  ```rust
  // Tables: presets, snapshots, modules, parameters, users, sync_log
  enum Preset {
      Table,
      Id,           // UUID
      Name,
      Description,
      AuthorId,     // UUID (nullable for local-only)
      CreatedAt,
      UpdatedAt,
      Version,      // For optimistic locking
      IsPublic,     // Published to community
      Tags,         // JSON array
      Data,         // JSON blob of preset content
  }
  ```
- [ ] Create migration system for schema versioning
- [ ] Implement `run_migrations()` function
- [ ] Add schema for: `presets`, `snapshots`, `rig_configs`, `module_chunks`, `sync_metadata`
- [ ] cargo check -p signal-storage passes

---

### US-026: Implement LocalStorage with SQLite

**Description:** As a user, I want my presets saved to a local SQLite database so they persist across app restarts and load quickly.

**Acceptance Criteria:**
- [ ] Create `LocalStorage` struct implementing `StorageService` trait
- [ ] Define `StorageService` trait:
  ```rust
  #[async_trait]
  trait StorageService {
      async fn save_preset(&self, preset: &Preset) -> Result<Uuid>;
      async fn get_preset(&self, id: Uuid) -> Result<Option<Preset>>;
      async fn list_presets(&self, filter: PresetFilter) -> Result<Vec<PresetSummary>>;
      async fn delete_preset(&self, id: Uuid) -> Result<()>;
      async fn save_snapshot(&self, snapshot: &RigSnapshot) -> Result<Uuid>;
      async fn get_snapshot(&self, id: Uuid) -> Result<Option<RigSnapshot>>;
      // ... similar for other entities
  }
  ```
- [ ] Use sea-query to build all SQL queries
- [ ] Store database at `~/.signal/signal.db` (or platform-appropriate location)
- [ ] Implement connection pooling with sqlx
- [ ] Add indexes for common queries (by name, by tag, by date)
- [ ] Implement full-text search for preset names/descriptions
- [ ] cargo test -p signal-storage with SQLite tests passes

---

### US-027: Implement preset CRUD operations

**Description:** As a user, I want to create, read, update, and delete presets through a clean API.

**Acceptance Criteria:**
- [ ] Implement `create_preset(name, data) -> Uuid`
- [ ] Implement `get_preset(id) -> Option<Preset>`
- [ ] Implement `update_preset(id, changes) -> Result<()>`
- [ ] Implement `delete_preset(id) -> Result<()>`
- [ ] Implement `list_presets(filter) -> Vec<PresetSummary>`
- [ ] Implement `search_presets(query) -> Vec<PresetSummary>`
- [ ] Add optimistic locking with version field
- [ ] Return proper errors for conflicts, not found, etc.
- [ ] Implement soft delete (mark deleted, don't remove)
- [ ] cargo test with CRUD integration tests passes

---

### US-028: Implement preset tagging and categorization

**Description:** As a user, I want to tag and categorize my presets so I can organize and find them easily.

**Acceptance Criteria:**
- [ ] Add `tags: Vec<String>` field to Preset
- [ ] Add `category: PresetCategory` enum (Amp, Effect, FullRig, Module, Snapshot)
- [ ] Implement `add_tag(preset_id, tag)` and `remove_tag(preset_id, tag)`
- [ ] Implement `list_by_tag(tag) -> Vec<PresetSummary>`
- [ ] Implement `list_tags() -> Vec<(String, usize)>` with counts
- [ ] Add tag autocomplete based on existing tags
- [ ] Support hierarchical categories (Amp/Clean, Amp/High Gain, etc.)
- [ ] Add favorite/starred flag for quick access
- [ ] cargo test with tagging tests passes

---

### US-029: Implement CloudStorage with PostgreSQL

**Description:** As a developer, I need a cloud storage backend using PostgreSQL for syncing and sharing presets.

**Acceptance Criteria:**
- [ ] Create `CloudStorage` struct implementing `StorageService` trait
- [ ] Reuse sea-query schema (same tables, different backend)
- [ ] Configure connection via environment variables or config file
- [ ] Implement connection pooling with sqlx (PostgreSQL)
- [ ] Add retry logic for transient failures
- [ ] Add timeout configuration for slow connections
- [ ] Implement health check endpoint
- [ ] Support TLS connections for security
- [ ] Add connection string builder for common cloud providers (Supabase, Neon, Railway)
- [ ] cargo test with PostgreSQL integration tests (using testcontainers)

---

### US-030: Implement user authentication for cloud sync

**Description:** As a user, I need to authenticate to sync my presets across devices securely.

**Acceptance Criteria:**
- [ ] Add `users` table schema: id, email, display_name, created_at, last_sync
- [ ] Implement OAuth2 login (GitHub, Google as initial providers)
- [ ] Store auth tokens securely in system keychain (keyring crate)
- [ ] Implement token refresh logic
- [ ] Add "Sign In" / "Sign Out" UI in settings
- [ ] Show sync status indicator (synced, syncing, offline, error)
- [ ] Handle unauthenticated state gracefully (local-only mode)
- [ ] Add "Forgot device" to revoke tokens remotely
- [ ] cargo test with mock auth provider

---

### US-031: Implement sync engine with conflict resolution

**Description:** As a user, I want my presets to sync automatically across devices with smart conflict handling.

**Acceptance Criteria:**
- [ ] Create `SyncEngine` struct managing local ↔ cloud synchronization
- [ ] Add `sync_metadata` table tracking last sync time per entity
- [ ] Implement delta sync: only transfer changed entities since last sync
- [ ] Detect conflicts: same entity modified on multiple devices
- [ ] Implement conflict resolution strategies:
  - `LastWriteWins`: Most recent modification wins
  - `Merge`: Combine non-overlapping changes
  - `AskUser`: Prompt user to choose
  - `KeepBoth`: Create duplicate with suffix
- [ ] Add `sync_status` field: `Synced`, `Pending`, `Conflict`, `LocalOnly`
- [ ] Implement background sync on timer (every 5 minutes when online)
- [ ] Implement manual sync trigger
- [ ] Show sync progress in UI
- [ ] Handle offline gracefully (queue changes for later)
- [ ] cargo test with sync integration tests

---

### US-032: Implement preset sharing and community library

**Description:** As a user, I want to share my presets with the community and discover presets others have shared.

**Acceptance Criteria:**
- [ ] Add `is_public: bool` and `published_at: Option<DateTime>` to presets
- [ ] Implement `publish_preset(id)` to make preset public
- [ ] Implement `unpublish_preset(id)` to make preset private again
- [ ] Create community browser UI showing public presets
- [ ] Implement `browse_community(filter, sort, page)` API
- [ ] Add sorting: Most Popular, Newest, Top Rated
- [ ] Add filtering: By category, by tag, by author
- [ ] Implement `clone_preset(id)` to copy community preset to local library
- [ ] Show author attribution on cloned presets
- [ ] Add download count tracking
- [ ] Add reporting mechanism for inappropriate content
- [ ] cargo test with sharing integration tests

---

### US-033: Implement preset ratings and reviews

**Description:** As a user, I want to rate and review community presets to help others find quality content.

**Acceptance Criteria:**
- [ ] Add `ratings` table: preset_id, user_id, score (1-5), review_text, created_at
- [ ] Implement `rate_preset(preset_id, score, review)`
- [ ] Implement `get_ratings(preset_id) -> Vec<Rating>`
- [ ] Calculate and cache average rating on preset
- [ ] Show star rating in preset browser
- [ ] Show review excerpts on preset detail page
- [ ] Allow editing/deleting own ratings
- [ ] Prevent rating own presets
- [ ] Add helpful/not helpful voting on reviews
- [ ] cargo test with ratings tests

---

### US-034: Implement preset versioning and history

**Description:** As a user, I want to see the history of changes to my presets and restore previous versions.

**Acceptance Criteria:**
- [ ] Add `preset_versions` table: preset_id, version_num, data, created_at, message
- [ ] Auto-create version on each save (with debouncing)
- [ ] Implement `list_versions(preset_id) -> Vec<VersionSummary>`
- [ ] Implement `get_version(preset_id, version_num) -> Preset`
- [ ] Implement `restore_version(preset_id, version_num)` (creates new version)
- [ ] Add version diff view (what changed between versions)
- [ ] Limit versions per preset (keep last 50, compact older)
- [ ] Add optional version message (like git commit message)
- [ ] Show version history in preset detail panel
- [ ] cargo test with versioning tests

---

### US-035: Implement preset import/export (JSON, file bundles)

**Description:** As a user, I want to export presets to files for backup or sharing outside the app, and import presets from files.

**Acceptance Criteria:**
- [ ] Define `PresetBundle` format (JSON with embedded binary data)
- [ ] Implement `export_preset(id, path)` → `.signalpreset` file
- [ ] Implement `export_presets(ids, path)` → `.signalbundle` file (multiple)
- [ ] Implement `import_preset(path) -> Uuid`
- [ ] Implement `import_bundle(path) -> Vec<Uuid>`
- [ ] Validate preset structure on import
- [ ] Handle duplicate detection (same UUID already exists)
- [ ] Support drag-and-drop import in UI
- [ ] Add "Export All" and "Import from File" menu items
- [ ] Include referenced module chunks in export
- [ ] cargo test with import/export round-trip tests

---

### US-036: Integrate storage with signal-ui

**Description:** As a user, I want the preset browser and snapshot system to use the new storage backend seamlessly.

**Acceptance Criteria:**
- [ ] Add `signal-storage` dependency to `signal-ui`
- [ ] Create `use_storage()` hook providing StorageService
- [ ] Update `SnapshotSlots` to persist slots to database
- [ ] Update snapshot save/load to use StorageService
- [ ] Update module preset save/load to use StorageService
- [ ] Add "Save to Cloud" option in save dialogs
- [ ] Show cloud sync status icon in toolbar
- [ ] Add offline indicator when cloud unavailable
- [ ] Lazy-load preset lists for performance
- [ ] cargo build -p fts-control-desktop succeeds
- [ ] Manual testing: save preset, restart app, preset still there

