[PRD]
# PRD: Session Crates Maintainability Improvements

## Overview

The `crates/session/` ecosystem (session-proto, session, session-ui) needs the same maintainability treatment applied to the signal crates: standardized error handling with `thiserror`, strongly-typed IDs, and structural improvements. Additionally, the 2,897-LOC `setlist_service.rs` monolith needs decomposition, with reusable caching and event broadcasting abstractions extracted.

Since session must NOT depend on signal, the shared `typed_uuid_id!` and `typed_string_id!` macros will be extracted into a new `utils` shared crate that both signal-proto and session-proto can depend on.

## Goals

- Extract shared ID macros into a new `utils` crate (shared between signal and session)
- Standardize error handling with typed `SessionServiceError` (matching signal's `SignalServiceError` pattern)
- Introduce strongly-typed UUID IDs (`SongId`, `SectionId`) using `typed_uuid_id!` from the new shared crate
- Break the `setlist_service.rs` monolith into focused sub-modules
- Extract reusable `EventBus` and `Cache` abstractions within the session crate
- Add module-level `//!` documentation to all session crate modules

## Quality Gates

These commands must pass for every user story:
- `cargo check --workspace` — Full workspace compilation

## User Stories

### US-001: Extract shared ID macros into utils crate

**Description:** As a developer, I want the `typed_uuid_id!` and `typed_string_id!` macros in a shared crate so both signal-proto and session-proto can use them without depending on each other.

**Acceptance Criteria:**
- [ ] New crate `crates/utils/` created with `Cargo.toml` (workspace member)
- [ ] `typed_uuid_id!` macro moved from `signal-proto/src/lib.rs` to `utils/src/lib.rs`
- [ ] `typed_string_id!` macro moved from `signal-proto/src/lib.rs` to `utils/src/lib.rs`
- [ ] `utils` depends on `uuid`, `serde`, `facet` (same deps the macros need)
- [ ] `signal-proto` updated: depends on `utils`, re-exports both macros for backward compatibility
- [ ] All existing `signal_proto::typed_uuid_id!` invocations still work (re-export preserves paths)
- [ ] Workspace `Cargo.toml` updated with `utils` as a member and workspace dependency

### US-002: Create SessionServiceError and update service traits in session-proto

**Description:** As a developer, I want typed service errors in session-proto so that session service boundaries have structured, pattern-matchable errors instead of opaque `eyre` or `Option` returns.

**Acceptance Criteria:**
- [ ] `SessionServiceError` enum defined in `session-proto/src/services.rs` with variants: `NotFound { entity: String, id: String }`, `DawError(String)`, `HydrationError(String)`, `Internal(String)`
- [ ] Derives `Debug, Clone, PartialEq, Serialize, Deserialize, Facet, thiserror::Error` with `#[repr(C)]`
- [ ] `thiserror` and `serde` added to session-proto's Cargo.toml
- [ ] Service trait methods that return `Option<T>` updated to `Result<T, SessionServiceError>` where appropriate (e.g., `get_song`, `get_active_song`, `get_active_section`)
- [ ] `SessionServiceError` re-exported from session-proto root
- [ ] Convenience constructor `SessionServiceError::not_found(entity, id)` provided
- [ ] `From<String>` impl maps to `Internal` variant

### US-003: Update session service implementations to use SessionServiceError

**Description:** As a developer, I want the session crate's service implementations to return `SessionServiceError` so error handling is consistent with the trait definitions.

**Acceptance Criteria:**
- [ ] `SetlistServiceImpl` methods updated from `eyre::Result<T>` / `Option<T>` to `Result<T, SessionServiceError>` where trait signatures changed
- [ ] `SongServiceImpl` methods updated similarly
- [ ] Internal `eyre` errors mapped to appropriate `SessionServiceError` variants (DawError for DAW failures, HydrationError for song extraction failures)
- [ ] `From<eyre::Report>` impl on `SessionServiceError` for ergonomic `?` conversion (extracts message string, maps to `Internal`)
- [ ] All existing callers in session-ui updated to handle `Result` instead of `Option`

### US-004: Introduce typed IDs in session-proto

**Description:** As a developer, I want strongly-typed `SongId` and `SectionId` types so that ID parameters can't be accidentally swapped and the code is self-documenting.

**Acceptance Criteria:**
- [ ] `session-proto` depends on `utils` (from US-001)
- [ ] `utils::typed_uuid_id!(SongId)` defined in session-proto
- [ ] `utils::typed_uuid_id!(SectionId)` defined in session-proto
- [ ] `Song` struct updated: add `id: SongId` field, keep `project_guid: String` as the DAW-specific reference
- [ ] `Section` struct updated: add `id: SectionId` field
- [ ] `ActiveIndices` updated to use `SongId` where it references songs by identity
- [ ] `QueuedTarget` variants updated to use typed IDs where applicable
- [ ] `SetlistEvent` variants updated to carry typed IDs
- [ ] New IDs re-exported from session-proto root

### US-005: Update session and session-ui to use typed IDs

**Description:** As a developer, I want all session crate code using string-based song/section identification updated to use the new typed IDs.

**Acceptance Criteria:**
- [ ] `SetlistServiceImpl` internal state updated: `active_song_id` uses `SongId`, song cache keyed by `SongId`
- [ ] `SongBuilder` assigns `SongId::new()` to each built song
- [ ] `SetlistBuilder` propagates typed IDs
- [ ] `session-ui/src/signals.rs` updated: `SONG_TRANSPORT` keyed by `SongId`, chart caches keyed by `SongId`
- [ ] All `get_song(index)` / `get_song_by_id(string)` updated to use `SongId` where identifying by ID
- [ ] Index-based access (by position in setlist) remains `usize` — only identity-based access uses `SongId`

### US-006: Extract EventBus abstraction from setlist_service

**Description:** As a developer, I want a reusable `EventBus<T>` abstraction so that the broadcast/watch channel patterns in setlist_service are composable and testable.

**Acceptance Criteria:**
- [ ] `EventBus<T>` struct created in the session crate (e.g., `src/event_bus.rs`)
- [ ] Wraps `tokio::sync::broadcast::Sender<T>` with typed subscribe/emit API
- [ ] `WatchBus<T>` variant wraps `tokio::sync::watch` for single-value streaming
- [ ] `setlist_service` refactored to use `EventBus<SetlistEvent>`, `EventBus<(usize, Song)>`, `WatchBus<u64>` instead of raw channels
- [ ] Channel capacity configurable at construction

### US-007: Extract Cache abstraction from setlist_service

**Description:** As a developer, I want a reusable `Cache<K, V>` abstraction so that the song cache, chart cache, and fingerprint tracking patterns are consolidated and testable.

**Acceptance Criteria:**
- [ ] `Cache<K, V>` struct created in the session crate (e.g., `src/cache.rs`)
- [ ] Wraps `Arc<RwLock<FxHashMap<K, V>>>` with typed `get`, `insert`, `invalidate`, `get_or_insert_with` API
- [ ] Supports optional fingerprint-based invalidation (generation counter pattern)
- [ ] Supports optional last-attempt timestamp tracking for throttled refresh
- [ ] `setlist_service` refactored to use `Cache<SongId, SongCacheEntry>` and `Cache<SongId, SongChartHydration>` instead of raw hash maps + locks
- [ ] `Cache` is `Send + Sync` for use across tokio tasks

### US-008: Split setlist_service.rs into sub-modules

**Description:** As a developer, I want the 2,897-LOC setlist_service.rs decomposed into focused modules so I can find and modify specific behavior without scrolling through a monolith.

**Acceptance Criteria:**
- [ ] `setlist_service/mod.rs` — `SetlistServiceImpl` struct definition, constructor, service trait impl delegation
- [ ] `setlist_service/hydration.rs` — song hydration, chart extraction, fingerprint checking, `ensure_song_hydrated`, `build_songs_with_cache`
- [ ] `setlist_service/navigation.rs` — `go_to_song`, `next_song`, `previous_song`, `next_section`, `previous_section`, queue target management
- [ ] `setlist_service/polling.rs` — active indices polling loop, transport state tracking, progress calculation
- [ ] `setlist_service/build.rs` — `build_from_open_projects`, project scanning, setlist assembly
- [ ] `SetlistServiceImpl` remains the single public type; sub-modules are `pub(crate)` or private
- [ ] No logic changes — pure structural refactor
- [ ] `mod.rs` is < 300 LOC (delegation hub only)

### US-009: Add module-level documentation to all session modules

**Description:** As a developer, I want `//!` doc comments on every module in the session crate ecosystem so I can quickly understand what each file provides.

**Acceptance Criteria:**
- [ ] All session-proto modules have `//!` doc comments (add where missing)
- [ ] All session crate modules have `//!` doc comments including new sub-modules from US-008
- [ ] All session-ui modules have `//!` doc comments (add where missing)
- [ ] New `event_bus.rs` and `cache.rs` have `//!` doc comments explaining usage patterns
- [ ] New `utils` crate has crate-level `//!` doc comment

## Functional Requirements

- FR-1: `SessionServiceError` must derive `Facet` for compatibility with `#[roam::service]` macro
- FR-2: Typed IDs must use `typed_uuid_id!` from the new `utils` shared crate
- FR-3: `utils` must NOT depend on signal-proto or session-proto — it is a leaf dependency
- FR-4: `signal-proto` must re-export `typed_uuid_id!` and `typed_string_id!` from `utils` for full backward compatibility
- FR-5: `EventBus<T>` and `Cache<K, V>` live in the session crate (not a separate crate) for now
- FR-6: `Cache<K, V>` must be `Send + Sync` for use across tokio tasks
- FR-7: The setlist_service split must maintain identical runtime behavior — no logic changes

## Non-Goals (Out of Scope)

- Rewriting the polling/hydration logic (structural refactor only)
- Adding tests for existing untested service methods (future work)
- Changing the session-ui signal architecture (GlobalSignal pattern stays)
- Moving EventBus/Cache to a shared utility crate (stay in session crate for now)
- Optimizing the 60Hz polling frequency or cache eviction policies
- Adding session-proto dependency on signal-proto (they remain independent, sharing only utils)

## Technical Considerations

- `#[macro_export]` places macros at crate root regardless of definition file — `utils` re-exports happen automatically
- Signal-proto can re-export with `pub use utils::typed_uuid_id;` for backward compatibility
- The `#[roam::service]` macro requires all error types to implement `Facet` (learned during signal work)
- `eyre::Report` is not `Clone` — `SessionServiceError` must extract message strings when converting from eyre
- `FxHashMap` requires keys to implement `Hash + Eq` — `SongId` from `typed_uuid_id!` already derives both

## Success Metrics

- `cargo check --workspace` passes with 0 errors
- `setlist_service.rs` (or `setlist_service/mod.rs`) reduced from 2,897 LOC to < 300 LOC
- All session service errors are pattern-matchable (no opaque strings or eyre at trait boundaries)
- No string-based song/section identification in public APIs
- `typed_uuid_id!` macro usable from both signal-proto and session-proto without cross-dependency
[/PRD]
