# PRD: Signal Crates Maintainability Improvements

## Overview
Improve the maintainability and developer experience of the `signal/` crate ecosystem by addressing two key pain points: (1) difficulty finding where code lives, especially in the large `signal-proto` crate, and (2) confusion caused by `String`-typed errors at service boundaries. This is a conservative refactor — existing crate boundaries are preserved, with changes focused on internal organization, documentation, and error standardization.

## Goals
- Make it immediately clear which crate and module to look in for any given piece of functionality
- Replace `String` errors at service trait boundaries with properly typed `thiserror` enums
- Break up `signal-proto`'s 1,454-line `lib.rs` into logically grouped sub-modules
- Add crate-level and module-level documentation across all signal crates
- Preserve the existing layered architecture (proto → storage → live → controller → signal facade)

## Quality Gates

These commands must pass for every user story:
- `cargo check --workspace` — Full workspace type checking

## User Stories

### US-001: Reorganize signal-proto into logical sub-modules
**Description:** As a developer, I want `signal-proto`'s exports organized into clear sub-modules so I can quickly find domain types, service traits, and shared abstractions without scanning a 1,454-line `lib.rs`.

**Acceptance Criteria:**
- [ ] `signal-proto` exports are grouped into sub-modules: `model` (domain types like Rig, Engine, Block, Layer, Preset, etc.), `traits` (Variant, Collection, HasMetadata, Tagged, etc.), `services` (BlockService, LayerService, EngineService, etc.), and `ids` (all ID newtypes)
- [ ] The top-level `lib.rs` re-exports everything from sub-modules for backwards compatibility (existing `use signal_proto::Rig` still works)
- [ ] Each sub-module has a doc comment explaining what it contains and when to use it
- [ ] No changes to the public API surface — only organizational restructuring
- [ ] `cargo check --workspace` passes

### US-002: Standardize signal-proto error types with thiserror
**Description:** As a developer, I want service traits in `signal-proto` to use typed error enums instead of `String` so I can pattern-match on error variants and get better diagnostics.

**Acceptance Criteria:**
- [ ] A `SignalServiceError` enum is defined in `signal-proto` using `#[derive(thiserror::Error)]`
- [ ] The enum has variants covering common service failure modes: `NotFound { entity: &'static str, id: String }`, `StorageError(String)`, `ValidationError(String)`, `ResolveError(ResolveError)`, and `Internal(String)`
- [ ] All service traits (`BlockService`, `LayerService`, `EngineService`, `RigService`, `ProfileService`, `SongService`, `SetlistService`, `SceneTemplateService`, `RackService`, `BrowserService`, `ResolveService`) return `Result<T, SignalServiceError>` instead of `Result<T, String>`
- [ ] Existing `String` error returns are migrated to appropriate `SignalServiceError` variants
- [ ] `cargo check --workspace` passes

### US-003: Standardize signal-storage errors with thiserror
**Description:** As a developer, I want `signal-storage` to use a proper `StorageError` enum with `thiserror` so database errors are descriptive and distinguishable from other failure modes.

**Acceptance Criteria:**
- [ ] `StorageError` in `signal-storage` uses `#[derive(thiserror::Error)]` with clear variant names
- [ ] Variants include: `DatabaseError(#[from] sea_orm::DbErr)`, `NotFound { entity: &'static str, id: String }`, `SerializationError(String)`, and `MigrationError(String)`
- [ ] All repo trait methods and `*Live` implementations use the updated `StorageError`
- [ ] `StorageError` implements `From<sea_orm::DbErr>` for ergonomic `?` usage
- [ ] `cargo check --workspace` passes

### US-004: Standardize signal-live errors with thiserror
**Description:** As a developer, I want `signal-live` errors (`EngineError`, `MacroError`, `PatchApplyError`, `SnapshotError`) standardized with `thiserror` so runtime failures are clear and actionable.

**Acceptance Criteria:**
- [ ] All error types in `signal-live` use `#[derive(thiserror::Error)]`
- [ ] `EngineError`, `MacroError`, `PatchApplyError`, and `SnapshotError` have descriptive `#[error("...")]` messages
- [ ] `SignalLive` service implementations convert internal errors to `SignalServiceError` variants (from US-002) at trait boundaries
- [ ] `cargo check --workspace` passes

### US-005: Standardize signal-controller OpsError with thiserror
**Description:** As a developer, I want `signal-controller`'s `OpsError` to use `thiserror` and properly wrap upstream errors from `signal-live` and `signal-storage`.

**Acceptance Criteria:**
- [ ] `OpsError` uses `#[derive(thiserror::Error)]` with clear variant messages
- [ ] `OpsError` has `From` implementations for `SignalServiceError` and `StorageError` to support `?` chaining
- [ ] All `Ops` methods (`BlockOps`, `LayerOps`, `EngineOps`, etc.) use the updated `OpsError`
- [ ] Error chains preserve the original cause through `#[source]` attributes
- [ ] `cargo check --workspace` passes

### US-006: Standardize nam-manager errors with thiserror
**Description:** As a developer, I want `nam-manager`'s `NamError` standardized with `thiserror` so file I/O, parsing, and content errors are descriptive and consistent with the rest of the signal crate ecosystem.

**Acceptance Criteria:**
- [ ] `NamError` uses `#[derive(thiserror::Error)]` with clear variant names and messages
- [ ] Variants cover: `IoError(#[from] std::io::Error)`, `ParseError(String)`, `ContentError(String)`, `CatalogError(String)`
- [ ] All public functions in `nam-manager` return `Result<T, NamError>` using the updated type
- [ ] `#[source]` attributes preserve error chains where applicable
- [ ] `cargo check --workspace` passes

### US-007: Add crate-level documentation to all signal crates
**Description:** As a developer, I want each signal crate to have a top-level doc comment in `lib.rs` explaining the crate's purpose, its position in the dependency graph, and what types/traits it provides.

**Acceptance Criteria:**
- [ ] Each of the 9 signal crates has a `//!` doc comment at the top of `lib.rs`
- [ ] Each doc comment includes: (1) one-line summary, (2) role in the signal architecture, (3) key types/traits provided, (4) which crates depend on it and which it depends on
- [ ] `signal-proto` docs explicitly list the sub-module groupings from US-001
- [ ] `signal` (facade crate) docs explain the bootstrap functions and re-export strategy
- [ ] `cargo check --workspace` passes

### US-008: Add module-level documentation to key modules
**Description:** As a developer, I want module-level doc comments on the most-navigated modules so I can quickly understand what each file provides.

**Acceptance Criteria:**
- [ ] All `signal-proto` sub-modules (from US-001) have `//!` doc comments
- [ ] All `signal-storage` repo modules have `//!` doc comments explaining the entity they persist
- [ ] All `signal-live` service modules have `//!` doc comments explaining the service's responsibilities
- [ ] All `signal-controller/ops/` modules have `//!` doc comments explaining the ops namespace
- [ ] `signal-live/engine/` has a `mod.rs` doc comment explaining the gapless runtime architecture
- [ ] `cargo check --workspace` passes

### US-009: Consolidate Collection/Variant boilerplate with macro in signal-proto
**Description:** As a developer, I want the repeated Collection/Variant implementation pattern consolidated via a `macro_rules!` macro in `signal-proto` so adding new entity types requires less boilerplate.

**Acceptance Criteria:**
- [ ] A `macro_rules!` macro (e.g., `impl_collection!`) is defined in `signal-proto` to reduce boilerplate for implementing `Collection` and `Variant` traits
- [ ] The macro lives in `signal-proto` alongside the traits it implements
- [ ] The macro also generates the `typed_uuid_id!` call for the collection's ID type, so a single macro invocation produces both the ID newtype and the trait impls
- [ ] At least 3 of the 10 existing Collection/Variant implementations are migrated to use the macro
- [ ] The macro handles common operations: `normalize_default()`, variant lookup by ID, default variant access
- [ ] Adding a new Collection/Variant pair requires fewer than 20 lines of boilerplate (excluding the struct definition)
- [ ] The macro is documented with a usage example in its doc comment
- [ ] `cargo check --workspace` passes

## Functional Requirements
- FR-1: All existing public API signatures must remain backwards-compatible; any downstream consumers found using `Result<T, String>` will be updated to the new error types
- FR-2: `signal-proto` sub-modules must re-export all types at the crate root so `use signal_proto::Rig` continues to work
- FR-3: Error types must implement `std::fmt::Display` and `std::error::Error` via thiserror
- FR-4: Error conversions between layers must preserve the original cause chain
- FR-5: Documentation must be accurate — don't document aspirational architecture, document what exists
- FR-6: The Collection/Variant macro must be documented with usage examples
- FR-7: Error migration should proceed bottom-up (storage → live → controller) to avoid cascading breakage

## Non-Goals
- Splitting or merging existing crate boundaries (conservative scope)
- Refactoring the 10-generic-parameter `SignalLive` type
- Changing the async runtime or database layer
- Adding new features or domain entities
- Performance optimization
- Changing the `signal-ui` component architecture
- Migrating to `anyhow` or any other error library

## Technical Considerations
- `thiserror` is likely already in the workspace — verify before adding as a dependency
- The `signal-proto` reorganization should be done as a single commit to avoid intermediate breakage
- The `impl_collection!` macro should be a `macro_rules!` in `signal-proto` that also calls `typed_uuid_id!` internally — no need for proc-macro complexity
- Error migration should proceed bottom-up: storage → live → controller to avoid cascading breakage
- Test that `cargo doc --workspace` generates useful output after documentation changes

## Success Metrics
- A developer unfamiliar with the codebase can locate any domain type within 30 seconds using crate docs and module structure
- Error messages in logs include structured context (entity type, ID) instead of opaque strings
- Adding a new entity type (e.g., a hypothetical `Pedalboard`) requires less than 50% of the current boilerplate
