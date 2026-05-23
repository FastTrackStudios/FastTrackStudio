# Architect — Framework Design

> **Status**: Design locked, implementation in progress.
> **Scope**: This document describes the target shape of the `architect`
> crate after the `#[architect::rpc]` work lands. The existing
> `#[derive(Entity)]` + `#[architect(repo)]` surface remains and is
> reframed as syntactic sugar over the new primitive.

## Goal in one sentence

Given a Rust struct (the data) and a Rust trait (the operations), the
`architect` crate should emit a complete RPC face — async client, async
server-side host, wire mirror — so the **same trait** is callable
in-process (zero-cost, sync where natural) and across the network
(async, vox-served) without the user writing any per-backend RPC
adapter code.

## Why this shape

Two real projects drove the design:

- **Task-architect** — CRUD-shaped domain (Project, Task, Cycle,
  Milestone). Operations are uniformly async (Loro CRDT, SeaORM
  storage). Architect's existing `#[derive(Entity)] #[architect(repo)]`
  already serves this well.
- **daw** — verb-shaped domain (`set_muted`, `place_marker_at_position`,
  `set_volume`). Live REAPER backend is **sync** (REAPER C API is
  thread-bound, not awaitable). Currently maintains parallel hand-written
  sync trait + async RPC dispatcher files per service per backend
  (`daw-proto/src/sync/*` mirrored by `daw-reaper/src/remote/*`).

A single primitive — `#[architect::rpc]` — covers both shapes if it
adapts to the trait's method signatures:

- All-sync trait → bridge marshals every call onto a `Dispatcher`,
  emits an async mirror as the RPC face.
- All-async trait → the trait *is* its own RPC face (current
  Task-architect behavior).
- Mixed → sync methods bridged, async methods passed through.

The user never picks a "mode." The trait's method shapes pick.

## The user-facing surface

Three nouns per service, one macro:

```rust
#[derive(architect::Entity)]
pub struct Track {
    pub guid: String,
    pub name: String,
    pub muted: bool,
    pub volume: f64,
    // …
}

#[architect::rpc]
pub trait Tracks {
    fn all(&self) -> Vec<Track>;
    fn by_guid(&self, guid: &str) -> Option<Track>;
    fn add(&self, name: &str, at_index: Option<u32>) -> DawResult<String>;
    fn set_muted(&self, guid: &str, muted: bool) -> DawResult<()>;
    fn set_volume(&self, guid: &str, volume: f64) -> DawResult<()>;
}
```

From those two declarations, `architect::rpc` emits:

| Name | Purpose |
|---|---|
| `Tracks` | (kept) the sync trait the user wrote — used by in-process callers |
| `TracksClient` | async caller proxy — used by remote code (web UI, CLI, other-language clients) |
| `TracksHost` | server-side wrapper that mounts on a vox router |

Backends implement `Tracks` directly. The bridge inside `TracksHost`
takes any `Arc<dyn Tracks>` and a `Dispatcher` and exposes it as the
async vox surface.

## Reference call sites

```rust
// in-process (REAPER extension, standalone, dawfile editor) ─────────
let tracks: &ReaperTracks = /* … */;
tracks.set_muted("guid-123", true)?;          // sync, no ceremony

let tracks: Arc<dyn Tracks> = /* … */;
tracks.set_muted("guid-123", true)?;          // dyn, one vtable hop

// remote (Dioxus web client, CLI) ───────────────────────────────────
let tracks = TracksClient::connect("ws://daw.local/vox").await?;
tracks.set_muted("guid-123", true).await?;

// server-side mount ─────────────────────────────────────────────────
router.mount(TracksHost::new(reaper_tracks, moire_dispatcher));
```

The trait's method names appear identically on every face. Only
`.await` placement differs between in-process and remote callers.

## Architecture

### Three layers (consumer side)

```
Layer 3: facade entry points (per-app, hand-wired)
    daw::get()  → LocalDaw   (sync handle, OnceLock global)
    daw::rpc()  → RemoteDaw  (async handle, connect-then-use)

Layer 2: services + entities (per-feature, written once)
    #[derive(Entity)] struct Track { … }
    #[architect::rpc] trait Tracks { … }
    impl Tracks for {ReaperTracks, StandaloneTracks, RppFileEditor, …}
    + auto-emitted: TracksClient, TracksHost

Layer 1: pure helpers over data (per-feature, write as needed)
    fn longest_track_name(tracks: &[Track]) -> Option<&str>
    fn validate_no_overlap(items: &[Item]) -> Result<(), OverlapError>
```

Reusable logic lives in Layer 1. Service-shaped helpers (Layer 2) are
intrinsically colored — sync or async, not both. Entry-point code
(Layer 3) is colored by which constructor the caller used. When code
needs to work in both contexts, refactor down into Layer 1.

### Crate layout (framework side)

```
macros/
  architect/                    runtime crate (re-exports + shared types)
  architect-derive/             #[derive(Entity)] proc-macro
  architect-rpc-derive/         #[architect::rpc] proc-macro             [new]

crates/
  architect-dispatch/           Dispatcher trait + standard impls         [new]
    └── features
        ├── tokio       → spawn_blocking-based Dispatcher
        ├── moire       → main-thread queue Dispatcher (for daw)
        └── current     → call-inline identity Dispatcher (tests, single-thread)
```

`architect-rpc-derive` is a separate proc-macro crate so projects that
only want the Entity derive don't pay its compile cost, and vice versa.
The user-facing `architect` crate re-exports both so consumers see
one import path: `#[derive(architect::Entity)]` / `#[architect::rpc]`.

## `#[architect::rpc]` — what it emits, by trait shape

The macro inspects every method on the trait and classifies it:

- **Sync**: regular `fn foo(&self, …) -> T` (no `async`, no `-> impl Future`)
- **Async**: `async fn foo(&self, …) -> T` or `fn foo(&self, …) -> impl Future<Output = T>`

The classification drives emission:

| Trait | Classification | Emits |
|---|---|---|
| All sync | All-sync mode | `<T>Client` (async), `<T>Host::new(impl T, dispatcher)`, hidden async mirror for vox::service |
| All async | All-async mode | `<T>Client` (async); trait is its own server-side surface; `<T>Host::new(impl T)` is a thin newtype, no dispatcher needed |
| Mixed | Mixed mode | `<T>Client` (async), `<T>Host::new(impl T, dispatcher)`. Bridge marshals sync methods, passes async methods through |

**Decision rule for the user**: write the trait the way that fits the
natural shape of the operations. Don't try to force sync when the
backend is async-native (SeaORM, sqlx, HTTP). Don't force async when
the backend is sync-native (REAPER C API, Loro CRDT, in-memory state).

### Object-safety requirement

`<T>Host` stores `Arc<dyn T>`. The trait must therefore be
object-safe:

- No generic methods
- No `Self` returns
- No `impl Trait` in return position
- No `async fn` in trait method declarations that aren't `Pin<Box<…>>`
  (the macro can rewrite if needed; or require `#[async_trait]`-style
  decoration on async methods; TBD by implementation)

The macro validates this at expansion time and produces a clear
compile error if any method is dyn-incompatible.

### Argument/return rewriting (sync → async mirror)

The sync trait's method signatures are written as a programmer would
naturally write them (`&str`, `&[T]`, owned returns). The hidden async
mirror used for vox::service must take owned `'static` arguments so
the bridge's closures can capture them safely. The macro rewrites:

- `&str` → `String`
- `&[T]` → `Vec<T>` (when `T: Clone`)
- `&T` (other ref args) → `T` (when `T: Clone`)
- `Cow<'_, str>` → `String`

Return types must already be owned `'static + Facet`; the macro errors
on borrowed returns. (daw's current sync traits already return owned
values, so this is a non-issue for the immediate roll-out.)

### Streams and other things sync can't express

Some methods are genuinely async-only:

- Subscriptions returning `Rx<T>` / `Tx<T>` / streams
- Methods that perform network IO themselves
- Long-running operations that need to be cancellable

These cannot be expressed in a sync trait. Two escape hatches:

1. **Mixed-mode trait**: declare these methods with `async fn` inline
   on the same trait. The macro classifies them as async and passes
   them through the bridge unchanged.
2. **Sibling trait pattern**: keep the sync trait pure, declare
   streaming methods on a separate `#[architect::rpc] trait <T>Stream`
   with `async fn` methods, mount both hosts on the router.

Either is acceptable; pick per service.

## The `Dispatcher` trait

The bridge marshals each sync method call through a `Dispatcher` so
the call lands on the right thread / executor.

```rust
pub trait Dispatcher: Send + Sync + 'static {
    fn dispatch<F, T>(
        &self,
        f: F,
    ) -> Pin<Box<dyn Future<Output = Result<T, DispatchError>> + Send + 'static>>
    where
        F: FnOnce() -> T + Send + 'static,
        T: Send + 'static;
}
```

`Pin<Box<dyn Future>>` is used (not `impl Future`) so the trait is
object-safe and bridges can store `Arc<dyn Dispatcher>`.

### Standard dispatchers

| Dispatcher | Behavior | When to use |
|---|---|---|
| `TokioBlockingDispatcher` | `tokio::task::spawn_blocking(f)` | Server-side async-native binaries with sync wrapping (e.g. wrapping a sync DB driver) |
| `MoireMainThreadDispatcher` | Marshals to the moire main-thread queue | daw's REAPER backend — runs sync calls on the REAPER thread |
| `CurrentThreadDispatcher` | Calls `f()` inline; returns `Poll::Ready(Ok(f()))` | Tests, single-threaded contexts, in-process callers where no marshaling is needed |

`CurrentThreadDispatcher` is important: a Loro-backed entity exposed
through `#[architect::rpc]` doesn't need real marshaling — Loro ops
are CPU-only and complete in microseconds. Wrapping them in
`spawn_blocking` would be pure overhead. The current-thread
dispatcher is the right pick.

### Error mapping

`DispatchError` covers "dispatcher shut down", "task panicked",
"deadline exceeded". The bridge maps these into vox errors at the
trait boundary. Application-level errors flow through unchanged via
`IntoVoxError`:

```rust
pub trait IntoVoxError {
    fn into_vox_error(self) -> vox::Error;
}
```

One impl per app-level error type. daw writes `impl IntoVoxError for
DawError` once; every service bridge uses it. Architect provides a
default `impl IntoVoxError for RepoError` for entity-derive consumers.

## The facade pattern (consumer side)

For a multi-service product (daw, Task-architect, anything similar),
the convention is two ergonomic entry points in the facade:

```rust
// daw (facade crate)

// ── sync (in-process) ──────────────────────────────────────────────
pub struct LocalDaw { backend: Arc<dyn DawBackend> }

impl LocalDaw {
    pub fn tracks(&self)  -> &dyn Tracks  { &*self.backend }
    pub fn items(&self)   -> &dyn Items   { &*self.backend }
    pub fn markers(&self) -> &dyn Markers { &*self.backend }
    // one accessor per service
}

static GLOBAL: OnceLock<LocalDaw> = OnceLock::new();

pub fn install(backend: Arc<dyn DawBackend>) { let _ = GLOBAL.set(LocalDaw { backend }); }
pub fn get() -> &'static LocalDaw { GLOBAL.get().expect("daw::install(…) at startup") }

// Per-backend convenience constructors:
pub fn reaper()     -> &'static LocalDaw { /* install ReaperBackend, return get() */ }
pub fn standalone() -> &'static LocalDaw { /* install StandaloneBackend */ }
pub fn dawfile(path: impl AsRef<Path>) -> Result<&'static LocalDaw, _> { /* install DawfileEditor */ }

// ── async (remote) ─────────────────────────────────────────────────
pub struct RemoteDaw {
    tracks:  TracksClient,
    items:   ItemsClient,
    markers: MarkersClient,
    // one field per service
}

impl RemoteDaw {
    pub fn tracks(&self)  -> &TracksClient  { &self.tracks }
    pub fn items(&self)   -> &ItemsClient   { &self.items }
    pub fn markers(&self) -> &MarkersClient { &self.markers }
}

pub async fn rpc(url: &str) -> Result<RemoteDaw, DawError> {
    let link = vox::WsLink::connect(url).await?;
    Ok(RemoteDaw {
        tracks:  TracksClient::on(&link).await?,
        items:   ItemsClient::on(&link).await?,
        markers: MarkersClient::on(&link).await?,
    })
}
```

Adding a new service is mechanical:
1. `#[architect::rpc] trait Mixing { … }` in proto
2. `impl Mixing for X` per backend
3. In facade: add `Mixing` to `DawBackend` supertrait, add
   `pub fn mixing()` accessor on `LocalDaw`, add `mixing: MixingClient`
   field on `RemoteDaw` with its accessor and one line in `rpc()`.

No per-service RPC wrapper code. No hand-mirrored async dispatcher.
Just one trait declaration plus the backend impls (the real logic).

## Relationship to `#[derive(Entity)]` and `#[architect(repo)]`

The Entity derive remains as the data-side surface. With the new
primitive, `#[architect(repo)]` becomes syntactic sugar:

```rust
#[derive(architect::Entity)]
#[architect(table_name = "projects", repo)]
pub struct Project { /* … */ }
```

is functionally equivalent to:

```rust
#[derive(architect::Entity)]
pub struct Project { /* … */ }

pub struct ProjectCreate { /* fields minus exclude(create) */ }
pub struct ProjectUpdate { /* fields minus exclude(update) */ }
pub struct ProjectList   { items: Vec<Project>, total: u32, page: Page }

#[architect::rpc]
pub trait ProjectRepo {
    async fn get(&self, id: Uuid) -> Result<Project, RepoError>;
    async fn list(&self, page: Page, sort: Option<Sort>, filter: Option<Filter>) -> Result<ProjectList, RepoError>;
    async fn create(&self, input: ProjectCreate) -> Result<Project, RepoError>;
    async fn update(&self, id: Uuid, input: ProjectUpdate) -> Result<Project, RepoError>;
    async fn delete(&self, id: Uuid) -> Result<(), RepoError>;
}
```

The auto-emitted `ProjectRepo` trait is all-async (because most
real-world repo backends are async-native — SeaORM, sqlx, HTTP).
Task-architect's existing usage keeps working unchanged.

Future opt-in: `#[architect(repo(sync))]` would emit the same trait
with sync method signatures, for entities backed by sync-native
stores (Loro, in-memory, files). The bridge would then provide the
async face the way it does for verb-shaped daw traits. Not in v1;
add if there's demand.

## Reusable logic — colorless functions

Function color (sync vs async) is unavoidable at the service-call
boundary. The framework recovers reusability by encouraging a clean
split:

| Layer | Color | Reusability |
|---|---|---|
| Entry points (`daw::get()`, `daw::rpc()`) | Colored | Per-app only |
| Service helpers (operate on `&dyn Tracks`, `&TracksClient`) | Colored | Per-color only |
| **Pure helpers (operate on `&[Track]`, `&Marker`, etc.)** | **Colorless** | **Universal** |

Example:

```rust
// Layer 1 — pure, colorless, lives in daw-proto or a shared helpers crate
pub fn longest_track_name(tracks: &[Track]) -> Option<&str> { … }
pub fn validate_no_overlap(items: &[Item]) -> Result<(), OverlapError> { … }

// Layer 3 — sync caller
fn report_longest() {
    let names = daw::get().tracks().all();
    println!("{:?}", longest_track_name(&names));
}

// Layer 3 — async caller, same helper
async fn report_longest_remote(url: &str) -> Result<(), DawError> {
    let names = daw::rpc(url).await?.tracks().all().await?;
    println!("{:?}", longest_track_name(&names));
    Ok(())
}
```

When dual-color helpers are genuinely necessary (probably rare), the
options are:

1. **Duplicate.** The diff between sync and async versions is exactly
   `.await?` placement — small, mechanical, readable.
2. **`#[architect::dual]` macro** that generates both copies from one
   source. Build only if 20+ duplicated dual helpers actually exist
   in practice.

Default to (1). Don't ship (2) speculatively.

## Decided design points

1. **`#[architect::rpc]` is the keystone primitive.** Entity is sugar
   that desugars to a rpc-decorated trait plus payload structs.
2. **No mode attributes on the macro.** Trait shape determines emission.
3. **Two named facade handles, not one generic.** `LocalDaw` and
   `RemoteDaw`; generic `Daw<Mode>` was rejected as readability-negative.
4. **`Arc<dyn Trait>` inside `<T>Host`.** Dyn dispatch costs one
   vtable hop per call, dominated by the dispatcher hop anyway.
   Backends compose at runtime.
5. **`Dispatcher` is object-safe** (`Pin<Box<dyn Future>>` return).
6. **One `IntoVoxError` per app**, not per service. Trait lives in
   `architect-core`; default impl for `RepoError`.
7. **Method names symmetric across faces.** `Tracks::set_muted` ↔
   `TracksClient::set_muted` ↔ `TracksHost`'s internal bridge. Argument
   types rewritten only for ownership; return types preserved.

## Open items for implementation

These need to be settled in the code, not in this doc:

- Exact syntax for object-safety errors emitted by the macro
- Whether the hidden async mirror trait gets a stable name (e.g.
  `__TracksRpcMirror`) or is fully anonymous via an inherent impl on
  `TracksHost`
- Argument-rewriting edge cases (`&mut`, `Option<&str>`, nested
  references, etc.) — start with the simple set, extend on demand
- Where `Dispatcher` lives: `architect-dispatch` standalone crate vs
  module under `architect` runtime crate. Likely standalone so the
  per-runtime feature crates can depend on it without dragging in
  the Entity derive.
- How `vox::service` interacts with the hidden mirror trait. If
  `vox::service` accepts trait declarations the macro generates,
  great. If it needs custom shape, the macro may need to emit the
  dispatcher/client glue directly without going through `vox::service`.

## Rollout plan

1. **Land `Dispatcher` trait + `architect-dispatch` crate** with the
   three standard impls. Useful on its own; no proc-macro work.
2. **Land `#[architect::rpc]` macro** for the all-sync case first
   (the daw shape). Defer mixed/all-async until the sync case is
   working end-to-end.
3. **Pilot in daw** with one feature: `markers` is a good candidate
   (~10 methods, clean shape, well-defined verbs). Measure LOC delta
   vs the current `sync/markers.rs` + `remote/markers.rs` pair.
4. **Roll across daw's remaining 17 sync services** if the pilot
   lands well.
5. **Extend macro to handle async methods** for mixed-mode traits.
   This unblocks streaming methods (`Rx<T>` subscriptions) within
   the same trait declaration.
6. **Rewire `#[architect(repo)]`** to emit through the new
   `#[architect::rpc]` pipeline. Verify Task-architect's existing
   usage still compiles unchanged.
7. **(Optional, on demand)** Add `#[architect(repo(sync))]` for
   sync-backed entities. Add `#[architect::dual]` if duplication
   pain emerges.

## What this design rules out

- **Mode-polymorphic API surfaces** (`Daw<M>`, generic `<T>Repo<Mode>`).
  Costs readability more than it saves.
- **Automatic cross-color helper synthesis.** Pure functions and
  manual duplication cover the real cases.
- **Hidden `block_on` inside any framework type.** If a sync trait
  wraps an async backend, the user opts into that explicitly by
  picking a dispatcher that supports it (`TokioBlockingDispatcher`
  with the impl handling its own runtime handle).
- **Runtime-polymorphic facade handles.** Callers that need this
  wrap an enum themselves; not a framework concern.

## Glossary

- **Service** — a trait describing operations on a domain concept.
  Annotated with `#[architect::rpc]`. The trait *is* the service;
  the macro derives the network face.
- **Entity** — a struct describing data shape. Annotated with
  `#[derive(architect::Entity)]`. Travels over the wire as
  `facet::Facet`.
- **Host** — the server-side wrapper that mounts a backend impl on
  a vox router. Auto-emitted as `<T>Host`.
- **Client** — the caller-side proxy that talks to a `<T>Host` over
  vox. Auto-emitted as `<T>Client`.
- **Dispatcher** — the marshaling primitive that runs sync closures
  on the right thread / executor. Implemented per runtime context
  (tokio, moire main-thread, current-thread).
- **Bridge** — internal: the adapter inside `<T>Host` that turns
  `Arc<dyn T>` + `Dispatcher` into the hidden async mirror trait that
  `vox::service` consumes. Not user-visible.
