# DAW → architect::rpc — Refactor Plan

> **Status:** Markers + Tracks ported (commits `00e37eb` → `44cf77f` → followup).
> Remaining: 15+ services. This doc tracks the plan, the recipe, and the punch list.

## Goal

Move every daw service from the legacy three-tree pattern
(`sync/`, `remote/`, async-service-with-broadcaster) to the
`#[architect::rpc]` pattern:

- one **canonical sync trait** in `daw-proto/src/<feature>/service.rs`,
  decorated with `#[architect::rpc]`, `ProjectContext` per method
- one **singleton backend** type per runtime: `Reaper`, `Standalone`,
  `RppFile`, … — stateless, holds no per-project state
- one **mount verb** per service: `track::serve(Reaper)`,
  `marker::serve(Standalone)`, etc.

Net effect: ~600–1500 LOC of parallel sync/remote/async-impl
infrastructure deleted **per service**, replaced with one trait
declaration + one impl block per backend. The framework absorbs the
RPC plumbing.

See `../../Development/architect/DESIGN.md` for the framework spec.

## The recipe (copy this per service)

For service `foo`:

1. **Rewrite `daw-proto/src/foo/service.rs`**:
   - delete the async `FooService` trait
   - emit a sync `trait Foo` decorated with `#[architect_rpc_derive::rpc]`
   - every method takes `ProjectContext` as its first arg (after `&self`)
   - keep helpers like `FooExtStateRequest` if used by batch ops; otherwise drop
   - drop streaming methods (`Tx<FooEvent>`) — these go on a sibling
     trait when revived

2. **Update `daw-proto/src/foo/mod.rs`**:
   ```rust
   pub use service::{Foo, FooRpc};
   #[cfg(feature = "vox")]
   pub use service::{
       FooClient, FooRpcDispatcher as Dispatcher,
       serve, foo_rpc_service_descriptor as descriptor,
   };
   ```

3. **Update `daw-proto/src/lib.rs`** glob re-exports: explicitly list
   what comes out of `foo::*` (since glob would pull `serve` /
   `descriptor` / `Dispatcher` into a colliding root namespace).

4. **Update `daw-proto/src/sync/project.rs`**:
   - delete `type Foo<'a>: Foo + 'a` GAT
   - delete `fn foo(&self) -> Self::Foo<'_>` accessor

5. **Update `daw-proto/src/sync/mod.rs`**:
   - remove `mod foo;`
   - re-export from `crate::foo::{Foo, FooRpc}` (+ `FooClient`,
     `serve as serve_foo` under `vox`)

6. **Delete `daw-proto/src/sync/foo.rs`** (the parallel sync trait).

7. **Port REAPER backend `crates/daw-reaper/src/foo.rs`**:
   - replace the file contents wholesale
   - `impl Foo for crate::Reaper { … }` — sync methods, each taking
     `ProjectContext`
   - method bodies call REAPER C API directly (no `main_thread::query`
     wrapping — the dispatcher handles threading)
   - preserve `pub(crate)` helpers if other modules use them
     (`resolve_project`, `resolve_track`, etc.)
   - drop broadcaster scaffolding (TRACK_BROADCASTER, etc.) —
     streaming retired

8. **Delete REAPER companion files**:
   - `crates/daw-reaper/src/sync/foo.rs` (borrowed-view sync impl)
   - `crates/daw-reaper/src/remote/foo.rs` (async-over-main-thread wrapper)
   - drop refs from `sync/mod.rs`, `remote/mod.rs`, `sync/project.rs`,
     `remote/project.rs`

9. **Update `crates/daw-reaper/src/lib.rs`** exports:
   - delete `pub use foo::ReaperFoo`
   - delete `pub use foo::{init_foo_broadcaster, poll_and_broadcast_foos}`
   - keep any `pub(crate)` helpers exposed publicly if they were

10. **Port Standalone backend `crates/daw-standalone/src/foo.rs`**:
    - `impl Foo for crate::sync::Standalone { … }` — sync, operates
      on `ProjectState` directly (no parallel `FooState` struct)
    - delete `crates/daw-standalone/src/sync/foo.rs`
    - drop the mod from `sync/mod.rs`, `sync/project.rs`

11. **Update `daw-proto/src/batch/op.rs`**:
    - trim `FooOp` variants to match the new trait
    - update `foo_op_project_arg` / `foo_op_track_arg` helpers

12. **Update `daw-reaper/src/batch/dispatch.rs`**:
    - `dispatch_foo` takes `&crate::Reaper` (not `&crate::ReaperFoo`)
    - call methods via fully-qualified syntax `Foo::method(svc, ctx, …)`
      (multiple service traits live on `Reaper` — verbs like `all`,
      `get`, `count`, `add` overlap and ambiguate without UFCS)
    - body for each variant: sync call, `.map_err(|e| e.to_string())?`
      for mutators, no `.await`

13. **Update `daw-reaper/src/batch/dispatch_sync.rs`** similarly if it
    has a `match` on `FooOp`.

14. **Update `daw-reaper/src/batch/mod.rs`**:
    - `foo_svc: crate::Reaper` (not `crate::ReaperFoo`)
    - construct with `crate::Reaper` (no `::new()`)

15. **Update `daw-reaper/src/plugin_services.rs`**:
    - drop `let foo = crate::ReaperFoo::new();`
    - drop `init_foo_broadcaster()` call
    - replace mount with `.with(foo::descriptor(), foo::serve(crate::Reaper))`
    - drop `FooServiceDispatcher`, `foo_service_service_descriptor`
      from the imports

16. **Update `crates/daw-bridge/src/lib.rs`** the same way.

17. **Update `crates/daw-control/src/lib.rs`**:
    - replace `pub(crate) use daw_proto::FooServiceClient;` with
      `pub(crate) use daw_proto::FooClient;`
    - update the `foo:` field type in `DawClients`

18. **Update `crates/daw-control/src/foo.rs`** facade:
    - rename method calls on `self.clients.foo` to match new trait:
      `get_foos` → `all`, `get_foo` → `get`, `foo_count` → `count`,
      `add_foo` → `add`, `remove_foo` → `remove`, etc.
    - mutator calls return `DawResult<T>` over vox: use `.await??`
      (one `?` for vox transport error, one for app error)
    - delete facade methods that called retired backend methods
      (no_event subscribe, visibility, ext_state, lane, etc. —
      depends on the service)
    - if a method's return needs the inner error type mapped: add
      `impl From<DawError> for Error` in `daw-control/src/error.rs`
      (already done; same impl serves every service)

19. **Update `apps/daw/src/ops.rs`**:
    - replace `daw::service::foo_service_service_descriptor()` with
      `daw::service::foo::descriptor()`
    - any CLI commands calling retired facade methods: stub with
      `Err(eyre::eyre!("retired with the architect::rpc port"))`

20. **Test cleanup**:
    - tests in `crates/daw-reaper/tests/reaper_<feature>*.rs` that
      call retired methods: comment them out or stub
    - integration tests that go through `.foo()` on `Project` need
      updating to construct the singleton differently

21. **Verify**: `cargo check --workspace` green, `cargo test --workspace --lib` green.

22. **Commit**: one commit per service.

## Service punch list

Sorted roughly by complexity (ascending — start with smaller).

| # | Service | Async LOC | REAPER LOC | Standalone LOC | Notes |
|---|---|---|---|---|---|
| ✅ | markers | 230 | 391 + 162 sync + 103 remote | 548 + 93 sync | DONE |
| ✅ | tracks | 275 | 1486 + 203 sync + 182 remote | 422 + 131 sync | DONE |
| 1 | **regions** | 120 | TBD | TBD | shape matches markers — same lanes / range / goto retirements |
| 2 | **transport** | TBD | TBD | TBD | per-project play/stop/rec/loop. No project-list semantics. |
| 3 | **tempo_map** | 108 | TBD | TBD | get/set tempo + time-sig points. Some streaming. |
| 4 | **takes** | TBD | TBD | TBD | per-item take CRUD. Mirrors items shape. |
| 5 | **ext_state** | TBD | TBD | TBD | project-scoped key-value. Trivial port. |
| 6 | **fx_chains** | TBD | TBD | TBD | enumerate chain on a track/take. Stateless. |
| 7 | **fx_params** | TBD | TBD | TBD | get/set a param on an FX node. |
| 8 | **routing** | 160 | TBD | TBD | sends/receives/hw outputs. Decent surface. |
| 9 | **items** | 348 | TBD | TBD | items + per-item take iteration. Sibling-trait for take details. |
| 10 | **fx** | 502 | TBD | TBD | biggest service. Plugin/chain/param/preset CRUD. |
| 11 | **midi** | 238 | TBD | TBD | per-take MIDI editing. |
| 12 | **automation** | 161 | TBD | TBD | envelope + point CRUD. |
| 13 | **live_midi** | 75 | TBD | TBD | live MIDI streaming. Mostly a stream → defer or sibling trait. |
| 14 | **audio_engine** | TBD | TBD | TBD | start/stop/status of REAPER's audio. Singleton state. |
| 15 | **action_registry** | TBD | TBD | TBD | REAPER action lookup. Probably stateless. |
| 16 | **toolbar** | TBD | TBD | TBD | toolbar layout state. |
| 17 | **window_geometry** | TBD | TBD | TBD | window pos/size. Has subscribe. |
| 18 | **plugin_loader** | TBD | TBD | TBD | external plugin loading. |
| 19 | **dawfile_service** | TBD | TBD | TBD | open/save/import project files. |
| 20 | **input** | TBD | TBD | TBD | input device enumeration. |
| 21 | **health** | TBD | TBD | TBD | health probe. Trivial. |
| 22 | **screenset** | TBD | TBD | TBD | named screen captures. |
| 23 | **position_conversion** | TBD | TBD | TBD | utility methods, may stay async. |
| 24 | **audio_accessor** | 39 | TBD | TBD | sample-level audio reads. May need careful retire. |
| 25 | **peak** | 37 | TBD | TBD | peak meter samples. Mostly streaming. |
| 26 | **dock_host** | TBD | TBD | TBD | already async-shaped (vox::service), assess separately |
| 27 | **batch** | TBD | TBD | TBD | batch dispatch glue — retire after the rest? |

The async-LOC column is the existing async `<Foo>Service` trait size.
The REAPER/Standalone columns are the impl sizes that will shrink to
roughly 30–60% under the port.

## Pre-port survey (do before starting each service)

Quick check to scope the port:

```bash
# How much LOC will get deleted/rewritten?
wc -l crates/daw-proto/src/<feature>/service.rs \
      crates/daw-proto/src/sync/<feature>.rs \
      crates/daw-reaper/src/<feature>.rs \
      crates/daw-reaper/src/sync/<feature>.rs \
      crates/daw-reaper/src/remote/<feature>.rs \
      crates/daw-standalone/src/<feature>.rs \
      crates/daw-standalone/src/sync/<feature>.rs

# Method count + retirement candidates
grep -c "async fn " crates/daw-proto/src/<feature>/service.rs
grep "Tx<" crates/daw-proto/src/<feature>/service.rs   # streaming methods
grep "subscribe" crates/daw-proto/src/<feature>/service.rs

# Facade usage — what daw-control calls
grep "self.clients.<feature>\." crates/daw-control/src/<feature>.rs

# Test usage
grep -l "<Feature>Service\|init_<feature>_broadcaster" crates/daw-reaper/tests/
```

## Things to watch out for

### 1. Verb collision on `Reaper`

`Reaper` impls multiple service traits, and many verbs (`all`, `get`,
`count`, `add`, `remove`, `rename`, `set_color`, `set_volume`, …)
appear on more than one. Inside impl bodies and in `batch/dispatch.rs`
this manifests as E0034 "multiple applicable items in scope".

**Fix**: use fully-qualified UFCS at the callsite:

```rust
Markers::all(svc, ctx)       // not svc.all(ctx)
Tracks::set_color(svc, ctx, t, c)
```

In-process callers with a concrete `Reaper` value: `Reaper as Tracks`
or `Tracks::method(reaper, …)`. Most call sites don't see this because
the daw-control facade passes through to a specific client type.

### 2. Reaper backend type name vs other crates

`crate::Reaper` (struct) vs `reaper_high::Reaper` (REAPER bindings).
Inside `daw-reaper/src/*.rs` files, import as `reaper_high::Reaper as
ReaperHigh` and let the bare name refer to your singleton:

```rust
use reaper_high::Reaper as ReaperHigh;
// ...
impl Tracks for crate::Reaper { ... }
let medium = ReaperHigh::get().medium_reaper();
```

### 3. `daw-control` `.await??` pattern

Sync trait methods returning `DawResult<T>` → vox client wraps as
`Result<DawResult<T>, vox::Error>`. The facade pattern is:

```rust
self.clients.foo.method(self.context(), args).await??
//                                            ^^^ two ?
```

`From<DawError> for Error` already exists in `daw-control/src/error.rs`
— don't redo it.

### 4. Glob re-export collisions in `daw-proto/src/lib.rs`

`pub use foo::*` will pull `serve` / `descriptor` / `Dispatcher`
auto-emitted names. Replace with explicit re-exports of the
data/trait/error types:

```rust
pub use foo::{Foo, FooRpc, FooError, FooEvent, /* + data types */};
#[cfg(feature = "vox")]
pub use foo::FooClient;
```

### 5. Standalone state reuse

`StandaloneState` already has fields for most domains (`tracks`,
`markers`, `regions`, `routing`, etc.). The new sync impl should
operate on those directly via `with_project` / `with_project_mut` —
don't create a parallel `FooState` struct.

### 6. Trait method visibility

`#[architect::rpc]` requires methods to be public (the bridge needs
to call them through the trait). User-trait methods must be `fn`
(not `pub fn` — they inherit from the trait).

### 7. Batch dispatch error conversion

Old async dispatch swallowed errors silently (mutators were
`async fn -> ()`). New sync mutators return `DawResult<()>`. Map to
`String` for the batch step error channel:

```rust
Tracks::set_muted(svc, ctx, tr, *v).map_err(|e| e.to_string())?;
```

### 8. Streaming subscribe methods

Sync trait can't carry `Tx<FooEvent>` or `async fn subscribe`. Three
options per service:

- **Drop entirely** (markers + tracks chose this). Sibling trait if
  revived.
- **Sibling async trait** `FooStream` decorated with
  `#[vox::service]` directly. Mount as a second service. Backends
  impl both.
- **Mixed-mode trait** with sync verbs + `async fn watch_changes()`.
  My macro supports this; it bridges sync methods and passes async
  through. Use sparingly — keeps the trait less coherent.

Default to dropping. Add back only when a real consumer needs it.

### 9. Cache thread-local for project lookup

`daw-reaper/src/track.rs` has `CURRENT_PROJECT_CACHE` thread-local
for fast project resolution. If other services depend on it
(`set_project_cache` / `clear_project_cache` are `pub(crate)`),
preserve them when porting tracks → port-target-service ordering matters.

## Per-service decision template

Copy this into the commit message of each port:

```text
feat(<feature>): port to architect::rpc

## Trait shape
Methods kept: [list]
Methods retired (sibling-trait territory): [list]

## Net LOC delta
daw-reaper/src/<feature>.rs:        X → Y  (delta)
daw-standalone/src/<feature>.rs:    X → Y  (delta)
sync/<feature>.rs (3 crates):       X → 0
remote/<feature>.rs (daw-reaper):   X → 0
daw-proto/src/<feature>/service.rs: X → Y
daw-control/src/<feature>.rs:       X → Y
Total LOC reduction: ~N

## Retired
[list of retired methods + reason: streaming, visibility,
lane, hierarchy, chunks, etc.]

## Notes
[any service-specific weirdness]
```

## When to stop

The pattern is mature enough by service #5 that future ports should
take ~1 hour of focused work each. Three meta-stops:

- **After service ~10**: revisit this doc. Some retirements may need
  reviving on sibling traits (subscribe is the obvious one). Decide
  if that work belongs inline or separately.
- **After all services**: the `sync/` folder in `daw-proto` is empty
  except for `daw.rs` and `project.rs`. Decide whether to flatten
  those too.
- **After `Project` trait has no more accessors**: it may be possible
  to retire `Project` entirely and have `Reaper` impl a flat
  `ProjectInfo` trait directly. Re-evaluate then.

## Reference

- Architect framework: `../../Development/architect/DESIGN.md`
- Markers port (template): commit `00e37eb`, then the cleaner
  re-port at commit `44cf77f`
- Tracks port: follow-up commit on `refactor/architect-rpc-markers`
