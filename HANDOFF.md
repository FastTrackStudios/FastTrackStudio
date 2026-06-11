# Handoff — real-time collaboration wave (2026-06-10)

The full roadmap from `docs/content/architecture/collaboration.md` is
implemented: architect now ships offline-first, real-time collaborative
state end to end — one `#[architect(crdt)]` attribute on an entity, one
`use_synced_doc` call at the app root, and every client holds a merging
replica of the doc. This file is the orientation map for whoever picks
the repo up next.

## What shipped, where

| Piece | Where | Notes |
| --- | --- | --- |
| Sync transport (`DocSync`, `DocSyncHost`, `SyncedDoc`) | `libs/crdt/src/sync.rs` | **Bidirectional** catch-up: `sync` returns the server's version vector; the client pushes back history the server lacks (offline-edit → restart → reconnect is lossless). Relayed updates are persisted explicitly (`CrdtDoc::apply_remote_durable`) — imports don't fire loro's local-update subscription. |
| Presence (`DocPresence`, `PresenceHost`, `PresencePeer`/`PresenceDriver`) | `libs/crdt/src/sync.rs` | Loro `EphemeralStore` payloads over a *sliding* PubSub (droppable, unlike doc updates). Host keeps a mirror so late joiners get the current picture; peers re-announce their keys on reconnect. |
| Compaction + shallow bootstrap | `libs/crdt/src/sync.rs` | `DocSyncHost::with_compaction(n)` folds the update log into a snapshot every n updates (channel → worker holding a **weak** doc handle — see gotcha below). `with_shallow_bootstrap()` serves fresh joiners a shallow snapshot. |
| Dioxus hooks | `libs/crdt/src/hooks.rs` (feature `dioxus`) | `use_synced_doc(_with)` (replica + revision Signal + `SyncStatus` + auto-resyncing driver), `use_crdt_list/entry`, `use_presence_channel`/`use_presence`. Doc change subscription → channel → Signal bump (never write signals from loro callbacks). |
| Client persistence | `libs/crdt/src/fs.rs` (native), `libs/crdt/src/indexeddb.rs` (wasm, feature `indexeddb`) | `Persistence` trait drops its `Send` bounds on wasm (`async_trait(?Send)` split); `CrdtDoc::open` works on both targets (persist writes drain through a channel into a `spawn`/`spawn_local` writer). |
| `#[architect(crdt)]` derive flag | `macros/architect-derive/src/lib.rs` (`build_crdt_block`) | Emits the `EntityCrdt` codec (field types → `crdt::codec` calls, same `on_create`/`on_update`/`exclude` policy as SeaORM), `<E>RepoLoro`, and — under `crdt`+`atom` — `use_<e>_crdt_list/„_crdt`/`<E>CrdtActions`. Requires `repo` + a Uuid pk. |
| Example showcase | `examples/app/` | `Note` entity (`example-proto/src/note.rs`, `COLLAB_DOC_ID`), server `Collab` bundle (`app-server::Collab` — file-persisted doc, compaction, presence; mounted per connection over shared hubs), `/collab` page (`ui/src/pages/collab.rs`): live notes, sync badge, presence strip. |

## Consumer conventions (feature wiring)

```toml
# proto crate
crdt = ["dep:crdt", "crdt/vox"]
atom = ["architect/atom", "crdt?/dioxus"]   # weak edge lights the hooks up
# UI crate
crdt = { path = "...", features = ["dioxus"] }
```

## The gotchas that cost real debugging time

1. **Never let a loro subscription closure own a strong `CrdtDoc`.**
   Loro fires callbacks synchronously under its subscriber-registry
   lock; if the closure is the doc's last owner, the doc drops inside
   unsubscribe and deadlocks on the same lock. Symptom: the test body
   finishes but the process never exits (nextest TIMEOUT with all
   asserts green). Hold `CrdtDoc::downgrade()` and push work through a
   channel — `DocSyncHost::new`'s compaction worker is the reference.
   Regression guard: `sync_convergence::establish_then_close`.
2. **Never prune/mutate an `EphemeralStore` from a render path.**
   `remove_outdated()` fires the store's change subscription; a render
   that prunes re-renders itself forever — in the browser that's a
   main-thread livelock followed by a tab crash (this was the "/collab
   freezes the app" bug). `PresencePeer::states()` is read-only; expiry
   runs on a timer in `use_presence_channel`'s sweep task
   (`PresencePeer::sweep`).
3. **A browser WebSocket's connection-failure `error` event is a plain
   `Event`, not an `ErrorEvent`.** vox-websocket read `.message()` off
   it → JS `undefined` → `passStringToWasm0` threw inside the
   wasm-bindgen import → the app wedged whenever the server was
   unreachable. Fixed in the vox fork (`1db8b139`, downcast +
   fallback); architect pins that rev. `console_error_panic_hook` is
   now installed in `app-web` so future panics print real messages
   instead of `RuntimeError: unreachable`.

Browser-only failure modes like 2 and 3 are exactly what the headless
e2e (`just web-e2e`, `examples/app/web/e2e/collab.spec.mjs`) guards:
it drives the real wasm app in system Chromium, clicks into /collab,
creates a note, and asserts the main thread stays responsive and the
tab never crashes.

## Verification status

- `cargo nextest run -p crdt` — 14/14 (convergence, late joiner,
  offline-restart push-back, shutdown teardown, file persistence).
- `cargo nextest run -p app-tests-e2e` — 8/8, including
  `notes_replicas_converge_over_websocket` and
  `presence_propagates_between_peers` over the real axum server.
- `just web-e2e` — headless-browser e2e (real server + dx serve +
  system Chromium): 10/10, including main-thread responsiveness and
  no-tab-crash guards.
- Run the demo: `just dev`, open `http://localhost:8765/collab` in two
  windows. Kill the server mid-edit to see offline buffering + delta
  reconnect (`COLLAB_DATA_DIR` defaults to `./collab-data`). With the
  server down the page now degrades gracefully (offline badge, edits
  buffer locally).

## Known follow-ups (not blocking)

- **Old server + new client**: a client whose `DocSync` service isn't
  mounted server-side spams reconnects (the `/collab` page itself keeps
  working locally). Worth a friendlier "service unavailable" surface on
  `ClientError` someday.
- `use_synced_doc` supports one doc per app (context-keyed). Multi-doc
  apps (Task: one doc per project) need a keyed variant — the underlying
  pieces (`SyncedDoc`, `DocHandle`) already support it.
- IndexedDB persistence compiles for wasm but has no automated browser
  test; exercised manually via `use_synced_doc_with` +
  `IndexedDbPersistence::open`.
- DAW asks still open: realtime bounded-queue dispatcher, windowed/
  paginated store for huge tables.

## Docs

- `docs/content/architecture/collaboration.md` — rewritten for the
  implemented surface (was the roadmap).
- Memory: `loro-subscription-drop-deadlock.md` in the project memory dir.
