# Vault-sync architect::rpc migration — SHIPPED

Status: **done.** Kept as the design record for why the
file-replication layer rides on architect::rpc (and thus vox)
like every other feature.

## What shipped

- `crates/vault-sync-proto` — one canonical sync trait
  `VaultSync` decorated with `#[architect::rpc]`. Sync CRUD
  methods (`manifest`, `get_file`, `put_file`, `delete_file`)
  + async `subscribe(Tx<VaultEvent>)` (mixed-mode trait).
  Borrowed `&str` args in the sync signature; the macro
  rewrites to owned `String` for the async client mirror.
  Payload types (`Manifest`, `ManifestEntry`, `FileBytes`,
  `PutAck`, `IfMatch`, `VaultEvent`, `VaultSyncError`) are
  `#[derive(Facet)]` + `vox_types::Reborrow`. Wasm-clean.
- Server: `apps/server/src/vault_sync.rs` exposes
  `VaultSyncState` (filesystem root + per-vault broadcast
  channels) and implements `VaultSync` directly with sync
  methods. The state also `impl HasDispatcher` returning
  `TokioBlockingDispatcher` so each remote sync call runs
  inside `spawn_blocking` (the `std::fs` calls don't stall the
  async executor). Mounted via the architect-emitted
  `vault_sync_proto::serve(state)` mount verb as one more arm
  in `vox_ws_handler` alongside `ProjectRepo` / `WorkspaceSync`
  / `AttachmentService` / etc. On the wire the service is
  named `"VaultSyncRpc"` (architect's `#[rpc]` macro suffixes
  the hidden vox mirror trait). No separate REST routes; no
  second WS upgrade.
- The old `crates/vault-sync` native HTTP client crate has been
  deleted. Consumers use the architect-emitted
  `vault_sync_proto::VaultSyncClient` directly — same client
  builds for native (tests, desktop) and wasm (`apps/web`)
  because `vox` itself is target-agnostic. This obsoletes the
  separate `vault-sync-web-transport` plan.
- `apps/server/tests/vault_sync_e2e.rs` drives the real
  `VaultSyncClient` against a booted `task-server`. Three
  cases: `put → manifest → get`, `subscribe` stream observing
  PUT + DELETE, and the conflict round-trip carrying server
  bytes + sha inside `VoxError::User(Conflict)`.

## Notes

File bytes through vox encode as `Vec<u8>` inside `FileBytes` /
`PutFileArg`. Fine for markdown pages; large media still belongs
in the `attachments` flow (signed-URL HTTP PUT/GET), unchanged
by this migration.

The original "vox is RPC-shaped, our events are topic-shaped"
doubt didn't survive contact with the codebase: `vox::Tx<T>`
return-by-output-channel handles topic-style streaming cleanly
— see `WorkspaceSync::subscribe` and now `VaultSync::subscribe`.

Using `#[architect::rpc]` instead of `#[vox::service]` (the
shape daw uses for its 11+ ported services) means we get the
sync trait + async client mirror from one declaration: the
trait reads as plain sync code (`fn put_file(&self, vault_id:
&str, …)`), backends impl it directly with zero call-site
ceremony, and the architect bridge marshals each call through
the backend's `Dispatcher` for cross-process callers. No
wrapper arg structs (no more `PutFileArg` / `VaultIdArg`); the
macro rewrites the borrowed args to owned for the wire form.

## Out of scope (still open)

- Desktop multi-server wiring (Local vs Remote backends) —
  `plans/vault-sync-desktop-multiserver.md`. The new
  `VaultSyncClient` is what the `Remote` variant wraps.
- Per-vault encryption-at-rest — deferred; TLS in transit +
  OS-level disk encryption only.
