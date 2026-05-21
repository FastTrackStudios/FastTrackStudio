# Vault-sync vox migration — SHIPPED

Status: **done.** Kept as the design record for why the
file-replication layer rides on vox like every other feature.

## What shipped

- `crates/vault-sync-proto` — `#[vox::service] trait VaultSync`
  with `manifest`, `get_file`, `put_file`, `delete_file`, and
  `subscribe(Tx<VaultEvent>)`. Payload types are
  `#[derive(Facet)]` + `vox_types::Reborrow`. Wasm-clean.
- Server: `apps/server/src/vault_sync.rs` exposes
  `VaultSyncState` (filesystem root + per-vault broadcast
  channels) and implements `VaultSync` on it directly. Mounted
  as one more arm in `vox_ws_handler` alongside `ProjectRepo` /
  `WorkspaceSync` / `AttachmentService` / etc. No separate REST
  routes; no second WS upgrade.
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

## Out of scope (still open)

- Desktop multi-server wiring (Local vs Remote backends) —
  `plans/vault-sync-desktop-multiserver.md`. The new
  `VaultSyncClient` is what the `Remote` variant wraps.
- Per-vault encryption-at-rest — deferred; TLS in transit +
  OS-level disk encryption only.
