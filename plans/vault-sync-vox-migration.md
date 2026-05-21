# Should vault-sync ride on vox?

## The question

The user asked for the WS push "via vox". The shipped slice
([`apps/server/src/vault_sync.rs`](../apps/server/src/vault_sync.rs))
uses a plain `axum::extract::ws::WebSocket` with a
`tokio::sync::broadcast` channel — not vox. This was a
pragmatic choice to unblock testing; whether to refactor onto
vox is an open architectural decision.

## Why I didn't reach for vox first

- **vox is RPC-shaped.** Every existing vox service in the
  repo is `#[vox::service]` with typed methods + return
  types (project sync, knowledge sync, attachments). Server-
  initiated broadcast of file events doesn't fit that shape
  naturally — clients don't *call* a method to receive
  events; they want a topic-style subscription.
- **vox already has a `subscribe` pattern** in
  [`apps/server/tests/sync.rs`](../apps/server/tests/sync.rs)
  (`WorkspaceSyncClient::subscribe(doc_id, tx)`). That's
  the precedent we'd follow — the broadcast becomes the
  return value of a long-running RPC. So it's doable; just
  more plumbing.
- **Same WS, different framing.** vox runs over its own
  websocket transport
  (`features = ["transport-websocket"]`); we'd be standing
  up a second route alongside `/vox` either way, or
  multiplexing vault events onto the existing one.

## What "vox-native" would look like

```rust
#[vox::service]
trait VaultSyncService {
    async fn manifest(&self, vault_id: String) -> Result<Manifest>;
    async fn get_file(&self, vault_id: String, path: String)
        -> Result<Bytes>;
    async fn put_file(&self, vault_id: String, path: String,
                      body: Bytes, if_match: IfMatch)
        -> Result<PutAck>;
    async fn delete_file(&self, vault_id: String, path: String,
                          if_match: IfMatch) -> Result<()>;
    async fn subscribe(&self, vault_id: String,
                        tx: vox::Tx<VaultEvent>) -> Result<()>;
}
```

Pros:
- Wasm-friendly today (vox `runtime` already builds for
  wasm; see workspace `vox` dep config).
- Auth, retries, reconnect plumbing comes for free from the
  vox client harness.
- One transport for everything; one place to add auth /
  rate-limiting.

Cons:
- File bodies through vox means encoding them through vox's
  message format (probably bincode/postcard). Fine for
  markdown (<1 MB pages), awkward for large media — but
  large media goes through `attachments` anyway.
- Migration work: rewrite the client crate, retire the
  axum routes, update tests.

## Recommendation

**Migrate before more clients lock in.** The native client
(`vault-sync`) has one consumer (the e2e test) and one
example (`watch.rs`). The desktop multi-server plan is
unstarted; the web transport plan is unstarted. Refactoring
to vox now costs <a day; doing it after both clients are
written is much worse.

Decision points still open:
- Naming: a single `VaultSyncService` per server, or per-
  vault subdivision? Probably one service, vault_id passed
  per-call (matches the REST URL shape).
- Subscribe contract: does the server replay missed events
  on reconnect (requires a sequence number), or always send
  `Resync` and let the client re-pull the manifest? Current
  WS impl already sends `Resync` on `Lagged`; we should
  keep that simple semantics.
- Auth: vox already has a middleware layer
  (`AuthClientMiddleware` — see
  `crates/task-ui/src/vox_session.rs`). vault-sync auth
  would plug into it. The user explicitly deferred auth,
  but the migration should leave the slot open.

## Slice plan

1. Add `vault-sync-proto` defining the `#[vox::service]`
   trait + types (shared between server and client, wasm-
   clean).
2. Implement the service in `task-server` against the same
   filesystem code path. Keep the REST routes alive in
   parallel for one release as an "escape hatch" /
   debugging surface (curl works).
3. Rewrite `vault-sync` (native) and the new `vault-sync-
   web` (per [`vault-sync-web-transport.md`](vault-sync-web-transport.md))
   as thin wrappers over the generated vox client.
4. Migrate the e2e test + `watch` example.
5. Retire the REST routes once the desktop is on the new
   client.

## Out of scope

- File hashing / If-Match semantics — unchanged regardless
  of transport.
- Conflict bytes-in-error response — vox can return a
  structured `Conflict { server_sha, server_bytes }` error
  directly, so this gets simpler.
