# Web/wasm transport for vault-sync

Follow-up to the native `vault-sync` client
([`crates/vault-sync`](../crates/vault-sync)). Today the
crate uses `reqwest` (with `rustls-tls`) and
`tokio-tungstenite` — both native-only. The web app
(`apps/web`, wasm32-unknown-unknown) can't link it.

## Goal

`apps/web` can call `manifest()` / `get_file` / `put_file` /
`delete_file` and `subscribe()` against `task-server` from
inside a wasm bundle, using the browser's `fetch` + native
`WebSocket`.

## Approach

Split the crate into a transport-agnostic core plus per-target
transports:

```
crates/vault-sync-core/        — types only (ManifestEntry,
                                  VaultEvent, IfMatch, Error,
                                  URL shaping). No I/O.
crates/vault-sync/             — native transport (reqwest +
                                  tokio-tungstenite). Re-exports
                                  vault-sync-core.
crates/vault-sync-web/         — wasm transport (reqwest's wasm
                                  feature OR `gloo-net::http` +
                                  `gloo-net::websocket`). Re-
                                  exports vault-sync-core.
```

Both transports expose the same `VaultClient` surface so call
sites only switch at the `use` line behind a
`#[cfg(target_arch = "wasm32")]`. The core crate stays small
enough that duplication isn't tempting.

### Why not one crate with feature gates?

Tried mentally. `reqwest` on wasm needs
`default-features = false, features = ["json"]` and *no*
`rustls-tls`; on native it needs `rustls-tls`. Negative
features (`tokio-tungstenite` only on native) are brittle.
Two transport crates with one shared core is the path the
existing `task-ui` crate already takes for vox.

## Open questions

1. **WS auth**: browser `WebSocket` can't set request headers,
   so any future auth needs to be query-param or
   subprotocol-based. Worth deciding now before the vox
   migration locks the answer in.
2. **Local mirror?**: should the web app keep an
   IndexedDB-backed `vault::Vault` mirror (so it's usable
   offline), or stay strictly online for v1? IndexedDB
   mirror is what makes Obsidian Sync on iOS feel native;
   matters less for desktop-shaped web users. Start online-
   only; revisit when mobile becomes the priority.
3. **Streaming uploads**: `reqwest`'s wasm path doesn't
   support streaming bodies. Large media won't go through
   this client anyway (separate `attachments` flow), so the
   restriction is probably fine.

## Slice plan

1. Extract `crates/vault-sync-core` with types + Error +
   `Url` builders. No async, no transport. Move unit tests
   that don't touch network here.
2. Convert `vault-sync` to depend on core; only the
   `VaultClient` impl + `Subscription` stay.
3. Add `crates/vault-sync-web` mirroring the same surface
   over `gloo-net`. Build with
   `cargo check --target wasm32-unknown-unknown -p vault-sync-web`.
4. Wire `apps/web` to the web client. Probably polling the
   manifest on a timer for v1; subscribe lands when the auth
   story is decided.

## Out of scope

- Auth (the user explicitly deferred it). When it lands it
  needs to work for both browser `WebSocket` (no headers)
  and native (`tokio-tungstenite`) — see
  [`vault-sync-vox-migration.md`](vault-sync-vox-migration.md).
- Native code refactor — `vault-sync` keeps its existing
  surface; only its internals shrink.
