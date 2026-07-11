# Multiplayer conformance suites

Continuous proof that every client of the same org converges to the
same state while connected: **5-way collaborative editing** on a
vault note and **20-peer presence churn**, driven through the real
web app (dx-built wasm bundle) with Playwright multi-context —
one browser context = one presence peer. Tracked issue `dd824506`.

## Run

```sh
just mp-test
# or, equivalently:
nix develop .#playwright --command tests/multiplayer/run.sh
# reuse already-built artifacts (server, CLI, wasm bundle):
nix develop .#playwright --command bash -c 'cd tests/multiplayer && MP_SKIP_BUILD=1 npx playwright test'
```

`run.sh` builds `task-server` + `task-cli` (debug) and the web
bundle with `TASK_VOX_URL_WEB=ws://127.0.0.1:$MP_SERVER_PORT/vox`
**baked at compile time** (`crates/ui/src/vox_session.rs`), then
runs Playwright. Building the bundle up front — instead of inside a
`webServer` block — is the warmup that absorbs the dx cold-build
flake; the tests are served static bytes by `serve.js`.

The whole stack is isolated per run (`global-setup.js`):

- throwaway `TASK_DATA_ROOT` (mktemp), never the dev data root;
- org `mptest` + the 4 dev accounts seeded through the CLI binary
  (`task org init` / `task auth signup`, with `XDG_DATA_HOME`
  redirected so the dev session.json is untouched);
- `task-server` on `MP_SERVER_PORT` (default **18091** — never the
  dev server's 18080), static bundle on `MP_WEB_PORT` (18092);
- fail-fast guards: ports must be free (a squatter would be silently
  tested instead), the wasm must contain the expected baked vox URL,
  the `__taskVault` hook, and no unresolved `env` imports (a bundle
  built outside the nix shell loses the tree-sitter C grammars).

Identity model: each context seeds `localStorage["task.auth.active"]`
before boot, so the app's boot restore performs the REAL dev-account
sign-in; presence display name = account name. Per-tab
`sessionStorage` mints each context a distinct presence client key,
so 20 peers over 4 accounts are 20 roster rows (and a reload keeps
the same key — no duplicate after refresh-rejoin).

Doc text is read from `window.__taskVault` (a wasm-side mirror of
the exact editor buffer + collab status + local replica text, see
`crates/ui/src/pages/vault.rs`) — the decorated DOM cannot be
scraped back into doc text because hidden live-preview ranges render
nothing.

## Suites & current status

| spec | test | status today | meaning |
|---|---|---|---|
| `convergence.spec.js` | baseline: 2 peers, light traffic | **passes** | full pipeline works: editor → replica → server → disk → peers, remote cursors, no console errors |
| `convergence.spec.js` | storm: 5 peers, concurrent edit storm | **expected-fail** (`test.fail`) | blocked by Finding 1 below |
| `offline-return.spec.js` | offline/return re-convergence | **skipped** (`test.fixme`) | blocked by P0 `6303584a` (dead connections don't recover); unskip when it merges |
| `presence-churn.spec.js` | baseline: 3 peers, roster agreement + DND propagation | **passes** | org presence stack works under light traffic |
| `presence-churn.spec.js` | churn: 20 peers, seeded schedule, 3 checkpoints | **expected-fail** (`test.fail`) | blocked by Finding 1 below |

When a fix lands, the `test.fail()` tests will report
"passed unexpectedly" — that's the signal to delete the marker.

## Findings (verification results, 2026-06-12)

### 1. vox downstream credit starvation — every server→client stream freezes after 16 messages

The defining failure of both full scenarios. Reproduced minimally
(2 peers, one-way 600 ms-paced typing): peer B receives the attach
backlog + exactly **15** updates, then **never another byte** —
`backlog + 15 = 16 = DEFAULT_INITIAL_CHANNEL_CREDIT`
(`vox-types/src/message.rs`). Meanwhile:

- B's **uploads keep working** — the server merged ALL 40 of A's and
  all of B's edits; the on-disk note (write-behind) was perfect.
- `SyncStatus` stays **Live** on both sides; zero console or server
  errors. Pure silent divergence.
- The per-file **cursor presence channel freezes identically** (A's
  remote caret vanishes from B ~30s later when the entry expires).
- The org **roster channel freezes too** once its own 16-message
  window is spent (initial entries + 10s heartbeats ≈ 30–90s with a
  few peers) — which is why the 2-minute churn cannot pass: every
  client's roster becomes a frozen snapshot; departures never
  expire, joiners never appear.

Mechanics: vox flow control is credit-based; the consuming side's
`on_item_consumed` should send `GrantCredit` after `initial/2 = 8`
items. Server-side Rx replenishment works (up-channels never stall);
the **wasm client's grants never reach the server**, so
`architect::PubSub::drain_one` gets `try_send → Full` forever and
re-queues into the mailbox (which grows unboundedly, a slow server
leak). Likely the observable root of — or a sibling to — P0
`6303584a`: sessions look alive but are half-dead.

### 2. Boot race: empty-slug org dial → dropped wasm-bindgen closure

On some boots the app dials `ws://…/org//vox` (empty slug — org
discovery hasn't resolved yet), the 404'd WebSocket's queued
callback then fires a dropped closure: `Error: closure invoked
recursively or after being dropped` 0–1 ms later. This is the exact
error class `tests/playwright/smoke.spec.js` guards against. The
harness counts these as `knownBootRaces` (only when they follow an
empty-slug failure within 5s) instead of failing; the same error in
any other context still fails the run. Fix belongs near
`crates/ui/src/vox_clients.rs` / org discovery sequencing.

### 3. Known noise (allowlisted, see `helpers.js`)

- `ws://…/_dioxus?build_id=0` failure — dx DEBUG bundles ship the
  devtools client; harmless against a static server.
- Dioxus signal-ownership WARNs: `CollabSession`-owned Copy values
  read from the `Editor` scope (`crates/ui/src/collab.rs:90` /
  `:312`, `crdt/src/hooks.rs:300`) — flagged by dioxus as a
  potential use-after-drop; worth a look independent of this suite.
- `Changing the props of Style {} is not supported` (vault page's
  `document::Style` with the collab CSS).

## Harness pieces

- `global-setup.js` / `global-teardown.js` — seeded org + server +
  static web, torn down by pid; state handed to specs via
  `.mp-state.json`.
- `helpers.js` — `settle()` (bounded predicate waiter with optional
  stability window — no bare sleeps in assertions), `mulberry32`
  (seeded RNG; churn schedule is deterministic per `MP_SEED`),
  `Peer` (context + sign-in + vim-aware editor driving + roster
  parsing + console capture), `dumpArtifacts` (on failure: per-peer
  doc text, replica text, vox state, roster snapshot, console ring,
  plus the on-disk note and a server-log tail, into the test's
  output dir).
- Editor driving details: vault editor boots in vim **Normal** mode
  (click + `i`, adaptively undone if the editor was already
  inserting); caret placement walks `[data-tile-pos]` tiles (same
  technique as `Editor/tests/editor.spec.js`), skipping remote-caret
  widget DOM; `[[` autocomplete is driven for real (type `[[query`,
  wait for `.completion-menu`, Enter).

## Knobs

| env | default | meaning |
|---|---|---|
| `MP_SERVER_PORT` | 18091 | isolated task-server port (baked into the wasm — rebuild if changed) |
| `MP_WEB_PORT` | 18092 | static bundle port |
| `MP_SEED` | 3551505 | churn schedule seed |
| `MP_SKIP_BUILD` | – | `1` = reuse existing artifacts |
| `MP_SERVER_LOG` | `task_server=info` | RUST_LOG for the spawned server |

## Flake mitigations

- dx cold-build absorbed by building before Playwright starts; the
  bundle is then static (no rebuild overlay, no hot-reload races).
- No sleeps in assertions — `settle()` everywhere, with stability
  windows where "stays converged" matters.
- Deterministic churn schedule (seeded RNG); seed printed in the
  run log and included in failure artifacts.
- Port-squatter / wrong-bundle / missing-toolchain guards fail the
  run in global-setup with explicit instructions.
- Presence ghost-bleed between specs (entries of closed contexts
  linger ≤30s) is outwaited explicitly where rosters are asserted.
- Retries are 0 by design: for a conformance suite, a flake is a
  finding.
