# Browser tests (Playwright)

Smoke + realtime-sync coverage for `apps/web` rendering the project route. Modeled on the Dioxus repo's `packages/playwright-tests/` setup — single
`playwright.config.js` with a `webServer` array that boots both the
vox server and `dx serve` for the test run.

## Layout

```
tests/playwright/
├── package.json            # @playwright/test dep only
├── playwright.config.js    # workers=1, two webServers
├── projects.spec.js        # the actual tests
└── README.md               # you are here
```

## First-time setup

```sh
cd tests/playwright
npm install
npx playwright install chromium
```

## Run the suite

```sh
cd tests/playwright
npx playwright test
```

The config's `webServer` block boots both processes for you:

- `cargo run --release -p task-server` with `TASK_SERVER_SEED=1`
  (binds `127.0.0.1:9090`, populates 1 project + 80 tasks before
  the listener starts).
- `dx serve --web --port 8765` from `apps/web`.

`reuseExistingServer` is on outside CI, so if you already have
`just dev` running the tests skip the boot and just hit your
existing servers.

The first run is slow because it builds task-server in release
mode and the wasm bundle from scratch (~3 minutes total). Cached
builds drop to ~10 seconds.

## What's covered

- **`projects route loads + tasks render`** — page mounts, vox
  session establishes, snapshot import bumps the version badge
  past `v0`, at least one `[data-testid^="task-row-"]` row
  appears.
- **`toggle checkbox flips data-task-done locally`** — single-tab
  read-after-write. Proves the local CrdtDoc + `use_resource`
  re-render path works end to end.
- **`two tabs sync via WorkspaceSync`** — opens two separate
  browser contexts (different cookies + storage = different
  peers from the server's POV), toggles a checkbox in one, and
  asserts the matching row in the other tab flips its
  `data-task-done` attribute. This is what catches regressions
  in the realtime push path.

## Stable selectors (UI contract)

`features/project/project-ui/src/live.rs` exposes:

| Element                   | Selector                                            |
|---------------------------|-----------------------------------------------------|
| Route container           | `#projects-route`                                   |
| Version badge             | `[data-testid="version-badge"]`                     |
| Task row                  | `[data-testid="task-row-<uuid>"]`                   |
| Done state on a row       | `[data-task-done="true"]` or `false`                |
| Task checkbox             | `[data-testid="task-checkbox-<uuid>"]`              |

If you change those, update both the UI component and these specs.

## Debugging a flake

1. `npx playwright test --ui` — opens the inspector with
   a step-through view of every action.
2. Failed runs save a Playwright trace to `test-results/`. Open
   with `npx playwright show-trace test-results/<dir>/trace.zip`.
3. Server logs from the test run are interleaved into stdout via
   the config's `stdout: "pipe"` setting.
