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

## Run the suite (Nix shell — recommended)

The `playwright` dev shell bundles node + a Nix-managed Chromium
+ all the Wayland/X11/GTK libs Playwright needs. From the repo
root:

```sh
nix develop .#playwright --command just test-browser
```

That runs `npm install` (just to fetch `@playwright/test`; no
browser download — `PLAYWRIGHT_BROWSERS_PATH` is preset to
nixpkgs's `playwright-driver.browsers`) and then runs the suite.

## Run the suite (manual / non-Nix)

If you're not on Nix, install node ≥ 20 and Chromium yourself,
then:

```sh
cd tests/playwright
npm install
npx playwright install chromium     # ~150MB browser download
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

## ⚠️  `dx serve` hot-patch gotcha

`dx serve`'s incremental hot-patch **does not** pick up new RSX
attribute additions — only function-body changes. If you add an
`id="…"` or `data-testid="…"` to a component and the playwright
tests fail with "element not found", the running `dx serve` is
still serving the wasm bundle from before your change. Fix:

```sh
nix develop .#playwright --command just test-browser-fresh
```

That kills any running `dx serve` + `task-server` and reboots
both fresh (CI=1 so `reuseExistingServer` is off). Costs ~3 min
extra for the cold rebuild but guarantees the test sees current
code.

Selector changes that DO survive hot-patch:
- Text content changes inside an element.
- Class string additions on an existing element.
- Function-body logic (which testid resolves to which row, etc.).

Selector changes that DON'T survive hot-patch:
- Brand-new `id="…"` or `data-testid="…"` attributes.
- New elements introduced by an `if` branch that didn't fire
  before.
- New routes registered in the `Route` enum.
