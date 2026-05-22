// @ts-check
const { defineConfig, devices } = require("@playwright/test");

/**
 * Playwright config for Task-architect.
 *
 * Two background servers run in parallel for the duration of the
 * test run:
 *
 *   1. task-server on :9090 — vox WebSocket endpoint, seeded with
 *      one project + N tasks before the listener binds.
 *   2. dx serve on :8765 — Dioxus dev server hosting the wasm
 *      bundle. Same port `crates/task-ui::vox_url()` hard-codes,
 *      so the in-page client connects to localhost.
 *
 * The pattern mirrors the Dioxus repo's `packages/playwright-tests/
 * playwright.config.js` (webServer array, port-keyed `cwd`,
 * `reuseExistingServer` for local dev).
 */
module.exports = defineConfig({
  testDir: ".",
  // Each spec needs the dev servers; we run one at a time so the
  // shared task-server's CRDT state doesn't see overlapping bursts
  // from unrelated tests.
  workers: 1,
  // Realtime assertions can take a moment for the WS round-trip.
  // 30s per test is plenty for the slice we have today.
  timeout: 30_000,
  expect: {
    // Generous timeout on `expect.poll` / `toHaveText` — the
    // realtime push is fast in practice but we don't want
    // flake on a slow CI box.
    timeout: 10_000,
  },
  retries: process.env.CI ? 2 : 0,
  use: {
    baseURL: "http://localhost:8765",
    trace: "retain-on-failure",
  },
  projects: [
    {
      name: "chromium",
      use: { ...devices["Desktop Chrome"] },
    },
  ],
  webServer: [
    {
      // task-server: vox endpoint + seed.
      // `TASK_SERVER_SEED=1` populates the workspace doc before
      // the listener binds, so the first browser request finds
      // tasks.
      command:
        "TASK_SERVER_SEED=1 TASK_SERVER_BIND=127.0.0.1:9090 cargo run --release -p task-server",
      url: "http://127.0.0.1:9090/health",
      reuseExistingServer: !process.env.CI,
      timeout: 180_000,
      cwd: "../..",
      stdout: "pipe",
      stderr: "pipe",
    },
    {
      // dx serve: serves the wasm bundle.
      command: "dx serve --web --addr 127.0.0.1 --port 8765",
      url: "http://127.0.0.1:8765",
      reuseExistingServer: !process.env.CI,
      timeout: 240_000,
      cwd: "../../apps/web",
      stdout: "pipe",
      stderr: "pipe",
    },
  ],
});
