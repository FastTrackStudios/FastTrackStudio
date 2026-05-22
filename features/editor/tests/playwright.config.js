// @ts-check
// Playwright config for the editor's browser test suite.
// Modeled on Dioxus's own playwright-tests setup at
// `~/Development/research/dioxus/packages/playwright-tests/playwright.config.js`.
//
// The webServer entry runs `dx serve --platform web` on a fixed
// port for the duration of the test run. Tests connect to
// http://localhost:8090 (chosen to avoid collisions with the
// default `dx serve` port of 8080).
//
// Run with:
//   pnpm install         # or `npm install`
//   pnpm playwright install chromium
//   pnpm test
//
// Headed / UI modes:
//   pnpm test:headed
//   pnpm test:ui

const { defineConfig, devices } = require("@playwright/test");
const path = require("path");

// Distinct from `dx serve`'s default (8080) and from 8090 (which
// users might have a long-running dev session on). Override via
// `PW_PORT=…` if 9091 is also taken.
const PORT = parseInt(process.env.PW_PORT || "9091", 10);
const repoRoot = path.resolve(__dirname, "..");

module.exports = defineConfig({
  testDir: ".",
  fullyParallel: false, // one editor, sequential tests
  forbidOnly: !!process.env.CI,
  retries: process.env.CI ? 2 : 0,
  workers: 1,
  reporter: process.env.CI ? "list" : "html",
  use: {
    baseURL: `http://localhost:${PORT}`,
    trace: "retain-on-failure",
    screenshot: "only-on-failure",
  },
  // Generous timeouts — the first run includes a wasm rebuild.
  timeout: 60_000,
  expect: { timeout: 5_000 },

  webServer: {
    // Force the web platform regardless of Dioxus.toml's default
    // (which is "desktop" for local dev). dx serve will build
    // the wasm bundle and host it.
    command: `dx serve --package playground --platform web --addr 127.0.0.1 --port ${PORT}`,
    cwd: repoRoot,
    port: PORT,
    // First-time wasm build can be slow.
    timeout: 10 * 60 * 1000,
    reuseExistingServer: !process.env.CI,
    stdout: "pipe",
    stderr: "pipe",
  },

  projects: [
    {
      name: "chromium",
      use: { ...devices["Desktop Chrome"] },
    },
  ],
});
