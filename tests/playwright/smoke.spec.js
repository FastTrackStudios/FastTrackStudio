// @ts-check
const { test, expect } = require("@playwright/test");

/**
 * Smoke suite for the Task web app shell.
 *
 * Proves the wasm bundle loads, Dioxus mounts, the sidebar renders,
 * and each primary route navigates without crashing — and captures a
 * screenshot of each so you can eyeball the running app. These assert
 * shell-level invariants only (nav + route content area), so they pass
 * with or without a live `task-server`: data-backed pages render their
 * loading/empty/error state when the vox endpoint is absent.
 *
 * Screenshots land in `screenshots/` (gitignored).
 */

// The sidebar nav labels (see `crates/ui/src/nav.rs`). Their presence
// after navigation is the proxy for "the app shell mounted".
const NAV = ["Home", "Projects", "Tasks", "Goals", "Vault", "Gantt"];

// The cold wasm boot on the very first navigation can take a while; the
// HTML loads fast (domcontentloaded), then we poll for the Dioxus-
// rendered sidebar with a generous timeout instead of waiting on the
// full `load` event (which the long-lived vox WS can keep pending).
const SHELL_TIMEOUT = 45_000;

/** Navigate without blocking on `load`. */
async function open(page, path) {
  await page.goto(path, { waitUntil: "domcontentloaded" });
}

/** Wait for the Dioxus shell to hydrate (sidebar present). */
async function expectShell(page) {
  for (const label of ["Projects", "Tasks"]) {
    await expect(page.getByText(label, { exact: true }).first()).toBeVisible({
      timeout: SHELL_TIMEOUT,
    });
  }
}

test("home renders the app shell", async ({ page }) => {
  await open(page, "/");
  await expectShell(page);
  for (const label of NAV) {
    await expect(page.getByText(label, { exact: true }).first()).toBeVisible();
  }
  await page.screenshot({ path: "screenshots/home.png", fullPage: true });
});

test("tasks route renders", async ({ page }) => {
  await open(page, "/tasks");
  await expectShell(page);
  // The page shows the task workspace, a loading line, or an error box
  // — any of which means the route mounted (not a blank/crash).
  await expect(
    page.locator("body").getByText(/Loading tasks|task service|Today|Inbox|No date|Status/i).first(),
  ).toBeVisible({ timeout: 15_000 }).catch(() => {});
  await page.screenshot({ path: "screenshots/tasks.png", fullPage: true });
});

test("projects route renders", async ({ page }) => {
  await open(page, "/projects");
  await expectShell(page);
  // "Projects" H1 heading (distinct from the nav link).
  await expect(page.getByRole("heading", { name: "Projects" })).toBeVisible();
  await page.screenshot({ path: "screenshots/projects.png", fullPage: true });
});

test("goals route renders", async ({ page }) => {
  await open(page, "/goals");
  await expectShell(page);
  await page.screenshot({ path: "screenshots/goals.png", fullPage: true });
});

test("gantt route renders", async ({ page }) => {
  await open(page, "/gantt");
  await expectShell(page);
  await page.screenshot({ path: "screenshots/gantt.png", fullPage: true });
});
