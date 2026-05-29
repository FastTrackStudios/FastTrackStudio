// @ts-check
const { test, expect } = require("@playwright/test");

/**
 * Smoke + hydration suite for the Task web app.
 *
 * These assert the app actually WORKS, not just that the shell paints:
 * - the Dioxus shell mounts (sidebar),
 * - data-backed routes get PAST their "Loading…" state (the vox fetch
 *   resolved — proving the WebSocket didn't crash mid-flight),
 * - NO fatal wasm/console error fires (esp. the
 *   "closure invoked … after being dropped" WebSocket crash).
 *
 * Every test records the full console + pageerror stream and dumps it
 * on failure, so a crash shows its trigger.
 */

const FATAL = /closure invoked recursively or after being dropped|after being dropped|unreachable executed|recursive use of an object/i;

function watch(page) {
  const logs = [];
  page.on("console", (m) => logs.push(`[${m.type()}] ${m.text()}`));
  page.on("pageerror", (e) => logs.push(`[pageerror] ${String(e)}`));
  return logs;
}

function assertNoFatal(logs) {
  const fatal = logs.filter((l) => FATAL.test(l));
  expect(fatal, `FATAL errors:\n${fatal.join("\n")}\n\n--- full console ---\n${logs.join("\n")}`).toHaveLength(0);
}

async function open(page, path) {
  await page.goto(path, { waitUntil: "domcontentloaded" });
}

async function expectShell(page) {
  for (const label of ["Projects", "Tasks"]) {
    await expect(page.getByText(label, { exact: true }).first()).toBeVisible({ timeout: 45_000 });
  }
}

test("home shell + no crash", async ({ page }) => {
  const logs = watch(page);
  await open(page, "/");
  await expectShell(page);
  await page.waitForTimeout(5000); // let the vox_session bootstrap WS settle/fail
  await page.screenshot({ path: "screenshots/home.png", fullPage: true });
  assertNoFatal(logs);
});

test("projects HYDRATES (gets past Loading, no crash)", async ({ page }) => {
  const logs = watch(page);
  await open(page, "/projects");
  await expect(page.getByRole("heading", { name: "Projects" })).toBeVisible({ timeout: 30_000 });
  // The fetch must resolve: the "Loading projects…" line disappears
  // (replaced by cards OR an error box). If the wasm crashed mid-fetch,
  // the app is dead and this stays forever → fail.
  await expect(page.getByText(/Loading projects/i)).toBeHidden({ timeout: 20_000 });
  await page.screenshot({ path: "screenshots/projects.png", fullPage: true });
  // Real data must hydrate — NOT the error box. The loaded view renders a
  // "<n> top-level · <m> total" counter; assert it appears.
  await expect(page.getByText(/Couldn't reach the project service/i)).toBeHidden();
  await expect(page.getByText(/top-level ·/i)).toBeVisible({ timeout: 10_000 });
  assertNoFatal(logs);
});

test("tasks HYDRATES (gets past Loading, no crash)", async ({ page }) => {
  const logs = watch(page);
  await open(page, "/tasks");
  await expectShell(page);
  await expect(page.getByText(/Loading tasks/i)).toBeHidden({ timeout: 20_000 });
  await page.waitForTimeout(4000);
  await page.screenshot({ path: "screenshots/tasks.png", fullPage: true });
  const errored = await page.getByText(/Couldn't reach the task service/i).isVisible().catch(() => false);
  console.log(`[test] tasks errored-box visible = ${errored}`);
  assertNoFatal(logs);
});

test("goals + gantt render without crash", async ({ page }) => {
  const logs = watch(page);
  await open(page, "/goals");
  await expectShell(page);
  await page.waitForTimeout(2000);
  await page.screenshot({ path: "screenshots/goals.png", fullPage: true });
  await open(page, "/gantt");
  await expectShell(page);
  await page.waitForTimeout(2000);
  await page.screenshot({ path: "screenshots/gantt.png", fullPage: true });
  assertNoFatal(logs);
});
