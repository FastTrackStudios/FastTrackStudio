// @ts-check
const { test, expect } = require("@playwright/test");

/**
 * Phase 8 — /servers route smoke.
 *
 * Verifies the registry's CRUD UI: form renders, adding a server
 * (no credentials → token=None path) inserts a row, removing it
 * empties the list. Persistence is exercised by reloading the
 * page and asserting the row is still there.
 *
 * The two-server fan-out is covered by the native
 * `tests/federation.rs` integration test — Phase 8 deliberately
 * defers a browser-side two-server boot to keep playwright.config
 * simple (a follow-up commit can grow webServer into an array
 * with a second TASK_SERVER_BIND).
 */

test("servers route renders + add/remove round trip", async ({ page }) => {
  await page.goto("/servers");
  await expect(page.locator("#servers-route")).toBeVisible();
  // List starts empty (modulo any leftover localStorage rows;
  // playwright opens a fresh context per test).
  const list = page.locator("[data-testid='servers-list']");
  await expect(list).toBeVisible();

  // Add a row. We don't sign in (email/password blank) so the
  // sign_in call short-circuits to Ok(None).
  await page
    .locator("[data-testid='servers-input-label']")
    .fill("local-dev");
  await page
    .locator("[data-testid='servers-input-url']")
    .fill("ws://127.0.0.1:9090/vox");
  await page
    .locator("[data-testid='servers-add-button'] button")
    .click();

  // Row appears.
  const row = page.locator("[data-testid^='server-row-']");
  await expect(row).toHaveCount(1);
  await expect(row).toHaveAttribute("data-signed-in", "false");
  await expect(row).toContainText("local-dev");
  await expect(row).toContainText("ws://127.0.0.1:9090/vox");

  // Reload — localStorage round-trips the entry.
  await page.reload();
  const rowAfterReload = page.locator("[data-testid^='server-row-']");
  await expect(rowAfterReload).toHaveCount(1);
  await expect(rowAfterReload).toContainText("local-dev");

  // Remove. Pick the remove button by its testid prefix.
  await page
    .locator("[data-testid^='servers-remove-']")
    .first()
    .locator("button")
    .click();
  await expect(page.locator("[data-testid^='server-row-']")).toHaveCount(0);
});

test("federated-tasks route renders empty state when no servers", async ({ page }) => {
  await page.goto("/federated-tasks");
  await expect(page.locator("#federated-tasks-route")).toBeVisible();
  await expect(
    page.getByText("No servers registered. Go to /servers to add one."),
  ).toBeVisible();
});

test("federated view picks up a registered local server", async ({ page }) => {
  // The playwright webServer config has task-server seeded with
  // 10 projects + 80 tasks on ws://127.0.0.1:9090/vox. Register
  // it via /servers, then visit /federated-tasks and verify at
  // least one task row renders.
  await page.goto("/servers");
  await page
    .locator("[data-testid='servers-input-label']")
    .fill("seed");
  await page
    .locator("[data-testid='servers-input-url']")
    .fill("ws://127.0.0.1:9090/vox");
  await page
    .locator("[data-testid='servers-add-button'] button")
    .click();
  await expect(page.locator("[data-testid^='server-row-']")).toHaveCount(1);

  await page.goto("/federated-tasks");
  await expect(page.locator("#federated-tasks-route")).toBeVisible();
  // First task row eventually appears once the snapshot fetch
  // resolves. Generous timeout for the WS round-trip.
  await expect(
    page.locator("[data-testid^='federated-task-']").first(),
  ).toBeVisible({ timeout: 8000 });
});
