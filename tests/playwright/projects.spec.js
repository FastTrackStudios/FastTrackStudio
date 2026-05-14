// @ts-check
const { test, expect } = require("@playwright/test");

/**
 * Smoke: the /projects route loads, the workspace doc syncs, and
 * at least one task row renders with the testid we expect.
 */
test("projects route loads + tasks render", async ({ page }) => {
  await page.goto("/projects");
  // The route's outer div has `id="projects-route"`.
  await expect(page.locator("#projects-route")).toBeVisible();
  // Version badge is `v0` until the snapshot lands; after import
  // it bumps to v1+.
  await expect(page.locator("[data-testid='version-badge']")).toContainText(
    /^v[1-9]/,
  );
  // At least one task row.
  await expect(page.locator("[data-testid^='task-row-']").first()).toBeVisible();
});

/**
 * Local mutation round-trips through the DOM:
 *   click the checkbox → the row's `data-task-done` flips.
 * This is the single-tab read-after-write — proves the local
 * CrdtDoc + use_resource re-render path works end to end.
 */
test("toggle checkbox flips data-task-done locally", async ({ page }) => {
  await page.goto("/projects");
  const firstRow = page.locator("[data-testid^='task-row-']").first();
  await expect(firstRow).toBeVisible();

  const initial = await firstRow.getAttribute("data-task-done");
  const checkbox = firstRow.locator("input[type='checkbox']");
  await checkbox.click();
  await expect(firstRow).toHaveAttribute(
    "data-task-done",
    initial === "true" ? "false" : "true",
  );
});

/**
 * Realtime sync across two browser contexts.
 *
 * Each browser context gets its own cookies + storage and behaves
 * like a separate user — separate WebSocket sessions to the
 * server, separate local CrdtDocs. Toggle in tab A, observe the
 * change in tab B without reloading.
 */
test("two tabs sync via WorkspaceSync", async ({ browser }) => {
  const ctxA = await browser.newContext();
  const ctxB = await browser.newContext();
  const a = await ctxA.newPage();
  const b = await ctxB.newPage();
  try {
    await a.goto("/projects");
    await b.goto("/projects");

    // Both tabs render the snapshot.
    await expect(a.locator("[data-testid^='task-row-']").first()).toBeVisible();
    await expect(b.locator("[data-testid^='task-row-']").first()).toBeVisible();

    // Pick a row that exists in both. The seeded task ids are
    // stable across the two snapshots since they come from the
    // same server doc.
    const rowA = a.locator("[data-testid^='task-row-']").first();
    const taskTestId = await rowA.getAttribute("data-testid");
    if (!taskTestId) throw new Error("expected a data-testid on the first row");
    const rowB = b.locator(`[data-testid='${taskTestId}']`);

    const initial = await rowA.getAttribute("data-task-done");
    const target = initial === "true" ? "false" : "true";

    // Toggle in A.
    await rowA.locator("input[type='checkbox']").click();

    // A's local state flips immediately…
    await expect(rowA).toHaveAttribute("data-task-done", target);
    // …and B catches up via the WorkspaceSync push.
    await expect(rowB).toHaveAttribute("data-task-done", target);
  } finally {
    await ctxA.close();
    await ctxB.close();
  }
});
