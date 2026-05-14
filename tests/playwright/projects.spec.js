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
 * Filter tabs at the top scope the visible task list. Switching
 * to "Done" should hide every active row.
 */
test("filter tabs scope the visible tasks", async ({ page }) => {
  await page.goto("/projects");
  await expect(
    page.locator("[data-testid='version-badge']"),
  ).toContainText(/^v[1-9]/);
  const tabs = page.locator("[data-testid='tasks-filter-tabs']");
  await expect(tabs).toBeVisible();

  // Snapshot a row's `data-task-done` so we know what to look
  // for after the filter change.
  const allRows = page.locator("[data-testid^='task-row-']");
  await expect(allRows.first()).toBeVisible();

  // Switch to Done tab. Every remaining row must have
  // data-task-done="true".
  await tabs.getByText(/Done/).click();
  await expect(allRows.first()).toBeVisible();
  const doneStatuses = await allRows.evaluateAll((nodes) =>
    nodes.map((n) => n.getAttribute("data-task-done")),
  );
  expect(doneStatuses.every((s) => s === "true")).toBeTruthy();

  // Switch to Active. Every remaining row must have
  // data-task-done="false".
  await tabs.getByText(/Active/).click();
  await expect(allRows.first()).toBeVisible();
  const activeStatuses = await allRows.evaluateAll((nodes) =>
    nodes.map((n) => n.getAttribute("data-task-done")),
  );
  expect(activeStatuses.every((s) => s === "false")).toBeTruthy();
});

/**
 * The per-project inline add input creates a new task linked to
 * the matching project.
 */
test("inline add input creates a task in the right project", async ({ page }) => {
  await page.goto("/projects");
  const project = "Website Redesign";
  const title = `Inline-${Date.now()}`;
  const input = page.locator(`[data-testid='add-task-input-${project}']`);
  await expect(input).toBeVisible();
  await input.fill(title);
  await input.press("Enter");
  // Task appears under the same project card.
  const projectCard = page.locator(`[data-testid='project-card-${project}']`);
  await expect(
    projectCard.locator("[data-testid^='task-row-']").filter({ hasText: title }),
  ).toBeVisible();
});

/**
 * Phase post-10 — inline expand opens a properties pane for the
 * task page, the same component `/knowledge` uses. Editing the
 * status dropdown writes to the page's frontmatter and triggers
 * the row's `data-task-done` to flip on the next render.
 */
test("inline expand reveals properties pane + status edit persists", async ({ page }) => {
  await page.goto("/projects");
  await expect(
    page.locator("[data-testid='version-badge']"),
  ).toContainText(/^v[1-9]/);

  const firstRow = page.locator("[data-testid^='task-row-']").first();
  await expect(firstRow).toBeVisible();
  // Pull the page_id out of the row's testid.
  const rowTestId = await firstRow.getAttribute("data-testid");
  if (!rowTestId) throw new Error("no row testid");
  const pageId = rowTestId.replace("task-row-", "");

  // Open the inline expansion.
  await page
    .locator(`[data-testid='task-expand-${pageId}'] button`)
    .click();
  const pane = page.locator(`[data-testid='task-properties-${pageId}']`);
  await expect(pane).toBeVisible();
  // Status editor is the same EnumWithMetadata <select> the
  // /knowledge properties pane uses.
  const statusEditor = pane.locator("[data-testid='prop-editor-status']");
  await expect(statusEditor).toBeVisible();

  // Switching the status to `done` flips the row's data-task-done.
  await statusEditor.selectOption("done");
  await expect(firstRow).toHaveAttribute("data-task-done", "true");

  // Reverse: switch to `todo` clears it.
  await statusEditor.selectOption("todo");
  await expect(firstRow).toHaveAttribute("data-task-done", "false");
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
