// @ts-check
const { test, expect } = require("@playwright/test");

/**
 * Phase 6.5b — Properties pane.
 *
 * The /knowledge route renders the typed-property editor pane at
 * the top of the page body whenever the selected page carries a
 * `kind:` frontmatter that matches a registered schema. Editing a
 * property writes through `PageRepo::update` to the local CRDT
 * doc, which propagates to peers via `WorkspaceSync`.
 */

test("properties pane appears for a `kind: task` page", async ({ page }) => {
  await page.goto("/knowledge");
  await expect(page.locator("#knowledge-route")).toBeVisible();
  await expect(
    page.locator("[data-testid='knowledge-version-badge']"),
  ).toContainText(/^v[1-9]/);

  // Add a page from the Knowledge sidebar — but the default page
  // has `kind` missing. To trigger the schema render, mint a
  // `kind: task` page through the tasks-kanban route's add-task
  // input (same vault).
  await page.goto("/tasks-kanban");
  const title = `Pane-${Date.now()}`;
  await page
    .locator("[data-testid='kanban-add-input-todo']")
    .fill(title);
  await page.locator("[data-testid='kanban-add-input-todo']").press("Enter");
  // Wait for the card to register.
  await expect(
    page
      .locator("[data-testid='kanban-column-todo']")
      .getByText(title, { exact: true }),
  ).toBeVisible();

  // Back to /knowledge — pick the new task page.
  await page.goto("/knowledge");
  await expect(
    page.locator("[data-testid='knowledge-version-badge']"),
  ).toContainText(/^v[1-9]/);
  const pageRow = page
    .locator("[data-testid='knowledge-page-list']")
    .getByText(title, { exact: true });
  await expect(pageRow).toBeVisible();
  await pageRow.click();

  // Properties pane up. data-kind should be `task` because the
  // page's frontmatter has `kind: task`.
  const pane = page.locator("[data-testid='properties-pane']");
  await expect(pane).toBeVisible();
  await expect(pane).toHaveAttribute("data-kind", "task");

  // Status is the schema's declared EnumWithMetadata. The editor
  // is a <select> with the four declared options.
  const statusEditor = pane.locator("[data-testid='prop-editor-status']");
  await expect(statusEditor).toBeVisible();
  // It carries the current value `todo`.
  await expect(statusEditor).toHaveValue("todo");

  // Priority editor is similar.
  await expect(pane.locator("[data-testid='prop-editor-priority']")).toBeVisible();

  // Tags is a chip list (data-testid is the wrapping div).
  await expect(pane.locator("[data-testid='prop-editor-tags']")).toBeVisible();

  // LexoRank rows are hidden (sort_order has no editor).
  await expect(pane.locator("[data-testid='prop-editor-sort_order']")).toHaveCount(0);
});

test("editing the status property updates the kanban column on the other tab", async ({
  browser,
}) => {
  // Two tabs: tab A on /knowledge, tab B on /tasks-kanban. Edit
  // the status property on tab A, observe the card move on tab B.
  const ctxA = await browser.newContext();
  const ctxB = await browser.newContext();
  const a = await ctxA.newPage();
  const b = await ctxB.newPage();
  try {
    // Seed a task via the kanban route on tab A.
    await a.goto("/tasks-kanban");
    const title = `PaneEdit-${Date.now()}`;
    await a
      .locator("[data-testid='kanban-add-input-todo']")
      .fill(title);
    await a.locator("[data-testid='kanban-add-input-todo']").press("Enter");
    await expect(
      a
        .locator("[data-testid='kanban-column-todo']")
        .getByText(title, { exact: true }),
    ).toBeVisible();

    // Tab B watches the kanban.
    await b.goto("/tasks-kanban");
    await expect(
      b
        .locator("[data-testid='kanban-column-todo']")
        .getByText(title, { exact: true }),
    ).toBeVisible();

    // Tab A navigates to /knowledge and edits the status field
    // via the properties pane.
    await a.goto("/knowledge");
    await expect(
      a.locator("[data-testid='knowledge-version-badge']"),
    ).toContainText(/^v[1-9]/);
    const pageRow = a
      .locator("[data-testid='knowledge-page-list']")
      .getByText(title, { exact: true });
    await expect(pageRow).toBeVisible();
    await pageRow.click();
    const statusEditor = a.locator("[data-testid='prop-editor-status']");
    await expect(statusEditor).toBeVisible();
    await statusEditor.selectOption("in_progress");

    // Tab B's kanban now shows the card in `in_progress`.
    await expect(
      b
        .locator("[data-testid='kanban-column-in_progress']")
        .getByText(title, { exact: true }),
    ).toBeVisible();
    await expect(
      b
        .locator("[data-testid='kanban-column-todo']")
        .getByText(title, { exact: true }),
    ).toHaveCount(0);
  } finally {
    await ctxA.close();
    await ctxB.close();
  }
});
