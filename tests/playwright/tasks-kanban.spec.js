// @ts-check
const { test, expect } = require("@playwright/test");

/**
 * Phase 6 — Tasks Kanban demo.
 *
 * The /tasks-kanban route runs a hand-coded ParsedBase
 * (`kind=task`, group by `status`) over the org vault and renders
 * the result via `KindKanban`. Three baseline columns always
 * exist: `todo`, `in_progress`, `done`.
 *
 * The goal-doc Phase 6 test calls for "drag a card across columns".
 * Faking HTML5 DnD in playwright is brittle, so this spec uses the
 * per-card "Move to X" button (same code path — the move triggers a
 * frontmatter update). HTML5 DnD as a UX enhancement can land later.
 */

test("kanban route renders three baseline columns", async ({ page }) => {
  await page.goto("/tasks-kanban");
  await expect(page.locator("#tasks-kanban-route")).toBeVisible();
  await expect(
    page.locator("[data-testid='tasks-kanban-version-badge']"),
  ).toContainText(/^v[1-9]/);
  await expect(page.locator("[data-testid='kind-kanban']")).toBeVisible();
  await expect(page.locator("[data-testid='kanban-column-todo']")).toBeVisible();
  await expect(
    page.locator("[data-testid='kanban-column-in_progress']"),
  ).toBeVisible();
  await expect(page.locator("[data-testid='kanban-column-done']")).toBeVisible();
});

test("adding a task lands in the todo column", async ({ page }) => {
  await page.goto("/tasks-kanban");
  await expect(
    page.locator("[data-testid='tasks-kanban-version-badge']"),
  ).toContainText(/^v[1-9]/);

  const title = `Task-${Date.now()}`;
  await page
    .locator("[data-testid='tasks-kanban-new-task-input']")
    .fill(title);
  await page
    .locator("[data-testid='tasks-kanban-add-button'] button")
    .click();

  // Card appears in the todo column.
  const todoCol = page.locator("[data-testid='kanban-column-todo']");
  await expect(todoCol.getByText(title, { exact: true })).toBeVisible();
});

test("HTML5 drag-and-drop moves a card across columns", async ({ page }) => {
  // Phase 6.5b — exercise the dragstart/drop listeners on
  // `KanbanCard` and `KanbanColumn`. Move-buttons stay as the
  // fallback / accessibility path; this spec uses real DnD.
  await page.goto("/tasks-kanban");
  await expect(
    page.locator("[data-testid='tasks-kanban-version-badge']"),
  ).toContainText(/^v[1-9]/);

  const title = `DnD-${Date.now()}`;
  await page
    .locator("[data-testid='tasks-kanban-new-task-input']")
    .fill(title);
  await page
    .locator("[data-testid='tasks-kanban-add-button'] button")
    .click();

  const card = page
    .locator("[data-testid^='kanban-card-']")
    .filter({ hasText: title });
  await expect(card).toBeVisible();
  await expect(card).toHaveAttribute("draggable", "true");
  await expect(card).toHaveAttribute("data-bucket", "todo");

  // Drag the card into the `done` column.
  const doneCol = page.locator("[data-testid='kanban-column-done']");
  await card.dragTo(doneCol);

  // Card is now in done.
  await expect(doneCol.getByText(title, { exact: true })).toBeVisible();
  await expect(
    page
      .locator("[data-testid='kanban-column-todo']")
      .getByText(title, { exact: true }),
  ).toHaveCount(0);
});

test("two tabs sync kanban: card moves across columns", async ({ browser }) => {
  const ctxA = await browser.newContext();
  const ctxB = await browser.newContext();
  const a = await ctxA.newPage();
  const b = await ctxB.newPage();
  try {
    await a.goto("/tasks-kanban");
    await b.goto("/tasks-kanban");
    await expect(
      a.locator("[data-testid='tasks-kanban-version-badge']"),
    ).toContainText(/^v[1-9]/);
    await expect(
      b.locator("[data-testid='tasks-kanban-version-badge']"),
    ).toContainText(/^v[1-9]/);

    // Tab A creates a task.
    const title = `Mover-${Date.now()}`;
    await a
      .locator("[data-testid='tasks-kanban-new-task-input']")
      .fill(title);
    await a
      .locator("[data-testid='tasks-kanban-add-button'] button")
      .click();

    // The card lands in todo on both tabs.
    const todoA = a.locator("[data-testid='kanban-column-todo']");
    const todoB = b.locator("[data-testid='kanban-column-todo']");
    await expect(todoA.getByText(title, { exact: true })).toBeVisible();
    await expect(todoB.getByText(title, { exact: true })).toBeVisible();

    // Tab A drags the card from todo → in_progress. The kanban
    // is drag-only — no buttons.
    const cardA = a
      .locator("[data-testid^='kanban-card-']")
      .filter({ hasText: title });
    await expect(cardA).toBeVisible();
    const ipColA = a.locator("[data-testid='kanban-column-in_progress']");
    await cardA.dragTo(ipColA);

    // Card is now in the in_progress column in both tabs.
    const ipA = a.locator("[data-testid='kanban-column-in_progress']");
    const ipB = b.locator("[data-testid='kanban-column-in_progress']");
    await expect(ipA.getByText(title, { exact: true })).toBeVisible();
    await expect(ipB.getByText(title, { exact: true })).toBeVisible();

    // And gone from the todo column.
    await expect(todoA.getByText(title, { exact: true })).toHaveCount(0);
    await expect(todoB.getByText(title, { exact: true })).toHaveCount(0);
  } finally {
    await ctxA.close();
    await ctxB.close();
  }
});
