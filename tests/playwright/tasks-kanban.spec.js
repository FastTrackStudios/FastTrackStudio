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

test("adding a task via the todo column input lands in todo", async ({ page }) => {
  await page.goto("/tasks-kanban");
  await expect(
    page.locator("[data-testid='tasks-kanban-version-badge']"),
  ).toContainText(/^v[1-9]/);

  const title = `Task-${Date.now()}`;
  await page
    .locator("[data-testid='kanban-add-input-todo']")
    .fill(title);
  await page.locator("[data-testid='kanban-add-input-todo']").press("Enter");

  // Card appears in the todo column.
  const todoCol = page.locator("[data-testid='kanban-column-todo']");
  await expect(todoCol.getByText(title, { exact: true })).toBeVisible();
});

test("adding a task via the done column input lands directly in done", async ({ page }) => {
  await page.goto("/tasks-kanban");
  const title = `Direct-done-${Date.now()}`;
  await page
    .locator("[data-testid='kanban-add-input-done']")
    .fill(title);
  await page.locator("[data-testid='kanban-add-input-done']").press("Enter");
  const doneCol = page.locator("[data-testid='kanban-column-done']");
  await expect(doneCol.getByText(title, { exact: true })).toBeVisible();
  // …and is NOT in the todo column.
  await expect(
    page.locator("[data-testid='kanban-column-todo']").getByText(title, { exact: true }),
  ).toHaveCount(0);
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
    .locator("[data-testid='kanban-add-input-todo']")
    .fill(title);
  await page.locator("[data-testid='kanban-add-input-todo']").press("Enter");

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

test("drag-over column highlight + drop indicator render", async ({ page }) => {
  // Verifies the dnd-kit-style visual feedback: the destination
  // column gets `data-drag-over=true` while a card is hovering,
  // and the card the dragged item is about to land before gets
  // `data-drop-target=true`.
  await page.goto("/tasks-kanban");
  // Need at least one card to drag.
  const title = `Hover-${Date.now()}`;
  await page
    .locator("[data-testid='kanban-add-input-todo']")
    .fill(title);
  await page.locator("[data-testid='kanban-add-input-todo']").press("Enter");
  const sourceCard = page
    .locator("[data-testid^='kanban-card-']")
    .filter({ hasText: title });
  await expect(sourceCard).toBeVisible();

  // Seed a card in the done column to use as a drop target.
  const targetTitle = `Target-${Date.now()}`;
  await page
    .locator("[data-testid='kanban-add-input-done']")
    .fill(targetTitle);
  await page.locator("[data-testid='kanban-add-input-done']").press("Enter");
  const targetCard = page
    .locator("[data-testid^='kanban-card-']")
    .filter({ hasText: targetTitle });
  await expect(targetCard).toBeVisible();

  // Simulate a drag hover using playwright's manual mouse —
  // dragTo's all-in-one move doesn't give us a chance to assert
  // mid-drag state, so we step through it.
  const sourceBox = await sourceCard.boundingBox();
  const targetBox = await targetCard.boundingBox();
  if (!sourceBox || !targetBox) throw new Error("bounding box missing");
  await page.mouse.move(sourceBox.x + sourceBox.width / 2, sourceBox.y + sourceBox.height / 2);
  await page.mouse.down();
  await page.mouse.move(
    targetBox.x + targetBox.width / 2,
    targetBox.y + targetBox.height / 2,
    { steps: 10 },
  );

  // The done column should now report drag-over.
  await expect(
    page.locator("[data-testid='kanban-column-done']"),
  ).toHaveAttribute("data-drag-over", "true");
  // The target card should report drop-target.
  await expect(targetCard).toHaveAttribute("data-drop-target", "true");

  await page.mouse.up();
  // After release, the source landed in done (just before the
  // target card).
  await expect(
    page.locator("[data-testid='kanban-column-done']").getByText(title, { exact: true }),
  ).toBeVisible();
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

    // Tab A creates a task via the todo-column input.
    const title = `Mover-${Date.now()}`;
    await a
      .locator("[data-testid='kanban-add-input-todo']")
      .fill(title);
    await a.locator("[data-testid='kanban-add-input-todo']").press("Enter");

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
