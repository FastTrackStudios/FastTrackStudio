// @ts-check
const { test, expect } = require("@playwright/test");

/**
 * Realtime stress: 20 rapid toggles in tab A, tab B has to land
 * on the FINAL state. Catches dropped uploads, mpsc back-pressure
 * issues, and "the version bumped but use_resource didn't rerun".
 */
test("rapid burst in tab A, tab B converges on final state", async ({ browser }) => {
  test.setTimeout(60_000);
  const ctxA = await browser.newContext();
  const ctxB = await browser.newContext();
  const a = await ctxA.newPage();
  const b = await ctxB.newPage();
  try {
    await a.goto("/projects");
    await b.goto("/projects");

    const rowA = a.locator("[data-testid^='task-row-']").first();
    await expect(rowA).toBeVisible();
    const taskTestId = await rowA.getAttribute("data-testid");
    if (!taskTestId) throw new Error("no data-testid on first row");
    const rowB = b.locator(`[data-testid='${taskTestId}']`);
    await expect(rowB).toBeVisible();

    const initial = await rowA.getAttribute("data-task-done");
    // 20 clicks → 20 toggles → final state = initial XOR (20 % 2 == 0)
    // → same as initial. Click 21 times to land on the opposite.
    const clicks = 21;
    const expected = initial === "true" ? "false" : "true";

    const checkboxA = rowA.locator("input[type='checkbox']");
    for (let i = 0; i < clicks; i++) {
      await checkboxA.click({ force: true });
    }

    // A's local state converges first.
    await expect(rowA).toHaveAttribute("data-task-done", expected);
    // B catches up via WorkspaceSync. Generous timeout — the
    // server has to import 21 chunks, broadcast each, and B's
    // sub stream has to deliver + import each.
    await expect(rowB).toHaveAttribute("data-task-done", expected, {
      timeout: 15_000,
    });
  } finally {
    await ctxA.close();
    await ctxB.close();
  }
});

/**
 * Bidirectional concurrent edits across two tabs.
 * Each tab toggles a DIFFERENT task simultaneously. After the
 * burst, both tabs must show both toggles applied.
 */
test("two tabs edit different tasks concurrently, both converge", async ({
  browser,
}) => {
  test.setTimeout(60_000);
  const ctxA = await browser.newContext();
  const ctxB = await browser.newContext();
  const a = await ctxA.newPage();
  const b = await ctxB.newPage();
  try {
    await a.goto("/projects");
    await b.goto("/projects");
    await expect(a.locator("[data-testid^='task-row-']").first()).toBeVisible();
    await expect(b.locator("[data-testid^='task-row-']").first()).toBeVisible();

    // Pick two different rows that both tabs see.
    const allRows = await a.locator("[data-testid^='task-row-']").all();
    if (allRows.length < 2)
      throw new Error("need at least 2 task rows to run this test");
    const idA = await allRows[0].getAttribute("data-testid");
    const idB = await allRows[1].getAttribute("data-testid");
    if (!idA || !idB) throw new Error("rows missing testid");

    const rowAinA = a.locator(`[data-testid='${idA}']`);
    const rowBinA = a.locator(`[data-testid='${idB}']`);
    const rowAinB = b.locator(`[data-testid='${idA}']`);
    const rowBinB = b.locator(`[data-testid='${idB}']`);

    const initialA = await rowAinA.getAttribute("data-task-done");
    const initialB = await rowBinB.getAttribute("data-task-done");
    const targetA = initialA === "true" ? "false" : "true";
    const targetB = initialB === "true" ? "false" : "true";

    // Fire both clicks "simultaneously" — playwright awaits both
    // in parallel, so the actual UI interactions land within
    // milliseconds of each other.
    await Promise.all([
      rowAinA.locator("input[type='checkbox']").click({ force: true }),
      rowBinB.locator("input[type='checkbox']").click({ force: true }),
    ]);

    // Each tab's own edit lands locally.
    await expect(rowAinA).toHaveAttribute("data-task-done", targetA);
    await expect(rowBinB).toHaveAttribute("data-task-done", targetB);
    // Cross-tab convergence: A sees B's edit on rowB, and
    // vice versa.
    await expect(rowBinA).toHaveAttribute("data-task-done", targetB, {
      timeout: 10_000,
    });
    await expect(rowAinB).toHaveAttribute("data-task-done", targetA, {
      timeout: 10_000,
    });
  } finally {
    await ctxA.close();
    await ctxB.close();
  }
});

/**
 * Five tabs, each toggles a different task in parallel. Every
 * tab has to see every other tab's edit. Hotter test of the
 * fanout — each apply_update has to reach 4 other subscribers
 * and re-render their UIs within the timeout.
 */
test("five tabs each toggle a different task, all converge", async ({ browser }) => {
  test.setTimeout(90_000);
  const N = 5;
  const ctxs = await Promise.all(
    Array.from({ length: N }, () => browser.newContext()),
  );
  const pages = await Promise.all(ctxs.map((c) => c.newPage()));
  try {
    await Promise.all(pages.map((p) => p.goto("/projects")));
    await Promise.all(
      pages.map((p) =>
        expect(p.locator("[data-testid^='task-row-']").first()).toBeVisible(),
      ),
    );

    // Pick N distinct task ids visible to all pages.
    const ids = (
      await pages[0].locator("[data-testid^='task-row-']").all()
    ).slice(0, N);
    const taskTestIds = await Promise.all(
      ids.map((row) => row.getAttribute("data-testid")),
    );
    for (const id of taskTestIds) if (!id) throw new Error("missing testid");

    // Capture each row's initial done state from page 0 (any
    // page would do — they all started from the same snapshot).
    const initials = await Promise.all(
      taskTestIds.map((id) =>
        pages[0].locator(`[data-testid='${id}']`).getAttribute("data-task-done"),
      ),
    );
    const targets = initials.map((v) => (v === "true" ? "false" : "true"));

    // Each page clicks ITS OWN row's checkbox in parallel.
    await Promise.all(
      pages.map((p, i) =>
        p
          .locator(`[data-testid='${taskTestIds[i]}']`)
          .locator("input[type='checkbox']")
          .click({ force: true }),
      ),
    );

    // Every page must show every task's target state.
    for (let pageIdx = 0; pageIdx < N; pageIdx++) {
      for (let taskIdx = 0; taskIdx < N; taskIdx++) {
        await expect(
          pages[pageIdx].locator(`[data-testid='${taskTestIds[taskIdx]}']`),
          `page ${pageIdx} must see task ${taskIdx} → ${targets[taskIdx]}`,
        ).toHaveAttribute("data-task-done", targets[taskIdx], {
          timeout: 15_000,
        });
      }
    }
  } finally {
    await Promise.all(ctxs.map((c) => c.close()));
  }
});
