// @ts-check
const { test, expect } = require("@playwright/test");

/**
 * Phase 5c — the /knowledge route loads, the `vault/org` doc
 * syncs, and the seeded "Org" vault renders.
 */
test("knowledge route loads + vault snapshot lands", async ({ page }) => {
  await page.goto("/knowledge");
  await expect(page.locator("#knowledge-route")).toBeVisible();
  // Version bumps to v1+ once the snapshot arrives.
  await expect(
    page.locator("[data-testid='knowledge-version-badge']"),
  ).toContainText(/^v[1-9]/);
  // Page list section is up. Empty vault: no rows yet.
  await expect(page.locator("[data-testid='knowledge-page-list']")).toBeVisible();
});

/**
 * Local mutation: create a page from the UI, see it appear in the
 * local list.
 */
test("create page locally — appears in list", async ({ page }) => {
  await page.goto("/knowledge");
  await expect(page.locator("#knowledge-route")).toBeVisible();
  await expect(
    page.locator("[data-testid='knowledge-version-badge']"),
  ).toContainText(/^v[1-9]/);

  const basename = `Solo-${Date.now()}`;
  await page
    .locator("[data-testid='knowledge-new-page-input']")
    .fill(basename);
  await page
    .locator("[data-testid='knowledge-add-page-button'] button")
    .click();

  // The new page row eventually appears in the sidebar.
  await expect(
    page
      .locator("[data-testid='knowledge-page-list']")
      .getByText(basename, { exact: true }),
  ).toBeVisible();
});

/**
 * Two-tab realtime: create a page in A, see it in B without reload.
 */
test("two tabs sync page creation via vault/org", async ({ browser }) => {
  const ctxA = await browser.newContext();
  const ctxB = await browser.newContext();
  const a = await ctxA.newPage();
  const b = await ctxB.newPage();
  try {
    await a.goto("/knowledge");
    await b.goto("/knowledge");

    await expect(a.locator("#knowledge-route")).toBeVisible();
    await expect(b.locator("#knowledge-route")).toBeVisible();
    await expect(
      a.locator("[data-testid='knowledge-version-badge']"),
    ).toContainText(/^v[1-9]/);
    await expect(
      b.locator("[data-testid='knowledge-version-badge']"),
    ).toContainText(/^v[1-9]/);

    const basename = `Sync-${Date.now()}`;
    await a
      .locator("[data-testid='knowledge-new-page-input']")
      .fill(basename);
    await a
      .locator("[data-testid='knowledge-add-page-button'] button")
      .click();

    // B sees the new page row appear via WorkspaceSync push.
    await expect(
      b
        .locator("[data-testid='knowledge-page-list']")
        .getByText(basename, { exact: true }),
    ).toBeVisible();
  } finally {
    await ctxA.close();
    await ctxB.close();
  }
});

/**
 * Two-tab realtime block edit: create a page + block in A, edit the
 * block content, see the same content in B.
 */
test("two tabs sync block edits via vault/org", async ({ browser }) => {
  const ctxA = await browser.newContext();
  const ctxB = await browser.newContext();
  const a = await ctxA.newPage();
  const b = await ctxB.newPage();
  try {
    await a.goto("/knowledge");
    await b.goto("/knowledge");
    await expect(
      a.locator("[data-testid='knowledge-version-badge']"),
    ).toContainText(/^v[1-9]/);
    await expect(
      b.locator("[data-testid='knowledge-version-badge']"),
    ).toContainText(/^v[1-9]/);

    const basename = `Blocks-${Date.now()}`;
    await a
      .locator("[data-testid='knowledge-new-page-input']")
      .fill(basename);
    await a
      .locator("[data-testid='knowledge-add-page-button'] button")
      .click();

    // Click the new page in A so it becomes the active page on the
    // right.
    const pageRow = a
      .locator("[data-testid='knowledge-page-list']")
      .getByText(basename, { exact: true });
    await expect(pageRow).toBeVisible();
    await pageRow.click();

    // Add a block.
    await a
      .locator("[data-testid='knowledge-add-block-button'] button")
      .click();

    // Type into the textarea.
    const message = `hello-from-A-${Date.now()}`;
    const ta = a.locator("[data-testid^='block-textarea-']").first();
    await expect(ta).toBeVisible();
    await ta.fill(message);

    // In B, select the same page and assert the textarea shows the
    // same content.
    const pageRowB = b
      .locator("[data-testid='knowledge-page-list']")
      .getByText(basename, { exact: true });
    await expect(pageRowB).toBeVisible();
    await pageRowB.click();
    await expect(
      b.locator("[data-testid^='block-textarea-']").first(),
    ).toHaveValue(message);
  } finally {
    await ctxA.close();
    await ctxB.close();
  }
});
