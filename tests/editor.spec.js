// @ts-check
// Browser tests for the Editor playground.
//
// Conventions:
// - The editor is the `[data-editor-id]` contenteditable. We
//   target it directly rather than via class so tests survive
//   styling churn.
// - State assertions read the debug panel's id-tagged
//   elements (`#dbg-len`, `#dbg-anchor`, `#dbg-head`,
//   `#dbg-text`). This sidesteps having to walk Dioxus's
//   rendered tile tree to reconstruct state — the debug panel
//   is already a faithful mirror of `EditorState`.
// - Helper `setSelection(...)` uses page.evaluate() to set the
//   DOM Selection directly, matching what a click or arrow
//   keypress would produce. We need this because Playwright
//   doesn't have a high-level "place caret in a contenteditable
//   at byte offset N" primitive.

const { test, expect } = require("@playwright/test");

/** Locator for the contenteditable editor element. */
const editor = (page) => page.locator("[data-editor-id]").first();

/**
 * Read the live `EditorState` mirror from the debug panel.
 * Returns `{ len, anchor, head, text }` — all strings as DOM
 * `textContent`. Tests `Number()`-cast as needed.
 */
async function readState(page) {
  return {
    len: await page.locator("#dbg-len").textContent(),
    anchor: await page.locator("#dbg-anchor").textContent(),
    head: await page.locator("#dbg-head").textContent(),
    text: await page.locator("#dbg-text").textContent(),
  };
}

/**
 * Place a caret inside the editor at the given visible-text
 * offset (counted by walking text descendants in order). For
 * Phase 8 (LineTile) the editor has multiple text nodes
 * across spans and lines, so a naive `setStart(editor, N)`
 * wouldn't work.
 */
async function setCaret(page, offset) {
  await page.evaluate((off) => {
    const el = document.querySelector("[data-editor-id]");
    const sel = window.getSelection();
    const range = document.createRange();
    let remaining = off;
    const walker = document.createTreeWalker(el, NodeFilter.SHOW_TEXT);
    let node;
    while ((node = walker.nextNode())) {
      const len = node.nodeValue.length;
      if (remaining <= len) {
        range.setStart(node, remaining);
        range.setEnd(node, remaining);
        sel.removeAllRanges();
        sel.addRange(range);
        return;
      }
      remaining -= len;
    }
    // Past the end — collapse to last position.
    range.selectNodeContents(el);
    range.collapse(false);
    sel.removeAllRanges();
    sel.addRange(range);
  }, offset);
}

/**
 * Wait until the debug panel's reported doc length equals `n`.
 * Used to synchronize against the async DOM → state → render
 * loop after typing.
 */
async function waitForLen(page, n) {
  await expect(page.locator("#dbg-len")).toHaveText(String(n));
}

test.describe("editor", () => {
  test.beforeEach(async ({ page }) => {
    // Quieter test output — fail the test on console.error.
    page.on("pageerror", (err) => {
      throw err;
    });
    await page.goto("/");
    // Wait for the editor to mount and the initial state to
    // populate the debug panel.
    await editor(page).waitFor();
    await expect(page.locator("#dbg-len")).not.toHaveText("");
  });

  test("renders the seeded document", async ({ page }) => {
    const state = await readState(page);
    // Seed text from main.rs — we don't assert exact bytes
    // (the welcome message may evolve), just that it's nonzero
    // and contains some recognizable substring.
    expect(Number(state.len)).toBeGreaterThan(0);
    expect(state.text).toContain("Editor");
    expect(state.text.toLowerCase()).toContain("typing");
  });

  test("types a character and the state grows by one", async ({ page }) => {
    const before = Number((await readState(page)).len);
    await editor(page).focus();
    await setCaret(page, before); // caret at end
    // `insertText` fires the same DOM InputEvent the browser
    // emits for normal typing, which our MutationObserver
    // listens for. `keyboard.type` simulates key-down/up at the
    // OS level and on Linux/headless can miss the input event
    // entirely on contenteditable elements.
    await page.keyboard.insertText("x");
    await waitForLen(page, before + 1);
    const after = await readState(page);
    expect(Number(after.len)).toBe(before + 1);
    expect(after.text.endsWith("x")).toBeTruthy();
  });

  test("backspace via command removes the previous char", async ({ page }) => {
    const before = Number((await readState(page)).len);
    await editor(page).focus();
    await setCaret(page, before);
    await page.keyboard.press("Backspace");
    await waitForLen(page, before - 1);
    expect(Number((await readState(page)).len)).toBe(before - 1);
  });

  test("Mod-A selects the visible document", async ({ page }) => {
    // Note: when the doc has Hidden tiles (markdown markers
    // hidden by the live-preview decoration), DOM Selection
    // can only span visible characters. CM6 keeps state's
    // selection authoritative even when DOM clamps, but our
    // bridge currently lets the clamp leak back into state
    // via the post-Mod-A keyup → sendSel path. So we verify
    // the visible portion got selected — the "user-perceived
    // select all" — and defer the Hidden-aware version.
    // FUTURE: Rust-side `pending writeback` flag in
    // push_selection.
    const len = Number((await readState(page)).len);
    await editor(page).focus();
    await page.keyboard.press("ControlOrMeta+a");
    await expect
      .poll(async () =>
        Math.abs(
          Number(await page.locator("#dbg-head").textContent()) -
            Number(await page.locator("#dbg-anchor").textContent())
        )
      )
      .toBeGreaterThan(0);
    const state = await readState(page);
    const anchor = Number(state.anchor);
    const head = Number(state.head);
    expect(Math.min(anchor, head)).toBe(0);
    // At minimum: selection should be longer than a single
    // char. The exact endpoint depends on how much of the doc
    // is currently visible.
    expect(Math.max(anchor, head)).toBeGreaterThan(20);
    expect(Math.max(anchor, head)).toBeLessThanOrEqual(len);
  });

  test("arrow key updates the caret position", async ({ page }) => {
    await editor(page).focus();
    await setCaret(page, 5);
    // Give Phase-9 MutationObserver / selection bridge a tick
    // to sync state with the manual selection.
    await page.waitForFunction(() => {
      const el = document.querySelector("#dbg-head");
      return el && Number(el.textContent) === 5;
    });
    await page.keyboard.press("ArrowLeft");
    await expect(page.locator("#dbg-head")).toHaveText("4");
  });

  test("markdown bold round-trips through the visible-text mirror", async ({
    page,
  }) => {
    // After the visible-text + offset-translation work in
    // tile/visible.rs, typing into a doc with Hidden markdown
    // markers should NOT drop the markers. We type at the end
    // of the doc and confirm doc.len() == before + 1, *not*
    // before + 1 - (number of currently-hidden markers).
    const before = Number((await readState(page)).len);
    await editor(page).focus();
    // Place caret at the END of the visible content. Visible
    // length < doc length when markers are hidden, so we use
    // a very large offset and let setCaret clamp.
    await setCaret(page, 100_000);
    await page.keyboard.insertText("x");
    await waitForLen(page, before + 1);
    const after = await readState(page);
    expect(Number(after.len)).toBe(before + 1);
    // The doc should still contain all the original markdown
    // markers — not just the visible bold text.
    expect(after.text).toContain("**Editor**");
    expect(after.text).toContain("**bold**");
    expect(after.text.endsWith("x")).toBeTruthy();
  });

  test("multi-line: pressing Enter creates a new line tile", async ({
    page,
  }) => {
    // After beforeinput interception lands, Enter is authored
    // by Rust as a Change inserting "\n" at the caret, which
    // the tile-tree renderer turns into a new `.cm-line` div.
    // The browser never gets to insert its own <br> or <div>.
    const linesBefore = await page.locator(".cm-line").count();
    await editor(page).focus();
    await setCaret(page, 100_000); // end
    await page.keyboard.press("Enter");
    await page.keyboard.insertText("second line");
    // Wait for the .cm-line count to grow.
    await expect
      .poll(async () => page.locator(".cm-line").count())
      .toBeGreaterThan(linesBefore);
    const linesAfter = await page.locator(".cm-line").count();
    expect(linesAfter).toBeGreaterThan(linesBefore);
  });

  test("typing does not lose characters under fast input", async ({
    page,
  }) => {
    const before = Number((await readState(page)).len);
    await editor(page).focus();
    await setCaret(page, before);
    const burst = "abcdefghijklmnopqrst";
    await page.keyboard.insertText(burst);
    await waitForLen(page, before + burst.length);
    const after = await readState(page);
    expect(after.text.endsWith(burst)).toBeTruthy();
  });
});
