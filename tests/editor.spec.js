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
    // and contains some recognizable strings.
    expect(Number(state.len)).toBeGreaterThan(0);
    expect(state.text).toContain("Editor");
    expect(state.text).toContain("Type");
  });

  test("types a character and the state grows by one", async ({ page }) => {
    const before = Number((await readState(page)).len);
    await setCaret(page, before); // caret at end
    await editor(page).focus();
    await page.keyboard.type("x");
    await waitForLen(page, before + 1);
    const after = await readState(page);
    expect(Number(after.len)).toBe(before + 1);
    expect(after.text.endsWith("x")).toBeTruthy();
  });

  test("backspace via command removes the previous char", async ({ page }) => {
    const before = Number((await readState(page)).len);
    await setCaret(page, before);
    await editor(page).focus();
    await page.keyboard.press("Backspace");
    await waitForLen(page, before - 1);
    expect(Number((await readState(page)).len)).toBe(before - 1);
  });

  test("Mod-A selects the entire document", async ({ page }) => {
    const len = Number((await readState(page)).len);
    await editor(page).focus();
    // `Meta` on Mac, `Control` elsewhere — Playwright maps
    // `ControlOrMeta` to whichever is the platform Mod.
    await page.keyboard.press("ControlOrMeta+a");
    const state = await readState(page);
    // Selection covers 0..len (anchor + head may be in either
    // order depending on direction; we just check the range
    // is [0, len]).
    const anchor = Number(state.anchor);
    const head = Number(state.head);
    expect(Math.min(anchor, head)).toBe(0);
    expect(Math.max(anchor, head)).toBe(len);
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

  test("typing a markdown bold range survives caret movement", async ({
    page,
  }) => {
    // The Obsidian-style live preview hides `**` markers when
    // the caret leaves the span. We verify the DOM doesn't lose
    // the *underlying* text — even when markers are visually
    // hidden, the doc still contains them.
    const before = Number((await readState(page)).len);
    await setCaret(page, before);
    await editor(page).focus();
    await page.keyboard.type("\n**hi**");
    await waitForLen(page, before + 7); // newline + 6 chars
    // Move caret away from the span so markers should be
    // replaced visually but doc text is unchanged.
    await setCaret(page, 1);
    const after = await readState(page);
    expect(after.text.endsWith("**hi**")).toBeTruthy();
  });

  test("multi-line: pressing Enter creates a new line tile", async ({
    page,
  }) => {
    const before = Number((await readState(page)).len);
    await setCaret(page, before);
    await editor(page).focus();
    await page.keyboard.press("Enter");
    await page.keyboard.type("second");
    await waitForLen(page, before + 1 + 6);
    // Two .cm-line elements in the DOM (or more if the seed
    // text already had newlines).
    const lineCount = await page.locator(".cm-line").count();
    expect(lineCount).toBeGreaterThanOrEqual(2);
  });

  test("typing does not lose characters under fast input", async ({
    page,
  }) => {
    const before = Number((await readState(page)).len);
    await setCaret(page, before);
    await editor(page).focus();
    // Burst-type 20 characters. Each should appear in the doc.
    const burst = "abcdefghijklmnopqrst";
    await page.keyboard.type(burst, { delay: 5 });
    await waitForLen(page, before + burst.length);
    const after = await readState(page);
    expect(after.text.endsWith(burst)).toBeTruthy();
  });
});
