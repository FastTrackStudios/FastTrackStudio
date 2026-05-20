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
    expect(state.text.toLowerCase()).toContain("type");
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

  // Markdown live-preview round-trip test is parked until the
  // input bridge handles visible↔doc offset translation through
  // the tile tree. The Hidden decoration emits 0 visible bytes
  // for the `**` markers, but the textContent-based diff in
  // editor.rs::handle_bridge_msg doesn't yet account for that
  // and would drop hidden bytes on each keystroke. See the
  // FUTURE comment in examples/playground/src/main.rs.
  test.skip("typing a markdown bold range survives caret movement", async () => {});

  // Multi-line Enter test is parked. The browser's
  // contenteditable Enter behavior inserts non-`.cm-line`
  // elements (Chrome adds a plain <div>, Firefox a <br>) that
  // our readText() — which joins `.cm-line` contents with \n —
  // misreads. The mismatch produces fake Changes on each
  // observer fire, looping. The real fix is intercepting
  // `beforeinput` and applying the edit ourselves (CM6's
  // domchange.ts), which is its own follow-up port phase.
  test.skip("multi-line: pressing Enter creates a new line tile", async () => {});

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
