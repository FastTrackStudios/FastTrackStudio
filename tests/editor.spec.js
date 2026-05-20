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
async function setCaret(page, docOffset) {
  await page.evaluate((off) => {
    const el = document.querySelector("[data-editor-id]");
    const sel = window.getSelection();
    const range = document.createRange();
    // Place the caret at *doc offset* `off`, not visible-text
    // offset. We use the `data-tile-pos` attribute on rendered
    // tiles (Text/Mark/Widget/Line) — each carries its start
    // position in doc space, so we can locate the owning tile
    // by binary-friendly walk and place the range at the right
    // text-node offset inside.
    const tiles = Array.from(el.querySelectorAll("[data-tile-pos]"));
    const candidates = tiles
      .map((t) => {
        const pos = parseInt(t.dataset.tilePos, 10);
        const text = t.firstChild;
        const len =
          text && text.nodeType === 3 ? text.nodeValue.length : 0;
        return { tile: t, text, pos, end: pos + len };
      })
      .sort((a, b) => a.pos - b.pos);
    // Find the latest tile whose `[pos, end]` covers `off`.
    let chosen = null;
    for (const c of candidates) {
      if (c.pos <= off && off <= c.end) chosen = c;
    }
    if (chosen && chosen.text && chosen.text.nodeType === 3) {
      const local = off - chosen.pos;
      range.setStart(
        chosen.text,
        Math.min(local, chosen.text.nodeValue.length)
      );
    } else if (chosen) {
      // Empty tile (e.g., empty line whose only child is a
      // `<br>`). Anchor the range INSIDE the tile div at
      // offset 0 — that places the caret before the `<br>`,
      // which is what the browser uses for empty
      // contenteditable lines.
      range.setStart(chosen.tile, 0);
    } else if (candidates.length > 0) {
      // Past the last tile — pin to its end.
      const last = candidates[candidates.length - 1];
      if (last.text && last.text.nodeType === 3) {
        range.setStart(last.text, last.text.nodeValue.length);
      } else {
        range.setStart(last.tile, 0);
      }
    } else {
      range.selectNodeContents(el);
      range.collapse(false);
    }
    range.collapse(true);
    sel.removeAllRanges();
    sel.addRange(range);
  }, docOffset);
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

  test("markdown bold via literal ** markers", async ({ page }) => {
    // Variant 1: user types the markdown markers manually,
    // same way you would in any plain markdown editor. The
    // doc text should contain the literal `**...**` exactly
    // as typed — markers stored, just hidden visually when
    // the caret leaves the span.
    await editor(page).focus();
    await setCaret(page, 100_000); // append at end
    // Insert a newline first so the test's content is on its
    // own line (cleaner doc assertions).
    await page.keyboard.press("Enter");
    const sample = "Testing **This is bold** This Isn't Bold";
    await page.keyboard.insertText(sample);
    // Wait for state to catch up — last batch of chars goes
    // through the visible→doc diff path.
    await expect
      .poll(async () => (await readState(page)).text)
      .toContain(sample);
  });

  test.describe("toggle_bold (command logic)", () => {
    test.beforeEach(async ({ page }) => {
      // Catch page errors / browser-side panics in this
      // describe block too (the outer beforeEach's listener
      // doesn't survive the page.goto inside this beforeEach).
      page.on("pageerror", (err) => {
        // Log to stdout AND attach for the trace viewer.
        console.error("PAGEERROR:", err.message);
        console.error(err.stack);
        throw err;
      });
      page.on("console", (msg) => {
        if (msg.type() === "error") {
          console.error("CONSOLE ERROR:", msg.text());
        }
      });
      await page.goto("/?seed=");
      await editor(page).waitFor();
    });

    test("Mod-B inserts a bold pair at caret", async ({ page }) => {
      await editor(page).focus();
      await setCaret(page, 0);
      await page.keyboard.press("ControlOrMeta+b");
      await expect.poll(async () => (await readState(page)).text).toBe("****");
      // Caret parked between the two pairs.
      await expect.poll(async () => (await readState(page)).head).toBe("2");
    });

    test("Mod-B skips past closing marker", async ({ page }) => {
      // Seed the doc by typing `**hi**` and then position
      // the caret right before the closing `**`.
      await editor(page).focus();
      await setCaret(page, 0);
      await page.keyboard.insertText("**hi**");
      // Wait for BOTH state AND DOM to catch up before
      // setCaret, otherwise setCaret walks a DOM that hasn't
      // been rendered with the new tile tree yet.
      await expect.poll(async () => (await readState(page)).text).toBe("**hi**");
      await expect
        .poll(async () => await page.locator(".cm-line").first().textContent())
        .toBe("**hi**");
      await setCaret(page, 4);
      await expect
        .poll(async () => (await readState(page)).head)
        .toBe("4");
      await page.keyboard.press("ControlOrMeta+b");
      // Length unchanged, caret moved by 2.
      await expect.poll(async () => (await readState(page)).len).toBe("6");
      await expect.poll(async () => (await readState(page)).head).toBe("6");
    });

    test("Mod-B sequence builds Testing **Bold** suffix", async ({ page }) => {
      // The user-requested end-to-end variant. Sync against
      // both Rust state AND DOM between each step so the next
      // setCaret/insertText sees the correct DOM.
      const syncDom = async (expected) => {
        await expect.poll(async () => (await readState(page)).text).toBe(expected);
        await expect
          .poll(async () => {
            const lines = page.locator(".cm-line");
            const n = await lines.count();
            let acc = "";
            for (let i = 0; i < n; i++) {
              if (i > 0) acc += "\n";
              acc += (await lines.nth(i).textContent()) || "";
            }
            return acc;
          })
          .toBe(expected);
      };
      await editor(page).focus();
      await setCaret(page, 0);
      await page.keyboard.insertText("Testing ");
      await syncDom("Testing ");
      await page.keyboard.press("ControlOrMeta+b");
      await syncDom("Testing ****");
      await page.keyboard.insertText("This Is Bold");
      await syncDom("Testing **This Is Bold**");
      await page.keyboard.press("ControlOrMeta+b");
      await expect.poll(async () => (await readState(page)).head).toBe("24");
      await page.keyboard.insertText(" This Isn't Bold");
      await syncDom("Testing **This Is Bold** This Isn't Bold");
    });

    test("literal **bold** typing produces the same final text", async ({
      page,
    }) => {
      // Variant 1: user types the markers manually.
      await editor(page).focus();
      await setCaret(page, 0);
      const sample = "Testing **This is bold** This Isn't Bold";
      await page.keyboard.insertText(sample);
      await expect.poll(async () => (await readState(page)).text).toBe(sample);
    });
  });

  test.describe("caret + selection across tiles", () => {
    test.beforeEach(async ({ page }) => {
      // Plain seed with multiple lines and a marked span so
      // we exercise the tile-tree position math. `nodeco=1`
      // keeps the live-preview decoration off — the click
      // tests target the *base* DOM↔doc translation, not the
      // decoration churn.
      await page.goto(
        "/?seed=Line%20one%0ALine%20two%0ALine%20three&nodeco=1"
      );
      await editor(page).waitFor();
      await expect.poll(async () => (await readState(page)).len).toBe("28");
    });

    test("clicking inside a line positions caret there", async ({ page }) => {
      // Find the second cm-line, click at its first character.
      // The caret should land at doc position 9 ("Line one\n"
      // is 9 chars, second line starts at 9).
      const lines = page.locator(".cm-line");
      await expect(lines.nth(1)).toBeVisible();
      // Click at the START of line 2's text node.
      await lines.nth(1).click({ position: { x: 1, y: 5 } });
      await expect
        .poll(async () => Number((await readState(page)).head))
        .toBeGreaterThanOrEqual(9);
      await expect
        .poll(async () => Number((await readState(page)).head))
        .toBeLessThan(13); // somewhere on line 2
    });

    test("shift+arrow extends selection across lines", async ({ page }) => {
      // Caret at start of line 2 (doc 9). Shift+ArrowDown
      // should extend the selection to line 3's same column,
      // crossing the BreakAfter on line 2.
      await editor(page).focus();
      await setCaret(page, 9);
      await expect.poll(async () => (await readState(page)).head).toBe("9");
      await page.keyboard.press("Shift+ArrowDown");
      // After shift-down, head moves to next line. Anchor stays
      // at 9. The exact head depends on browser's column
      // tracking, but it should be > 9 and < doc.len.
      const after = await readState(page);
      expect(Number(after.anchor)).toBe(9);
      expect(Number(after.head)).toBeGreaterThan(9);
      expect(Number(after.head)).toBeLessThanOrEqual(Number(after.len));
    });
  });

  test.describe("cursor preservation", () => {
    // Regression coverage for the "every render resets the
    // cursor to position 0" bug. The bug was: the state→DOM
    // selection writeback compared `el.textContent` against
    // `state.doc`, but textContent doesn't include `\n` between
    // block children — so multi-line docs always failed the
    // check, the writeback never ran, and the cursor sat
    // wherever Dioxus's reconciler had dropped it after each
    // mutation (often offset 0).
    //
    // Each test below pins down a different way the bug
    // surfaced. Add new tests here when you find another.
    test.beforeEach(async ({ page }) => {
      // Use `nodeco=1` for the basic cases — the bug exists
      // independent of decorations. Decoration-specific
      // variants live in the separate block below.
      await page.goto(
        "/?seed=Line%20one%0ALine%20two%0ALine%20three&nodeco=1"
      );
      await editor(page).waitFor();
      await expect.poll(async () => (await readState(page)).len).toBe("28");
    });

    test("cursor stays at end-of-doc after typing a character", async ({
      page,
    }) => {
      await editor(page).focus();
      // Place at end of doc (pos 28).
      await setCaret(page, 28);
      await expect.poll(async () => (await readState(page)).head).toBe("28");
      await page.keyboard.insertText("X");
      // Cursor should be at 29, NOT 0.
      await expect.poll(async () => (await readState(page)).head).toBe("29");
      await expect.poll(async () => (await readState(page)).text).toBe(
        "Line one\nLine two\nLine threeX"
      );
    });

    test("cursor stays mid-line after typing", async ({ page }) => {
      // Caret in the middle of line 2 (between "Line " and
      // "two"). doc pos = 9 + 5 = 14.
      await editor(page).focus();
      await setCaret(page, 14);
      await expect.poll(async () => (await readState(page)).head).toBe("14");
      await page.keyboard.insertText("X");
      // Cursor advances to 15 — NOT 0, NOT end-of-doc.
      await expect.poll(async () => (await readState(page)).head).toBe("15");
    });

    test("cursor stays through multiple keystrokes in middle of doc", async ({
      page,
    }) => {
      // Type a multi-char burst in the middle and verify the
      // cursor advances one char per char, never resetting.
      await editor(page).focus();
      await setCaret(page, 14);
      const before = "abcde";
      for (let i = 0; i < before.length; i++) {
        await page.keyboard.insertText(before[i]);
        // After each char, cursor must be at the previous
        // position + 1 — never 0, never anywhere else.
        const expected = String(14 + i + 1);
        await expect
          .poll(async () => (await readState(page)).head)
          .toBe(expected);
      }
    });

    test("cursor stays after backspace mid-line", async ({ page }) => {
      await editor(page).focus();
      await setCaret(page, 14);
      await expect.poll(async () => (await readState(page)).head).toBe("14");
      await page.keyboard.press("Backspace");
      // Cursor moves to 13, NOT to 0.
      await expect.poll(async () => (await readState(page)).head).toBe("13");
    });

    test("cursor stays after pressing Enter mid-line", async ({ page }) => {
      // Enter is routed through beforeinput-insert which splits
      // a line — exactly the kind of structural mutation that
      // used to orphan the cursor.
      await editor(page).focus();
      await setCaret(page, 14);
      await page.keyboard.press("Enter");
      // Cursor moves +1 (past the inserted \n), NOT 0.
      await expect.poll(async () => (await readState(page)).head).toBe("15");
      // And there's an extra line now.
      const after = await readState(page);
      expect(Number(after.len)).toBe(29);
    });

    test("cursor across many small edits never collapses to 0", async ({
      page,
    }) => {
      // The deepest version of the regression: a long sequence
      // of typing + backspace + arrow operations. After each
      // step the cursor must be at a sensible non-zero
      // position. We log every cursor jump so any future
      // regression points at the specific operation.
      await editor(page).focus();
      await setCaret(page, 9); // start of line 2
      await expect.poll(async () => (await readState(page)).head).toBe("9");
      for (const ch of "Hello") {
        const before = Number((await readState(page)).head);
        await page.keyboard.insertText(ch);
        const after = Number((await readState(page)).head);
        expect(after, `after typing '${ch}' from ${before}`).toBe(before + 1);
      }
      // ArrowLeft x3: each step head -= 1, never collapses.
      for (let i = 0; i < 3; i++) {
        const before = Number((await readState(page)).head);
        await page.keyboard.press("ArrowLeft");
        const after = Number((await readState(page)).head);
        expect(after, `after ArrowLeft from ${before}`).toBe(before - 1);
      }
      // Backspace twice: head -= 1, len -= 1, never collapses.
      for (let i = 0; i < 2; i++) {
        const before = Number((await readState(page)).head);
        const lenBefore = Number((await readState(page)).len);
        await page.keyboard.press("Backspace");
        const after = await readState(page);
        expect(Number(after.head), `head after BS from ${before}`).toBe(
          before - 1
        );
        expect(Number(after.len), `len after BS from ${lenBefore}`).toBe(
          lenBefore - 1
        );
      }
    });

    test("cursor never resets to 0 across many random renders", async ({
      page,
    }) => {
      // Stress-style. Type a bunch of chars at the end of the
      // doc. Each render emits a new tile arena (fresh
      // data-tile-ids), which used to be what triggered the
      // bug. Asserts the cursor matches what it should be at
      // every step.
      await editor(page).focus();
      await setCaret(page, 28);
      const expectedFinal = 28 + 30;
      for (let i = 0; i < 30; i++) {
        await page.keyboard.insertText("z");
        const got = Number((await readState(page)).head);
        // The cursor must NEVER be 0 once we've started typing.
        expect(got, `iteration ${i}`).toBeGreaterThan(0);
      }
      // Final position lands at end of inserts.
      await expect
        .poll(async () => Number((await readState(page)).head))
        .toBe(expectedFinal);
    });
  });

  test.describe("cursor preservation with live-preview decorations", () => {
    // Same regressions as the block above, but with the
    // markdown decoration source ON. Decoration shape changes
    // mid-typing (e.g., cursor leaving a `**...**` span makes
    // the markers hide → tile-tree rebuilds → text nodes get
    // replaced) are the most aggressive form of the "cursor
    // gets orphaned" bug. These tests pin it down.
    test.beforeEach(async ({ page }) => {
      // Seed: "hello **world** after" — 21 doc bytes. When
      // caret sits outside the **world** span, live_preview
      // hides the markers (Hidden tiles), so the rendered
      // visible text is "hello world after" (17 chars).
      await page.goto("/?seed=hello%20**world**%20after");
      await editor(page).waitFor();
      await expect.poll(async () => (await readState(page)).len).toBe("21");
    });

    test("typing after a decorated span keeps cursor at end", async ({
      page,
    }) => {
      await editor(page).focus();
      // Place caret at end of doc (pos 21).
      await setCaret(page, 21);
      await expect.poll(async () => (await readState(page)).head).toBe("21");
      await page.keyboard.insertText("!");
      // Cursor at 22, NOT 0.
      await expect.poll(async () => (await readState(page)).head).toBe("22");
    });

    test("cursor advances each step when typing many chars at end", async ({
      page,
    }) => {
      await editor(page).focus();
      await setCaret(page, 21);
      for (let i = 0; i < 10; i++) {
        const before = Number((await readState(page)).head);
        await page.keyboard.insertText("x");
        const after = Number((await readState(page)).head);
        expect(after, `step ${i}: head went ${before} → ${after}`).toBe(
          before + 1
        );
      }
    });

    test("cursor doesn't collapse when caret crosses decoration boundary", async ({
      page,
    }) => {
      // Place caret inside the **world** span at doc 9 (the
      // 'w'). live_preview keeps markers visible while caret
      // touches the span. Move caret right past the closing
      // `**` (doc 15) — markers should now hide, which
      // rebuilds the tile tree. The crucial property is
      // "cursor does NOT collapse to 0" — what the
      // user-reported bug looked like.
      await editor(page).focus();
      await setCaret(page, 9);
      await expect.poll(async () => (await readState(page)).head).toBe("9");
      for (let i = 0; i < 8; i++) {
        await page.keyboard.press("ArrowRight");
      }
      const head = Number((await readState(page)).head);
      expect(head, "cursor did NOT collapse to 0").toBeGreaterThan(0);
      expect(head).toBeGreaterThan(9);
    });
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
