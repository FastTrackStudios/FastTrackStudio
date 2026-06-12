// @ts-check
// Shared harness for the multiplayer conformance suites.
//
//   - settle(): bounded convergence waiter (poll until a predicate
//     yields truthy and, optionally, stays stable) — no bare sleeps.
//   - Peer: one browser context = one presence peer. Identity is
//     seeded by writing `task.auth.active` into localStorage before
//     the app boots (the app's boot restore then performs the REAL
//     sign-in flow with the dev credentials); per-tab sessionStorage
//     gives every context its own presence client key.
//   - editor driving: vim-aware (the vault editor mounts VimState,
//     which boots in Normal mode — we click + `i` once per page),
//     caret placement through the editor's [data-tile-pos] tile map
//     (same technique as Editor/tests/editor.spec.js), text input
//     via keyboard.insertText (fires the real beforeinput path).
//   - doc reading: window.__taskVault (a wasm-side mirror of the
//     exact editor buffer — the decorated DOM cannot be scraped back
//     into doc text because hidden live-preview ranges render
//     nothing).
//   - roster reading: parses the sidebar "Online — N" group.
//   - artifacts: on failure, dump every peer's doc text + roster +
//     console ring + vox state (plus the on-disk note + server log
//     tail) into the test's output dir.

const fs = require("fs");
const path = require("path");

const STATE_FILE = path.join(__dirname, ".mp-state.json");

/** Read the stack state written by global-setup. */
function loadState() {
  return JSON.parse(fs.readFileSync(STATE_FILE, "utf8"));
}

/** Mirror of crates/ui/src/auth.rs::DEV_ACCOUNTS (email → name). */
const DEV_ACCOUNTS = [
  { email: "cody@fasttrackstudios.com", name: "Cody Wright" },
  { email: "carter@fasttrackstudios.com", name: "Carter Whitlock" },
  { email: "tom@fasttrackstudios.com", name: "Tom Brooks" },
  { email: "guest@fasttrackstudios.com", name: "Guest" },
];

// ── settle ───────────────────────────────────────────────────────

/**
 * Poll `fn` (sync or async) until it returns a truthy value, then —
 * when `stableFor` is set — keep polling and require the
 * JSON-serialized value to stay identical for that long.
 *
 * @template T
 * @param {() => T | Promise<T>} fn
 * @param {{ timeout?: number, interval?: number, stableFor?: number, label?: string }} [opts]
 * @returns {Promise<T>}
 */
async function settle(fn, opts = {}) {
  const { timeout = 30_000, interval = 250, stableFor = 0, label = "condition" } = opts;
  const deadline = Date.now() + timeout;
  let lastErr = null;
  let stableSince = 0;
  let lastJson = null;
  let lastVal = null;
  while (Date.now() < deadline) {
    let val;
    try {
      val = await fn();
      lastErr = null;
    } catch (e) {
      lastErr = e;
      val = undefined;
    }
    if (val) {
      if (!stableFor) return val;
      const json = JSON.stringify(val);
      if (json === lastJson) {
        if (Date.now() - stableSince >= stableFor) return val;
      } else {
        lastJson = json;
        stableSince = Date.now();
      }
      lastVal = val;
    } else {
      lastJson = null;
    }
    await new Promise((r) => setTimeout(r, interval));
  }
  const why = lastErr ? ` (last error: ${lastErr})` : lastVal ? ` (value never stabilized: ${JSON.stringify(lastVal).slice(0, 400)})` : "";
  throw new Error(`settle: timed out after ${timeout}ms waiting for ${label}${why}`);
}

// ── deterministic RNG (churn schedule) ───────────────────────────

/** mulberry32 — tiny seedable PRNG; identical schedules per seed. */
function mulberry32(seed) {
  let a = seed >>> 0;
  return function () {
    a |= 0;
    a = (a + 0x6d2b79f5) | 0;
    let t = Math.imul(a ^ (a >>> 15), 1 | a);
    t = (t + Math.imul(t ^ (t >>> 7), 61 | t)) ^ t;
    return ((t ^ (t >>> 14)) >>> 0) / 4294967296;
  };
}

// ── in-page snippets ─────────────────────────────────────────────

/**
 * Parse the sidebar presence roster. Returns
 * `{ count, rows: [{name, status, agent}] }` or null when the
 * "Online — N" group isn't rendered yet.
 */
const READ_ROSTER = () => {
  const leaves = Array.from(document.querySelectorAll("aside *, div"));
  const label = leaves.find(
    (e) => e.childElementCount === 0 && /^Online — \d+$/.test((e.textContent || "").trim()),
  );
  if (!label) return null;
  const count = parseInt((label.textContent || "").replace(/\D+/g, ""), 10);
  // Walk up from the label until an ancestor contains the roster
  // rows (PresenceRow roots: .rounded-md with a status-dot
  // span[title] inside).
  let group = label.parentElement;
  for (let i = 0; group && i < 4; i++) {
    if (group.querySelector("span[title]") || /Nobody around/.test(group.textContent || "")) break;
    group = group.parentElement;
  }
  if (!group) return { count, rows: [] };
  const rows = [];
  for (const row of group.querySelectorAll("div.rounded-md")) {
    const name = row.querySelector("span.font-medium")?.textContent?.trim();
    // The STATUS dot is the absolutely-positioned span on the
    // avatar's corner (`title` = status label); the avatar span
    // itself also carries a title (the peer's name) — don't grab it.
    const status = row.querySelector("span.absolute[title]")?.getAttribute("title");
    if (!name || !status) continue;
    const agent = Array.from(row.querySelectorAll("span")).some(
      (s) => (s.textContent || "").trim() === "bot" && s.className.includes("uppercase"),
    );
    rows.push({ name, status, agent });
  }
  return { count, rows };
};

/**
 * Set the editor's DOM selection at doc offsets [anchor, head]
 * (bytes — keep test content ASCII). Walks [data-tile-pos] tiles,
 * skipping widget DOM (remote carets!), mirroring the technique in
 * Editor/tests/editor.spec.js.
 */
const SET_SELECTION = ({ anchor, head }) => {
  const el = document.querySelector("[data-editor-id]");
  if (!el) throw new Error("no [data-editor-id] editor");
  const cands = Array.from(el.querySelectorAll("[data-tile-pos]"))
    .filter((t) => !t.closest(".editor-widget") && !t.classList.contains("cm-widgetBuffer"))
    .map((t) => {
      const pos = parseInt(t.getAttribute("data-tile-pos") || "0", 10);
      const first = t.firstChild;
      const text = first && first.nodeType === 3 ? first : null;
      const len = text ? text.nodeValue.length : 0;
      return { t, text, pos, end: pos + len };
    })
    .sort((a, b) => a.pos - b.pos || a.end - b.end);
  const locate = (off) => {
    let chosen = null;
    for (const c of cands) {
      if (c.pos <= off && off <= c.end) {
        if (c.text) chosen = c; // latest covering TEXT tile wins
        else if (!chosen) chosen = c; // empty tile only as fallback
      }
    }
    if (!chosen && cands.length) chosen = cands[cands.length - 1];
    if (!chosen) return { node: el, off: 0 };
    if (chosen.text) {
      return { node: chosen.text, off: Math.min(Math.max(off - chosen.pos, 0), chosen.text.nodeValue.length) };
    }
    return { node: chosen.t, off: 0 };
  };
  const a = locate(anchor);
  const h = locate(head);
  const sel = window.getSelection();
  if (!sel) throw new Error("no selection");
  sel.removeAllRanges();
  if (anchor === head) {
    const range = document.createRange();
    range.setStart(a.node, a.off);
    range.collapse(true);
    sel.addRange(range);
  } else {
    sel.setBaseAndExtent(a.node, a.off, h.node, h.off);
  }
};

// ── console policy ───────────────────────────────────────────────

/**
 * Console-error allowlist. Anything matching is recorded but does
 * NOT fail the no-console-errors assertion. Keep this list short —
 * unexplained errors are findings.
 */
const CONSOLE_ALLOWLIST = [
  // Asset 404s from the static server (favicons etc.) are not app
  // errors.
  /Failed to load resource.*404/i,
  // The dx DEBUG bundle ships the devtools hot-reload client, which
  // dials ws://…/_dioxus against our static server and fails once at
  // boot. Dev-build-only noise, absent from release bundles.
  /ws:\/\/[^ ]*\/_dioxus[^ ]*' failed/i,
  // KNOWN WART (worth fixing, not a suite failure): during boot the
  // app dials the per-org vox endpoint with an EMPTY slug
  // (`/org//vox` → 404) before org discovery resolves, then retries
  // correctly. Source: connection root established from
  // `home_slug()` while the org list is still empty.
  /ws:\/\/[^ ]*\/org\/\/vox' failed/i,
];

function isAllowedError(text) {
  return CONSOLE_ALLOWLIST.some((re) => re.test(text));
}

// ── Peer ─────────────────────────────────────────────────────────

/** Status-picker label → roster status-dot title. */
const STATUS_TITLE = {
  "Active (auto)": "Active",
  Available: "Available",
  "Do not disturb": "Do not disturb",
};

class Peer {
  /**
   * @param {import('@playwright/test').Browser} browser
   * @param {{ id: number, email: string, name: string }} opts
   */
  constructor(browser, opts) {
    this.browser = browser;
    this.id = opts.id;
    this.email = opts.email;
    this.name = opts.name;
    this.label = `peer${opts.id}(${opts.name})`;
    /** @type {import('@playwright/test').BrowserContext | null} */
    this.context = null;
    /** @type {import('@playwright/test').Page | null} */
    this.page = null;
    /** @type {{t: number, type: string, text: string}[]} */
    this.console = [];
    /** @type {string[]} */
    this.errors = [];
    /**
     * KNOWN BOOT RACE (documented finding, not a suite failure): on
     * some page boots the app dials the org vox endpoint with an
     * empty slug (`/org//vox` → 404) before discovery resolves, and
     * the failed WebSocket's queued callback then fires a DROPPED
     * wasm-bindgen closure — `Error: closure invoked recursively or
     * after being dropped` 0–1ms later. Counted here + reported, but
     * only when it follows an empty-slug failure; the same error in
     * any other context still fails the no-console-errors assertion
     * (it's the exact regression tests/playwright/smoke.spec.js
     * guards against).
     */
    this.knownBootRaces = 0;
    this._lastEmptySlugErrAt = 0;
    this.sawRemoteCursor = false;
    this.connected = false;
  }

  /** Open the context + page and wait for the signed-in shell. */
  async join(route = "/") {
    this.context = await this.browser.newContext();
    await this.context.addInitScript((email) => {
      try {
        // Boot restore reads this and performs the real dev-account
        // sign-in (crates/ui/src/auth.rs::provide_auth).
        localStorage.setItem("task.auth.active", email);
      } catch {
        /* about:blank etc. */
      }
    }, this.email);
    this.page = await this.context.newPage();
    this.page.on("console", (m) => {
      const entry = { t: Date.now(), type: m.type(), text: m.text() };
      this.console.push(entry);
      if (/\/org\/\/vox/.test(entry.text)) this._lastEmptySlugErrAt = entry.t;
      if (m.type() === "error" && !isAllowedError(entry.text)) this.errors.push(entry.text);
    });
    this.page.on("pageerror", (e) => {
      const text = String(e);
      const t = Date.now();
      this.console.push({ t, type: "pageerror", text });
      const isClosure = /closure invoked recursively or after being dropped/.test(text);
      if (isClosure && t - this._lastEmptySlugErrAt < 5_000) {
        this.knownBootRaces += 1; // see the field's doc comment
        console.log(`[mp] ${this.label}: known boot race (empty-slug dial -> dropped closure)`);
        return;
      }
      this.errors.push(text);
    });
    await this.page.goto(route, { waitUntil: "domcontentloaded" });
    await this.waitForShell();
    this.connected = true;
  }

  /** Sidebar rendered AND our account finished signing in. */
  async waitForShell(timeout = 60_000) {
    const page = this.mustPage();
    await settle(
      () =>
        page.evaluate((name) => {
          const switcher = document.querySelector('button[title="Account & status"]');
          return !!switcher && (switcher.textContent || "").includes(name);
        }, this.name),
      { timeout, label: `${this.label} signed-in shell` },
    );
  }

  mustPage() {
    if (!this.page) throw new Error(`${this.label} has no page (not joined?)`);
    return this.page;
  }

  // ── vault / editor ────────────────────────────────────────────

  /**
   * Open a note from the vault tree and wait until the collab
   * session is LIVE, then enter vim insert mode (the vault editor
   * boots in Normal mode).
   */
  async openNote(title, { liveTimeout = 45_000 } = {}) {
    const page = this.mustPage();
    if (!page.url().includes("/vault")) {
      await page.goto("/vault", { waitUntil: "domcontentloaded" });
      await this.waitForShell();
    }
    const row = page.locator("aside button", { hasText: title }).first();
    await row.waitFor({ state: "visible", timeout: 30_000 });
    await row.click();
    await settle(
      () => page.evaluate(() => (window.__taskVault?.live ? window.__taskVault : null)),
      { timeout: liveTimeout, label: `${this.label} collab live` },
    );
    // Toolbar must agree.
    await page.getByText("Collab: live", { exact: true }).waitFor({ timeout: 10_000 });
    // Focus + vim insert mode. The vault editor mounts VimState
    // (boots in Normal mode), so `i` should switch modes WITHOUT
    // inserting. Detect adaptively: if the buffer changed, the
    // editor was already accepting text — undo the stray "i".
    await page.locator("[data-editor-id]").click({ position: { x: 10, y: 10 } });
    const before = await this.docText();
    await page.keyboard.press("i");
    await new Promise((r) => setTimeout(r, 300));
    const after = await this.docText();
    if (after !== before) {
      await page.keyboard.press("Backspace");
      await settle(async () => (await this.docText()) === before, {
        timeout: 5_000,
        label: `${this.label} stray 'i' undone`,
      });
    }
  }

  /** The exact editor buffer (wasm-side mirror). */
  async docText() {
    return await this.mustPage().evaluate(() => window.__taskVault?.text ?? null);
  }

  /** Full vox/collab state mirror for artifacts. */
  async vaultState() {
    return await this.mustPage().evaluate(() => window.__taskVault ?? null);
  }

  /** Set the editor selection at doc offsets (collapsed or range). */
  async setSelection(anchor, head = anchor) {
    await this.mustPage().evaluate(SET_SELECTION, { anchor, head });
  }

  /**
   * Place the caret at the end of the line containing `marker` (per
   * the CURRENT buffer) and insert `text` there. Concurrent remote
   * edits can shift the offset between read and insert — that's the
   * point of the storm; the insert is still applied as one change.
   */
  async typeAtMarker(marker, text) {
    const doc = await this.docText();
    if (doc === null) throw new Error(`${this.label}: no doc`);
    const idx = doc.indexOf(marker);
    if (idx === -1) throw new Error(`${this.label}: marker ${JSON.stringify(marker)} not in doc`);
    const eol = doc.indexOf("\n", idx);
    const pos = eol === -1 ? doc.length : eol;
    await this.setSelection(pos);
    await this.mustPage().keyboard.insertText(text);
  }

  /** Press Backspace n times at the end of the marker's line. */
  async backspaceAtMarker(marker, n) {
    const doc = await this.docText();
    const idx = doc.indexOf(marker);
    if (idx === -1) throw new Error(`${this.label}: marker ${JSON.stringify(marker)} not in doc`);
    const eol = doc.indexOf("\n", idx);
    await this.setSelection(eol === -1 ? doc.length : eol);
    for (let i = 0; i < n; i++) {
      await this.mustPage().keyboard.press("Backspace");
    }
  }

  /** Select `target` (first occurrence) and replace it by typing. */
  async replaceText(target, replacement) {
    const doc = await this.docText();
    const idx = doc.indexOf(target);
    if (idx === -1) throw new Error(`${this.label}: ${JSON.stringify(target)} not in doc`);
    await this.setSelection(idx, idx + target.length);
    await this.mustPage().keyboard.insertText(replacement);
  }

  /**
   * The real `[[` autocomplete flow: type `[[query`, wait for the
   * completion menu, accept the top candidate with Enter.
   */
  async insertWikilink(marker, query) {
    const page = this.mustPage();
    const doc = await this.docText();
    const idx = doc.indexOf(marker);
    if (idx === -1) throw new Error(`${this.label}: marker ${JSON.stringify(marker)} not in doc`);
    const eol = doc.indexOf("\n", idx);
    await this.setSelection(eol === -1 ? doc.length : eol);
    await page.keyboard.insertText(`[[${query}`);
    await page.locator(".completion-menu .slash-row").first().waitFor({ timeout: 10_000 });
    await page.keyboard.press("Enter");
  }

  /** Count remote presence carets currently decorated in the DOM. */
  async pollRemoteCursors() {
    const n = await this.mustPage().evaluate(() => document.querySelectorAll(".collab-caret").length);
    if (n > 0) this.sawRemoteCursor = true;
    return n;
  }

  // ── presence / roster ─────────────────────────────────────────

  /** Parse the sidebar roster. */
  async roster() {
    return await this.mustPage().evaluate(READ_ROSTER);
  }

  /**
   * Change presence status via the account switcher popover
   * (label: "Active (auto)" | "Available" | "Do not disturb").
   */
  async setStatus(label) {
    const page = this.mustPage();
    await page.locator('button[title="Account & status"]').click();
    await page.getByText(label, { exact: true }).first().click();
    // Popover closes on select; give the publisher effect a beat.
    await page
      .getByText(label, { exact: true })
      .first()
      .waitFor({ state: "hidden", timeout: 5_000 })
      .catch(() => {});
  }

  /** Client-side route change via a sidebar nav item. */
  async navigate(label) {
    const page = this.mustPage();
    await page
      .locator("aside, nav, div")
      .locator(`button:has-text("${label}")`)
      .first()
      .click();
  }

  /** Refresh-rejoin: same context (same sessionStorage client key). */
  async reload() {
    await this.mustPage().reload({ waitUntil: "domcontentloaded" });
    await this.waitForShell();
  }

  /** Depart — close the whole context (presence entry must expire). */
  async leave() {
    this.connected = false;
    if (this.context) await this.context.close();
    this.context = null;
    this.page = null;
  }

  /** Snapshot for failure artifacts. */
  async snapshot() {
    const base = {
      id: this.id,
      label: this.label,
      email: this.email,
      connected: this.connected,
      sawRemoteCursor: this.sawRemoteCursor,
      knownBootRaces: this.knownBootRaces,
      errors: this.errors,
      console: this.console.slice(-300),
    };
    if (!this.page || this.page.isClosed?.()) return { ...base, docText: null, roster: null, vault: null };
    try {
      return {
        ...base,
        docText: await this.docText().catch(() => null),
        vault: await this.vaultState().catch(() => null),
        roster: await this.roster().catch(() => null),
        url: this.page.url(),
      };
    } catch {
      return { ...base, docText: null, roster: null, vault: null };
    }
  }
}

// ── failure artifacts ────────────────────────────────────────────

/**
 * Dump per-peer state + the on-disk note + server log tail into the
 * test's output dir. Call from a catch/afterEach on failure.
 *
 * @param {import('@playwright/test').TestInfo} testInfo
 * @param {Peer[]} peers
 * @param {Record<string, unknown>} [extra]
 */
async function dumpArtifacts(testInfo, peers, extra = {}) {
  const state = loadState();
  const dir = testInfo.outputDir;
  fs.mkdirSync(dir, { recursive: true });
  for (const peer of peers) {
    const snap = await peer.snapshot();
    fs.writeFileSync(path.join(dir, `peer-${peer.id}.json`), JSON.stringify(snap, null, 2));
  }
  const notePath = path.join(state.vaultDir, state.collabNote);
  const disk = fs.existsSync(notePath) ? fs.readFileSync(notePath, "utf8") : null;
  let serverTail = null;
  try {
    const log = fs.readFileSync(state.serverLog, "utf8");
    serverTail = log.split("\n").slice(-200).join("\n");
  } catch {
    /* ignore */
  }
  fs.writeFileSync(
    path.join(dir, "stack.json"),
    JSON.stringify({ ...extra, diskNote: disk, serverLogTail: serverTail, state }, null, 2),
  );
  testInfo.attachments.push({
    name: "mp-artifacts",
    contentType: "text/plain",
    body: Buffer.from(`artifacts dumped to ${dir}`),
  });
  console.log(`[mp] failure artifacts dumped to ${dir}`);
}

module.exports = {
  loadState,
  settle,
  mulberry32,
  DEV_ACCOUNTS,
  STATUS_TITLE,
  Peer,
  dumpArtifacts,
  isAllowedError,
};
