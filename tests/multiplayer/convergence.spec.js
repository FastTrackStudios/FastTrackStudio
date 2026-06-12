// @ts-check
// Suite 1 — 5-way editor convergence (tracked issue dd824506).
//
// Two tests:
//
//  1. "baseline" — two peers, light traffic (under the vox credit
//     window, see below): proves the whole stack converges TODAY:
//     editor → replica → server → disk → other peers, presence
//     cursors, no console errors.
//
//  2. "storm" — the full 5-context concurrent edit storm from the
//     PRD. Marked test.fail() with a precise finding:
//
//     ┌────────────────────────────────────────────────────────────┐
//     │ FINDING (dd824506 verification, 2026-06-12): every vox     │
//     │ server→client stream FREEZES after exactly 16 delivered    │
//     │ messages (DEFAULT_INITIAL_CHANNEL_CREDIT in vox-types).    │
//     │ The wasm client consumes its Rx but its credit grants      │
//     │ never replenish the server side, so PubSub::drain_one sees │
//     │ try_send → Full forever and the subscriber's mailbox grows │
//     │ silently. Client→server traffic is unaffected (server-side │
//     │ Rx replenishes fine): in a 2-peer probe, peer B stopped    │
//     │ receiving after `backlog + 15` updates while the server    │
//     │ merged ALL 40 of peer A's edits to disk; SyncStatus stayed │
//     │ Live throughout. The per-file cursor channel freezes the   │
//     │ same way (remote carets vanish ~30s later via expiry).     │
//     │ Likely the observable root of / sibling to P0 6303584a.    │
//     │ When the fix lands this test will "pass unexpectedly" —    │
//     │ remove the test.fail() line then.                          │
//     └────────────────────────────────────────────────────────────┘
//
// The offline/return scenario is a separate spec —
// offline-return.spec.js (skipped until P0 6303584a merges).

const fs = require("fs");
const path = require("path");
const { test, expect } = require("@playwright/test");
const { loadState, settle, DEV_ACCOUNTS, Peer, dumpArtifacts } = require("./helpers");

const NOTE_TITLE = "Collab Test";
const ROUNDS = 6;

// Seeded note: one marker line per peer so concurrent edits target
// disjoint regions (offsets still shift across peers — that's the
// storm). ASCII only: the tile-walk caret helper works in byte
// offsets and the tokens must be index-comparable in JS strings.
const DELETE_TARGET = "XDELETEMEXXX"; // 12 chars, removed 4-per-round
const REPLACE_TARGET = "REPLACE_TARGET_AAA";
const SEED = `# Collab Test

alpha: .
bravo: .
charlie: ${DELETE_TARGET}
delta: .
echo: ${REPLACE_TARGET}

tail.
`;

/** Re-seed the note and stand up n peers on it. */
async function bringUp(browser, n, state) {
  fs.writeFileSync(path.join(state.vaultDir, state.collabNote), SEED);
  const identities = [...DEV_ACCOUNTS, DEV_ACCOUNTS[0]].slice(0, n);
  const peers = identities.map((a, i) => new Peer(browser, { id: i, email: a.email, name: a.name }));
  for (const peer of peers) await peer.join("/vault");
  for (const peer of peers) await peer.openNote(NOTE_TITLE);
  await settle(
    async () => {
      const texts = await Promise.all(peers.map((p) => p.docText()));
      return texts.every((t) => t === SEED) ? texts[0] : null;
    },
    { timeout: 30_000, label: "all peers see the seeded note" },
  );
  return peers;
}

/** Quiesce: all buffers identical and stable. */
async function quiesce(peers, timeout = 90_000) {
  return settle(
    async () => {
      const texts = await Promise.all(peers.map((p) => p.docText()));
      if (texts.some((t) => t === null)) return null;
      return texts.every((t) => t === texts[0]) ? texts[0] : null;
    },
    { timeout, stableFor: 2_500, label: "all peer buffers identical" },
  );
}

test.describe("5-way editor convergence", () => {
  test.setTimeout(420_000);

  test("baseline: two peers converge bidirectionally (and on disk)", async ({ browser }, testInfo) => {
    const state = loadState();
    const notePath = path.join(state.vaultDir, state.collabNote);
    const peers = await bringUp(browser, 2, state);
    try {
      // Three edits each, interleaved — comfortably inside the vox
      // 16-message credit window so the known stream-freeze finding
      // (see header) doesn't mask basic conformance.
      for (let r = 0; r < 3; r++) {
        await Promise.all([
          peers[0].typeAtMarker("alpha:", ` (a${r})`),
          peers[1].typeAtMarker("bravo:", ` (b${r})`),
        ]);
        await Promise.all(peers.map((p) => p.pollRemoteCursors()));
        await new Promise((res) => setTimeout(res, 400));
        await Promise.all(peers.map((p) => p.pollRemoteCursors()));
      }

      const converged = await quiesce(peers, 30_000);
      for (const tokn of ["(a0)", "(a1)", "(a2)", "(b0)", "(b1)", "(b2)"]) {
        expect(converged.split(tokn).length - 1, `${tokn} exactly once`).toBe(1);
      }
      await settle(() => fs.readFileSync(notePath, "utf8") === converged, {
        timeout: 20_000,
        label: "note on disk equals the converged text",
      });
      for (const peer of peers) {
        const vault = await peer.vaultState();
        expect(vault?.live, `${peer.label} collab live`).toBe(true);
        expect(peer.sawRemoteCursor, `${peer.label} saw a remote cursor`).toBe(true);
        expect(peer.errors, `${peer.label} console errors:\n${peer.errors.join("\n")}`).toHaveLength(0);
      }
    } catch (err) {
      await dumpArtifacts(testInfo, peers, { suite: "convergence-baseline" });
      throw err;
    } finally {
      for (const peer of peers) await peer.leave().catch(() => {});
    }
  });

  test("storm: five concurrent editors converge everywhere (and on disk)", async ({ browser }, testInfo) => {
    test.fail(
      true,
      "FINDING: vox downstream credit starvation — every server→client stream freezes after " +
        "16 messages (DEFAULT_INITIAL_CHANNEL_CREDIT), so peers stop receiving mid-storm while " +
        "SyncStatus stays Live and the server/disk merge ALL edits correctly. " +
        "See the header of this spec + tests/multiplayer/README.md. Remove this marker when fixed.",
    );
    const state = loadState();
    const notePath = path.join(state.vaultDir, state.collabNote);
    const peers = await bringUp(browser, 5, state);

    /** Tokens every peer types; asserted present exactly once. */
    const tokens = [];
    const tok = (s) => {
      tokens.push(s);
      return s;
    };

    try {
      // ── the storm ────────────────────────────────────────────
      // Each round fires all five peers' edits concurrently at
      // different document positions: p0 short chunks, p1 longer
      // chunks, p2 deletes (then types), p3 a `[[` autocomplete
      // wikilink (then types), p4 select-and-replace (then types).
      for (let r = 0; r < ROUNDS; r++) {
        const actions = [
          peers[0].typeAtMarker("alpha:", tok(` (a${r})`)),
          peers[1].typeAtMarker("bravo:", tok(` (b${r}-some-longer-chunk)`)),
          r < 3
            ? peers[2].backspaceAtMarker("charlie:", 4)
            : peers[2].typeAtMarker("charlie:", tok(` (c${r})`)),
          r === 1
            ? peers[3].insertWikilink("delta:", "Link")
            : peers[3].typeAtMarker("delta:", tok(` (d${r})`)),
          r === 0
            ? peers[4].replaceText(REPLACE_TARGET, tok("(replaced-by-echo)"))
            : peers[4].typeAtMarker("echo:", tok(` (e${r})`)),
        ];
        await Promise.all(actions);
        // Cursor presence is debounced 200ms; poll between rounds so
        // every peer gets a chance to observe a remote caret.
        await Promise.all(peers.map((p) => p.pollRemoteCursors()));
        await new Promise((res) => setTimeout(res, 400));
        await Promise.all(peers.map((p) => p.pollRemoteCursors()));
      }

      // ── quiesce: all five buffers identical & stable ─────────
      const converged = await quiesce(peers);

      // ── disk agrees (server write-behind, ~1s debounce) ──────
      await settle(() => fs.readFileSync(notePath, "utf8") === converged, {
        timeout: 20_000,
        label: "note on disk equals the converged text",
      });

      // ── content invariants ───────────────────────────────────
      for (const token of tokens) {
        const count = converged.split(token).length - 1;
        expect(count, `token ${JSON.stringify(token)} must appear exactly once`).toBe(1);
      }
      expect(converged, "deletion target fully removed").not.toContain(DELETE_TARGET);
      expect(converged, "wikilink autocomplete inserted the full link").toContain("[[Link Target]]");
      expect(converged, "markers intact").toContain("alpha:");

      // ── collab still live everywhere ─────────────────────────
      for (const peer of peers) {
        const vault = await peer.vaultState();
        expect(vault?.live, `${peer.label} collab live`).toBe(true);
        expect(vault?.status, `${peer.label} sync status`).toBe("Live");
      }

      // ── presence cursors: every peer saw a remote caret ──────
      for (const peer of peers) {
        expect(peer.sawRemoteCursor, `${peer.label} saw at least one remote cursor`).toBe(true);
      }

      // ── console hygiene ──────────────────────────────────────
      for (const peer of peers) {
        expect(peer.errors, `${peer.label} console errors:\n${peer.errors.join("\n")}`).toHaveLength(0);
      }
    } catch (err) {
      await dumpArtifacts(testInfo, peers, { suite: "convergence-storm" });
      throw err;
    } finally {
      for (const peer of peers) await peer.leave().catch(() => {});
    }
  });
});
