# Bible Study — Feature Plan

> Status: **research / design draft** (2026-06-16). Branch `feat/biblical-research`,
> worktree `Task-wt-biblical`. Multiple agents will pick up slices from here.
>
> Goal: study the Bible the way you'd want to in Obsidian, but without Obsidian's
> rough edges — every verse (and every *word*) directly linkable, translations
> comparable, original languages and lexicons inline, ESV-study-Bible-grade
> annotations — all in **raw markdown an LLM can read**, wired into the existing
> **vault** (personal thoughts/experience) and **LLM wiki** (facts/deep studies).

---

## 0. The thesis (what we're actually building)

Three knowledge layers that already exist in this app, plus a new one:

| Layer | Holds | Backed by |
|---|---|---|
| **Vault** | your personal thoughts, experiences, questions, devotional notes | `features/vault/*` (markdown + block IDs on disk) |
| **Wiki** | LLM-generated facts, entity pages, deep studies | `features/wiki/*` (`<vault>/Wiki/` markdown) |
| **Scripture spine** *(new)* | the Bible text itself + original languages + lexicons + cross-refs | new `scripture` feature (**read-only** bundled data) |
| **Annotations** *(new)* | your verse-anchored study notes, typed like NET/ESV apparatus | hybrid: structured refs in CRDT, prose in vault markdown |

The central design move — and the thing that beats Obsidian — is to **separate the
scripture spine from the thought graph**. Scripture is a stable, addressable,
**read-only** substrate. Your notes, the wiki, and cross-references all *link
into* it by stable verse/word ID. They are never written *inside* the Bible files
(the #1 mistake every Obsidian setup makes).

> **Read-only by design (confirmed).** Users cannot edit the Bible text — the reader
> and verse blocks are immutable from the UI. Only the *system* (ingest/generation
> tooling) writes scripture files. Annotations, highlights, and links live in separate
> layers that reference verses; the text itself is never mutated by a user action.

Because this app already has a `BlockIndex` (`features/vault/vault-live/src/blocks.rs`,
`uuid → (page, offset)`, O(1)), **per-verse clean backlinks come for free** — which
is exactly the pain point that forces Obsidian users into 31k-file vaults. We get
granular backlinks *at chapter-file scale*. That single property dissolves the
whole "per-verse vs per-chapter" war the community has never resolved.

---

## 1. Use cases to satisfy (from the brief)

1. Read the Bible *in this app*, switching translations freely.
2. Link directly to **any single verse** from a note (`[[John 3:16]]`).
3. Link to a **specific word or phrase within a verse** (language study).
4. **Compare translations** side-by-side.
5. See the **original language** (Hebrew/Greek) with lemma + morphology + Strong's.
6. **Annotate** verses/ranges like an ESV Study Bible, in raw markdown.
7. Bible text is **entity-tagged**: a city/person in the text links to its wiki page.
8. Build **timelines** and **question/topic notes** that aggregate every relevant passage.
9. Wiki entries (locations, people, events) act as **sources/context** for reading.

---

## 2. Data model — the keystone

Everything hangs on a stable verse/word addressing scheme. Get this right first.

### 2.1 Verse addressing
- **Human / interchange key:** OSIS `osisID` string — `Gen.1.1`, ranges `Gen.1.10-12`.
  (CrossWire standard; STEP data already uses this style.)
- **Sortable primary key:** `BBCCCVVV` integer — `Gen 1:1 → 1001001`, `John 3:16 → 43003016`.
  Book (1–2 digits) + chapter (3) + verse (3). Used for ordering/range queries.
- **Block ID:** each verse is an addressable block (Logseq model) carrying its stable
  ID. Referencing a verse renders it **by transclusion** (live), never by copy — edit
  the source, every reference updates.

### 2.2 Sub-verse (word) addressing — the differentiator
- Each original-language word has a stable word ID (OSHB/MACULA already provide these).
- A note can anchor to `(verse, wordSpan)` — e.g. "the Greek behind *love* in John 3:16".
- This is **unsolved in Obsidian** (no native sub-verse anchor). It's a clear win and
  the foundation of language study.

### 2.3 Versification mapping
- Translations number verses differently (Psalm titles, the Ps 9/10 split, Jeremiah in
  LXX, Daniel additions, 3 John). A reference must resolve across editions.
- **Ship the Copenhagen Alliance `versification-specification` JSON mappings**
  (base = `org`, plus `eng/lxx/vul/rsc/rso`). Structure gives `maxVerses`,
  `mappedVerses` (`"PSA 9:22-39":"PSA 10:1-18"`), `excludedVerses`, `partialVerses`.
  Alternative: STEP **TVTMS** TSV.

### 2.4 Ingest format
- Bundle texts as **USFM** (the most widely distributed format — eBible.org), convert
  to **USJ** (USFM-as-JSON, USFM 3.1) via `usfm-grammar`. USJ → Rust structs / block
  store / markdown emission with no XML parser.
- Keep OSIS/USX/OSIS-XML as archival/interchange only, not the internal model.

---

## 3. Data sources (all bundleable unless noted)

### Bundle offline — public domain / CC0 / CC BY (no API key needed)
- **Reading texts:** **WEB** (World English Bible, PD) and **BSB** (Berean Standard
  Bible, **CC0** since 2023) are the zero-restriction modern-English defaults. Add
  **KJV** (PD except UK Crown copyright — prefer WEB/BSB for UK/EU) and **ASV/YLT** as
  classics. Source: **eBible.org USFM** → `usfm-grammar`.
- **Original language:** **STEP TAGNT/TAHOT** (amalgamated Greek/Hebrew, disambiguated
  Strong's + morphology, **CC BY 4.0, flat TSV** — the workhorse) + **OSHB/morphhb**
  (Hebrew word IDs) + **MACULA** (Clear Bible: syntax trees, senses, glosses — best for
  deep study). All bundleable with attribution.
- **Lexicons (all PD):** Strong's (openscriptures/strongs), BDB (Hebrew),
  Thayer's (Greek), STEP **TBESH/TBESG** brief + **TFLSJ** (full Liddell-Scott-Jones).
- **Interlinear/alignment:** **Berean interlinear** (PD, 2023) + OSHB word IDs.
- **Entities seed:** STEP **TIPNR** — every proper noun → forms + **genealogy +
  geolocation** (directly usable as Factbook-style entity seeds).
- **Cross-references:** **Treasury of Scripture Knowledge** (TSK, public domain).
- **Versification:** Copenhagen Alliance mappings (above).
- **NET Bible:** text + ~60k **typed footnotes** via labs.bible.org API
  (`?passage=...&formatting=full&type=json`, free non-commercial) — a ready-made
  typed-annotation corpus to model our apparatus against, and a generously-licensed
  translation.

> ⚠️ **Snapshot the SBLGNT license at ship time** — it moved from a restrictive EULA
> to CC BY 4.0; verify before bundling.

### Fetch via API only (copyrighted — never bundle)
- **NIV** *(priority — user wants this)*. **Tightly controlled by Biblica / Zondervan /
  HarperCollins** — among the hardest licenses in the space. Not reliably available on a
  free/standard API tier the way ESV is. Options to verify, in order of preference:
  1. **API.Bible** (American Bible Society) — confirm NIV is in catalog + cost/commercial terms.
  2. **Faithlife/Biblia API** — has NIV, but anti-compete + no-DB-extraction clause (read ToS).
  3. Direct **Biblica license** for heavier/commercial use.
  Whichever we use: NIV is **fetched per-passage, cached within license limits, never
  bundled or persisted as a redistributable file**. Notes anchor to verse IDs, so the
  vault stays shareable even though the NIV text isn't.
- **ESV** → ESV API (`api.esv.org`, free non-commercial, cache ≤500 verses / ½ book).
- **NLT / NASB / CSB / NRSV** + broad catalog → **API.Bible** (per-translation paid for
  commercial). Convenience PD live lookup without keys: **Bolls.life** or **GetBible v2**.

> **Copyright-clean by design:** annotations/notes anchor to **verse IDs**, not to
> copyrighted text. Licensed translations are fetched/optional. The vault stays legally
> shareable. (This is why Obsidian's "My Bible" plugin fetches instead of persisting.)

---

## 4. Feature set (prioritized)

Tags: **[must]** load-bearing first cut · **[high]** · **[nice]**.

### Foundation
- **[must]** Verse addressing scheme + permalinks (§2.1) — the keystone.
- **[must]** Strong's-tagged bundled text (STEP TTESV/TAGNT/TAHOT): every word →
  `H####`/`G####` + lemma + morphology.
- **[must]** Lexicon entries as KB pages — one page per Strong's ID (TBESH/TBESG/TFLSJ).
- **[high]** Versification mapping (§2.3).

### Reading & comparison
- **[must]** In-app reader, chapter-at-a-time, verse blocks — **read-only** (no user edit).
- **[must]** Translation switch + **side-by-side parallel comparison** (stable IDs, no
  link breakage on swap). Bundled PD texts (WEB/BSB/KJV) plus API-fetched **NIV/ESV**.
- **[high]** Translation-as-a-layer: one set of verse IDs, swappable text (bundled PD or
  API-fetched licensed).

### Word-level study
- **[must]** Click word → Strong's ID → lexicon entry (BLB/BibleHub/STEP chain).
- **[must]** **Every-occurrence concordance** (click lemma → every verse using it) — this
  is just a backlinks query over the tagged text (Englishman's Concordance model).
- **[high]** Reverse interlinear / hover-parse (English↔original highlight, lemma+morph).
- **[nice]** Translation-count breakdown (how a lemma is rendered, with counts).
- **[nice]** Sense-based lookup (Logos Bible Sense Lexicon) — no open dataset; LLM-tag later.

### Cross-references & annotations (the heart)
- **[must]** **TSK cross-reference graph**, phrase-keyed, with target verse text inlined.
- **[must]** **Typed annotations** (NET model): each note carries a category enum —
  `tc` text-critical · `tn` translator's · `sn` study · `map`. Filterable / color-codable
  / toggleable. Highest-leverage idea here.
- **[must]** **Two-level anchoring** (ESV model): section/passage notes (titled, span a
  range) + verse/word notes that quote the keyed text. Anchor =
  `(book, ch, vStart, vEnd, optional wordSpan)`.
- **[high]** Multi-anchor / reference-anchored notes (Logos): one note → several
  non-contiguous passages; appears at that ref across all translations.
- **[high]** ESV cross-reference notation grammar as typed refs (`ver.`, `ch.`,
  `[...]` thematic, `See`, `For…see` parallels).
- **[high]** Highlighting + saved-filter overlays (Logos Visual Filters).

### Entity / knowledge-base layer (your stated priority)
- **[high]** **Factbook-style entity pages** for people/places/things/events as
  first-class wiki notes; entities in the verse text become links. Seed from STEP
  **TIPNR**. This is the bridge into the existing wiki feature.
- **[high]** Entity cross-linking: one entity backs several views (a node is
  simultaneously a Factbook page, a Timeline node, an Atlas location).
- **[nice]** Atlas/geography (TIPNR has coordinates) — map view, hover → entity snippet.
- **[nice]** Timeline/chronology of events linked to verses + entity pages.

### Synthesis workflows
- **[high]** **Topic / question notes** that aggregate passages — a topic is a vault/wiki
  note collecting passages + sub-topics + related topics. This is exactly what a PKB does
  well (a topic = a note with backlinks/queries). Directly serves "I have a question and
  link to everything that helps answer it."
- **[high]** **Passage Guide panel** (Olive Tree "context-follows-cursor"): side panel
  resolving all cross-refs/notes/commentaries/entities for the focused verse — a fan-out
  query from the current verse anchor.
- **[nice]** Book/section intros + outlines (ESV template), inline charts/maps.

### Original-language power tools (advanced, later)
- **[nice]** Morphology search (we have STEP morph tags).
- **[nice]** Syntax/clause search (Accordance-style) — needs syntax-tree DB (MACULA);
  likely overkill for personal study.

---

## 5. Pain points we explicitly beat (vs Obsidian/Logseq)

1. **Per-verse backlink granularity without 31k files** — our `BlockIndex` gives each
   verse its own clean backlink set while chapters stay the file unit. *The* killer fix.
2. **Sub-verse word/phrase anchoring** — nobody does this well; §2.2.
3. **Graph that doesn't drown in scripture** — keep the scripture spine separate from the
   thought graph; filter scripture out of the graph by default.
4. **Native translation comparison** — parallel view, stable IDs, no link breakage.
5. **Original-language layer** — Strong's/Hebrew/Greek/lexicon inline; the biggest gap
   vs Logos-class tools, and we have the open data to do it.
6. **Copyright-clean by design** — notes anchor to IDs; licensed text fetched, optional.
7. **Zero-friction onboarding** — bundle a PD Bible natively; no Ruby/Perl/regex cleanup.
8. **Name disambiguation** — James-the-book vs James-the-apostle as distinct typed
   entities in the data model, not user-invented `- Book` suffix hacks.

---

## 6. How it maps onto this codebase

Architecture pattern (from `AGENTS.md`): feature trio `proto / crdt / db / ui / facade`,
Loro is canonical, cross-feature refs via proto `Option<Uuid>`, fts-ui primitives only,
theme tokens, dumb components.

**Recommended shape — hybrid, three pieces:**

1. **`scripture` (bundled reference data, mostly read-only).** New crate(s) for the
   verse store, tagged text, lexicons, cross-refs, versification. Most of this is *not*
   user-mutable domain data, so it can be **bundled assets + an index**, not heavy CRDT.
   - Reader UI: new route `feature_routes/scripture.rs`, mounted in `crates/ui` shell.
   - Reuse/extend the markdown renderer model in `features/task/task-ui/src/markdown.rs`
     (`MdBlock`/`MdInline`) for verse rendering, or a purpose-built verse component.
   - Permalinks + verse-block IDs feed the existing `BlockIndex`.

2. **Annotations — hybrid.** Structured anchors (`verse range`, `wordSpan`, category
   enum, multi-anchor list) as a small CRDT feature trio (`bible-notes-proto/crdt/db`);
   the prose body lives as **vault markdown** so it's raw, LLM-readable, and shareable.
   Cross-feature link to a verse = `Option<Uuid>` verse-block ref via proto.

3. **Entities → wiki.** Bible people/places/events are **wiki pages** under
   `<vault>/Wiki/Entities/`. Seed an ingest from STEP TIPNR via the existing wiki ingest
   pipeline (`wiki-proto` two-step: `enqueue_ingest` → `record_analysis` → `record_pages`).
   Entity-tagging the running scripture text links into these pages — reuse the wikilink
   parser in `features/vault/vault-obsidian/src/obsidian_parse.rs`
   (`[[Page]]`, `[[Page#^block]]`, `((uuid))`).

**Linking/graph reuse:** verse refs and word refs extend the existing ref kinds; the
`VaultGraph` backlinks surface "every note touching this verse." The 4-signal relevance
scorer (`features/wiki/wiki-graph`) can score passage↔note↔entity relevance.

**Text editing caveat:** collaborative prose editing is blocked on the
`loro-text-editor-upgrade` work (`Block.content: String` LWW loses concurrent edits).
For v1, annotations can be vault-markdown edited through the existing vault path; don't
build on per-keystroke string CRDT writes until that upgrade lands.

---

## 7. Suggested build order (slices for agents)

1. **Data spine.** Ingest WEB + BSB (USFM → USJ), assign verse IDs (OSIS + BBCCCVVV),
   bundle versification map. Verify a verse resolves and renders. *(no UI yet)*
   - ✅ **Started (2026-06-16).** New `features/scripture/` crates:
     `scripture-proto` (wasm-clean keystone — 66-book canon, `VerseId` with OSIS +
     BBCCCVVV keys, human/OSIS reference parsing, translation/licensing registry) and
     `scripture` (native USFM ingest → in-memory `Bible` store). Real WEB Gospel of John
     bundled as `assets/web/JHN.usfm`; `John 3:16` resolves end-to-end to clean text. 17
     unit tests pass, clippy clean. WEB USFM carries per-word `\w …|strong="G…"\w*` tags —
     captured as the slice-4 word-study hook (`// FUTURE` in `usfm.rs`).
   - ⬜ **Remaining:** bundle the *full* WEB corpus (all 66 books, same
     `assets/<tx>/<BOOK>.usfm` layout) + **BSB**; add the versification map (Copenhagen
     Alliance JSON or STEP TVTMS); decide bundled-assets vs feature-trio for the store.
2. **Reader + permalinks.** Chapter reader route, verse blocks, `[[John 3:16]]` linking,
   backlinks per verse via `BlockIndex`.
3. **Translation layer.** Swap + side-by-side parallel; wire ESV API behind a user key.
4. **Strong's + lexicons.** Bundle STEP TAGNT/TAHOT + Strong's/BDB/Thayer's; click-word →
   lexicon page; every-occurrence concordance (backlinks query).
5. **Annotations.** Typed (NET) + two-level (ESV) anchoring; vault-markdown bodies.
6. **TSK cross-references.** Phrase-keyed, target text inlined.
7. **Entities.** TIPNR → wiki ingest; entity-tag the text; Factbook pages.
8. **Synthesis.** Topic/question notes, Passage Guide panel.
9. *(later)* Atlas, timeline, morphology search, sense lexicon.

Pillars to nail in the first cut (≈80% of the value on free, redistributable,
markdown-friendly data): **typed annotations + two-level verse anchoring + TSK
phrase-keyed cross-refs + Strong's click-through**, on a **stable verse/word ID spine**
with **translation-as-a-swappable-layer**.

---

## 8. Decisions & open questions

**Decided (2026-06-16):**
- **Scripture is read-only.** Users cannot edit Bible text; only system tooling writes it.
- **NIV is a priority translation**, fetched via API (never bundled — copyright). Verify
  the licensing path (API.Bible vs Biblia vs direct Biblica) before wiring — see §3.

**Open:**
- **NIV license path** — confirm which API actually serves NIV and on what terms.
- **Versification source:** Copenhagen Alliance JSON vs STEP TVTMS — pick one, normalize.
- **Verse store as bundled assets vs a real feature trio** — it's read-only, so lean to
  **bundled + indexed** (not heavy CRDT); annotations/entities = CRDT/vault.
- **MACULA depth:** ship syntax trees in v1, or just lemma+morph+Strong's and add trees
  later? (Lean: defer trees.)
- **Sub-verse anchor encoding** in markdown — how do we serialize a `wordSpan` so it
  round-trips and stays LLM-readable?
- **Graph separation** — explicit "scripture layer" flag so the spine doesn't flood the
  thought graph.

---

## 9. Key sources

- Open data: STEPBible-Data (CC BY 4.0 TSV), OpenScriptures morphhb, MACULA (Clear Bible),
  eBible.org (USFM), bereanbible.com (BSB CC0), openscriptures/strongs, Copenhagen-Alliance
  versification-specification, labs.bible.org (NET API), `usfm-grammar` (Bridgeconn).
- Feature references: Logos (Factbook/Bible Word Study/reverse interlinear), Accordance
  (construct search), Blue Letter Bible + BibleHub (Strong's/interlinear/TSK), STEP Bible,
  NET Bible (typed notes), ESV Study Bible (two-level apparatus).
- Obsidian/Logseq prior art: tim-hub/obsidian-bible-reference, kuchejak/bible-linker,
  pmbauer/av-obsidian, selfire1/BibleGateway-to-Obsidian, gslogimaker/my-bible,
  echokos/logseq-berean-standard-bible, Evan Travers "Connected Bible Study in Markdown",
  Biblically Connected (Joschua), faithbasedproductivity.com (backlink-granularity).
