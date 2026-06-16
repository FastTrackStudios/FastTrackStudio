# Knowledge Primitives — typed links · confidence · visibility · publishing

> Status: **design proposal** (2026-06-16), from the research in this session. Generalizes
> beyond the Bible — these are primitives for *any* topic. Grounded in what the repo
> already has (see "Existing hooks"). The Bible is the first heavy user.

## The goal (from the user)

- Link verses ↔ verses, verses ↔ wiki entries (people, places), notes ↔ anything.
- Tag verses topically ("where does the Bible talk about money / pain").
- A **private journal** in the vault that links to everything, kept private.
- **Publish** just the *interconnections* — a shareable graph of verses ↔ ideas ↔ wiki
  entries — **without** publishing private vault content.
- **Quality filters**: separate unstructured thoughts / feelings / opinions from
  facts / research / strongly-established links. Show a graph of only the solid links.

## The model (research-backed — see §sources)

Two first-class things: **nodes** and **typed links**. The field has converged on
**three orthogonal axes** for nodes (Gwern): don't conflate "how finished" with "how
sure" with "how important."

### Node (a vault note, wiki page, verse, entity, …)
| field | values | borrowed from |
|---|---|---|
| `maturity` | `seedling` → `budding` → `evergreen` | Maggie Appleton digital-garden growth stages |
| `confidence` | `certain > likely > possible > unlikely > speculative` (ordinal) | Gwern / Kesselman estimative words |
| `visibility` | `private` (default) → `unlisted` → `public` | Quartz ExplicitPublish (opt-in is the safe default) |
| `importance` | `0–10` (optional) | Gwern |
| `last_reviewed` | date | Matuschak (staleness is a liability when published) |

This is exactly the user's "thoughts/feelings/opinions vs facts/proven": `confidence` +
`maturity` carry it, and `visibility` gates publishing.

### Typed link (a FIRST-CLASS object, not just an edge — the key insight)
Nanopublication / RDF-star lesson: a link carries its *own* confidence, visibility, and
provenance, so you can filter and publish at link granularity.
```
{ source, target, relation, confidence, visibility, provenance{ source_ref?, created_by, created_at, derived } }
```
**Relation vocabulary** (directional, single-token, named inverses, extensible):
- topic (SKOS): `broader`⇄`narrower`, `related`
- structure (Breadcrumbs): `up`⇄`down`, `next`⇄`prev`
- definitional: `defines`, `instance-of`⇄`has-instance`, `example-of`
- epistemic (argument mapping): `supports`⇄`supported-by`, `refutes`⇄`refuted-by`, `cites`/`source-for`
- scripture-specific: `cross-ref` (verse↔verse), `fulfills`/`quotes`, `mentions` (verse→entity), `tagged` (verse→topic)

### Publishing
- **Opt-in** (private by default). Publishing exports nodes with `visibility ≥ unlisted`
  **plus their links** whose `visibility` allows it.
- **Public → private link policy** (declared at publish time): `redact` (strip href, keep
  text — default, avoids leaking private titles) | `drop` | `stub`. Lint warns on any
  public node linking a private target.
- The published artifact is a **graph of interconnections** (verse↔verse, verse↔entity,
  idea↔idea) filtered by confidence/maturity — exactly the user's ask.

### Reader-facing quality filters (the differentiator — nobody ships this)
A confidence slider ("only ≥ likely"), maturity badges, relation-type toggles ("only
typed/epistemic edges, hide loose wikilinks"). The data model above is what enables it.

## Existing hooks in this repo (lift, don't reinvent)

The local survey found the model already exists for *code* — the job is to raise it to
notes/verses and add visibility:
- **`features/wiki/wiki-graph/src/code_extract.rs`** already has `Relation {Defines,Calls,
  Imports,Implements}` + `Confidence {Extracted,Inferred,Ambiguous}` per edge. Generalize
  these enums.
- **`features/wiki/wiki-proto/src/graph.rs`** `GraphEdge`/`GraphNode` — add `relation` +
  `confidence` fields.
- **`features/vault/vault-live/src/property_schema.rs`** `PropertyType::EnumWithMetadata`
  — model `confidence`/`visibility`/`maturity` as page properties (with colors/icons).
- **`features/view/view-knowledge-graph/src/filters.rs`** `GraphFilterState` already prunes
  by kind/node — add confidence/relation/visibility filters here.
- **`features/wiki/wiki-graph`** 4-signal relevance scorer + Louvain communities — built;
  feeds the published graph.
- **Federation** (`wiki-proto/federation.rs`, `plans/federated-task-platform.md`) +
  archived `plans/archived/vault-publisher.md` (Quartz-style static export) — the publish
  hooks. Add the visibility filter to the export path.
- **Layered model** already enforced: `vault/` → `wiki/Knowledge/` (curated, self-contained
  link-in target) → `resources/`. Maps onto private-journal → publishable-facts.

## Bible-specific data to bundle (all CC BY / PD — research §1)

These populate the link/tag graph with authoritative data (the resource-library pattern):
- **Cross-references** → OpenBible.info `cross-references.txt` (CC BY, ~340k verse↔verse,
  **signed votes** — negative = bad link, so confidence falls out of the data). Bundle to
  `<org>/resources/crossref/`. Optional: raw TSK SWORD for phrase anchors.
- **Topical tags** → OpenBible.info `topic-votes.txt` + `topic-scores.txt` (CC BY, **weighted**)
  as primary; Nave's Topical (BradyStephenson CSV, CC BY) for a PD taxonomy. → `resources/topics/`.
- **Entities** (people/places → verses, genealogy, geo) → STEPBible **TIPNR** (CC BY,
  stable `uStrong` ids). → `resources/entities/`. Seeds wiki entity pages
  (`mentions` links from verses).
- Join key: a single canonical verse id (we have `VerseId` OSIS + BBCCCVVV) — normalize
  all sources to it. The vote/score columns become link `confidence`.

## Build order (proposed)

1. **Bible link/tag data** — bundle OpenBible cross-refs + topics + TIPNR entities into the
   resource library; expose `cross_refs(verse)`, `topics(verse)`, `verses_for_topic`,
   `entity(verse)` via `ScriptureService`. Concrete, authoritative, weighted (→ confidence).
2. **Generalized typed-link primitive** — lift `Relation`/`Confidence`, add `visibility`;
   a link is a first-class object in the vault/wiki graph. Page properties for
   confidence/visibility/maturity.
3. **Quality-filtered graph view** — extend `GraphFilterState` with confidence/relation/
   visibility; the verse↔verse↔entity graph.
4. **Publishing** — visibility-filtered export (extend vault-publisher / federation), with
   the public→private redaction policy.

## Open design questions (need the user)

- Build order: **Bible data first** (1) then generalize, or **generalized primitives first**?
- Confirm **opt-in publishing** (private by default) — research strongly recommends it.
- Adopt the proposed **vocabularies** (maturity/confidence/visibility + relation set) as-is?
- Where typed links live: extend the **wiki `GraphEdge`** model, or a dedicated
  `links` feature trio that the vault/wiki/scripture all write into? (Leaning: a small
  shared `links` feature so verses, notes, and wiki entries all use one typed-link store.)

## Sources
OpenBible.info (topics + cross-refs, CC BY, weighted); STEPBible TIPNR (CC BY); Nave's
(BradyStephenson, CC BY). Gwern epistemic axes; Maggie Appleton digital gardens; Andy
Matuschak evergreen notes; Breadcrumbs/Juggl typed links; SKOS; argument mapping;
nanopublications / RDF-star; W3C Web Annotation; Quartz/Obsidian Publish/Logseq.
