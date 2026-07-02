# Agent Guide

Canonical instructions for AI agents working on this repository. Read this first.

This project is **Local-first, Realtime, Collaborative, Multiplayer, Extensible** — a workspace of feature crates built on Rust + Dioxus + Loro CRDTs + the `architect` framework. See `README.md` for the product story; this document covers the *how*.

---

## TL;DR — what to do in the first 60 seconds

1. **Read `README.md`** for the project's identity and UI rules.
2. **Skim `plans/`** — any open plans here are the next-arc roadmap. Don't duplicate work.
3. **Survey `features/`** to see what already exists before adding anything.
4. **Run `just check`** (or `cargo check --workspace`) to confirm a clean baseline before making changes.
5. **Match patterns**. Every feature trio is shaped identically; copy from the most recent one (currently `features/knowledge/`).

---

## Architecture in 90 seconds

```
features/<name>/
  <name>-proto/    architect-derive wire types (#[derive(Entity)])
  <name>-crdt/     EntityCrdt impl + <Name>RepoLoro newtype (Loro source of truth)
  <name>-db/       SeaORM persistence (crdt-seaorm tables + projections)
  <name>-ui/       dumb Dioxus components — fts-ui only, theme-aware
  <name>/          facade crate with feature gates (vox / server / fake / full)
  spec/<name>.md   tracey spec rules (when present)
  tests/native/    Repo trait + replica-convergence tests

apps/server         WebSocket sync relay, webhook receivers, integration registry boot
apps/web/desktop    Dioxus platform launchers; thin shells over task-ui
apps/db             standalone migrator + seeder

crates/task-ui      Dioxus app shell, AppShell/Sidebar/router, per-feature routes
crates/task-cli     CLI commands
```

**Data flow**: every domain entity lives in a Loro CRDT document. UI components are *dumb* — data in via props, events out via `EventHandler<T>`. The route layer in `task-ui` mounts repos and threads them into components. WebSocket relay propagates Loro doc updates across peers; SeaORM stores opaque snapshots + update log.

**External integrations** sit behind trait seams in `agent-proto`:
- `AgentIntegration` (`hermes`, `mock`) for task dispatch + agent runs
- `ChatModel` (`mock`, future Anthropic/OpenAI/Ollama) for conversational completion
- GitHub webhooks for PR↔task linking
- CalDAV bidirectional sync for the calendar feature

Auth (`features/auth/`) is the only server-authoritative state — everything else is local-first.

---

## Hard rules

These are non-negotiable. Violating them will require rework.

### UI rules

1. **fts-ui primitives only**. Don't hand-roll Buttons / Cards / Sheets / Dialogs / Comboboxes / Sidebars. If a primitive doesn't exist or is missing a feature, fix it in the upstream sibling repo at `../FastTrackStudio/fts-ui/` (the workspace dep is a path checkout — edits propagate immediately).

2. **Theme tokens, never hex**. `bg-background` / `text-foreground` / `bg-card` / `border-border` / `bg-primary` / `text-muted-foreground` / `bg-muted` / `bg-accent`. Never `bg-slate-900` or `#1a1a1a`. The CSS variables flip per dark/light mode and per org/project theme override — a component that hardcodes color breaks every theme preset.

3. **Dark mode is the default**. Both modes must look correct with no `dark:` overrides. If you need a different palette per mode, that's a theme token concern, not a component concern.

4. **Two-tier theming**. Organization picks a preset; project can override. `fts_ui::ThemeProvider` wraps the App root; `ThemeScope` wraps a project route's content when override active. Don't bypass — read from / write through the contexts in `crates/task-ui/src/theming.rs`.

5. **Dumb components**. Feature `*-ui` crates own no state. Data in via props, events out via `EventHandler<T>`. The route is where signals + repos live. This keeps components portable across web/desktop/mobile and reusable in storybooks.

6. **No `.to_string()` on Dioxus prop literals**. Props accepting `Into<String>` take `&str` directly in Dioxus 0.7. `class: "foo bar"`, not `class: "foo bar".to_string()`. Same for ternary arms. The cleanup pass in commit `625c3d1` ripped out 259 of these — don't reintroduce.

### Feature rules

1. **Every domain is a self-contained trio** (proto / crdt / db) plus an optional ui crate and a facade. Adding a new feature is mechanical; don't invent new layouts.

2. **Loro is the source of truth**. Never write directly to SeaORM tables for domain data — that desyncs every connected client. The only DB-direct writes are server-private (`webhook_inbox`, `integration_state`, `git_repo_connections`, the auth schema).

3. **Cross-feature references go through proto types**. A `Task.agent_run_id: Option<Uuid>` is fine; a `Task` holding an `AgentRun` value type is not. Each feature's proto stays a strict acyclic leaf.

4. **Embeddable knowledge**. For rich text on any entity (Task notes, Project descriptions), use `knowledge_proto::shadow_page_id("kind", entity_id)` to derive a deterministic Page UUID. Drop `OutlinerEmbed { page_id: shadow_id, ... }` in the detail sheet. No proto migration needed on the consumer side.

### Engineering rules

1. **`cargo check` before claiming done**. Both native and `cargo check -p task-app-web --target wasm32-unknown-unknown`. Warnings ok, errors aren't.

2. **server-only crates must not leak into the wasm graph**. `apps/server`, `knowledge-sync`, `agent-hermes` are native-only. If `cargo check -p task-app-web --target wasm32-unknown-unknown` starts pulling them in, something is misconfigured.

3. **Demo data only**. Anything seeded by `apps/db/src/seed.rs` is throwaway — feel free to break wire compatibility when iterating schemas. No backwards-compat shims required.

4. **Don't fabricate APIs**. Read the upstream source first: `~/Development/FastTrackStudio/fts-ui/crates/fts-ui/src/prelude.rs` for fts-ui exports, `../architect/macros/architect/` for the entity macro, `../architect/libs/crdt/src/codec.rs` for codec helpers. The actual function signatures matter more than what's "obvious".

5. **Server-only code uses `#[cfg(not(target_arch = "wasm32"))]` on the lib.rs**. See `features/knowledge/knowledge-sync/src/lib.rs` for the pattern.

---

## Common gotchas (learned the hard way)

These keep tripping up new agents. Read them before writing your first line.

### fts-ui

- `StatusBadgeVariant` is **`Success / Warning / Danger / Neutral` only**. No `Error`, no `Info`. Map carefully.
- `ButtonVariant` has **no `Default`**. Use `Primary`.
- `ButtonSize` has **no `Default`**. Use `Medium`.
- `PopoverContent` and `DropdownContent` take a `side` prop (`top` / `right` / `bottom` / `left`). For triggers near the bottom of a scroll container (e.g. sidebar footer), use `side="top"` or the menu falls below the viewport. We fixed the positioning math upstream — `data-side` now produces correct anchor classes.
- `Slider`'s value is `Signal<f64>` only — no `on_change` callback. Use the signal directly.
- `Textarea` has **no `on_keydown` prop**. Workarounds: a sibling `tabindex=-1` capture div, or upstream a prop.
- `Combobox` / `ComboboxTrigger` don't take rich children; trigger renders the raw value. Render extras (badges, icons) as siblings.
- `Checkbox` and `Switch` need `Signal<bool>`, not `bool`. Inside a `for` loop, materialize with `use_signal` per row.

### Lucide icons (re-exported via `fts_ui::lucide_dioxus::*`)

- `CircleCheck` — not `CheckCircle2`
- `TriangleAlert` — not `AlertTriangle`
- `House` — not `Home`
- `Ellipsis` — not `MoreHorizontal`
- `CircleStop` — for the stop button
- `CornerDownLeft` / `SendHorizontal` — for the send button
- `BookOpen` — knowledge route icon
- Icon size is `usize`, not `u32` or `i32`.

### Dioxus 0.7

- Use `use_signal(|| init)` for state. Inside `use_effect`, `.read()` subscribes — use `.peek()` to read without subscribing (critical when avoiding update-loops between bridging effects).
- `spawn(...)` ties the task to the current component's scope — a task spawned from inside a sheet/dropdown/modal that closes (unmounts) on selection is **silently cancelled mid-flight**. For fire-and-forget actions triggered from self-closing UI, use `dioxus::dioxus_core::spawn_forever` (root-scoped), ideally behind a shared `*_detached` method so every surface gets the same behavior (see `AuthCtx::switch_account_detached` — the mobile account-sheet bug).
- `Event<FormData>::value()` on contenteditable returns `textContent` of the element. Anything rendered inside that contenteditable becomes part of `value()`. Keep non-editable chrome (list bullets, heading hash, blockquote `>`) OUTSIDE the contenteditable as a flex sibling — otherwise every keystroke duplicates the prefix.
- `use_memo(use_reactive!(...))` for derived values that should re-run when inputs change.
- Raw strings with embedded `"#` need `r##"..."##` (we have this in `knowledge-proto::canvas::tests`).
- `wasm-split` is experimental and requires `lto = true` + `debug = true`. Disabled by default; the dev profile uses `debug = false` + `strip = "debuginfo"` to keep the bundle ~26 MB.

### Loro CRDT

- `Block.content`, `Message.body`, and similar text fields are currently stored as plain string LWW per-write. This is **wrong** for true collaborative editing — concurrent edits lose data. See `plans/loro-text-editor-upgrade.md` for the planned fix.
- The codec helpers in `crdt::codec` cover `read_str` / `write_str` / `read_bool` / `read_uuid` / `read_dt` / `read_string_list` etc. — for `Option<i32>` and similar gaps, define a local helper inside your `*-crdt` crate that sentinel-encodes through `write_i64`.
- Cross-feature codec gotcha: when reading optional fields that may be absent in pre-extension snapshots, use `read_opt_*(...).unwrap_or(None)` so old data decodes cleanly.
- Loro `LoroValue` doesn't natively store `NaiveDate` — keep `journal_day` and similar as `Option<String>` (ISO `YYYY-MM-DD`).

### Build / dev shell

- This repo uses **direnv** for the `.#ui` dev shell. Recipes call `cargo` / `dx` directly — no `nix develop` wrapping. On hosts without direnv, prefix recipes with `nix develop .#ui --command just <recipe>`.
- The Hermes dashboard at `hermes.starcommand.live` rejects external Host headers; SSH-tunnel to `localhost:9119` for live integration testing (documented in `IntegrationSettings`).

### Proto changes require a task-server rebuild (schema skew)

Changing any `*-proto` crate changes vox method ids (they hash the
method's name + payload shapes), so a **running `task-server` built
before the change can't talk to freshly built clients** — the failure
mode is opaque `structural mismatch` / `InvalidPayload` / `Unknown
method` errors. The rule: **after touching a `*-proto`, rebuild +
restart task-server before trusting any live behavior.**

The guard: the server publishes per-service schema stamps in
`/.well-known/task-server.json` (`schema_stamps`, see
`task_server::schema_stamps` / `org_proto::schema_stamp`). Run
`task doctor` to compare your CLI build against the running server
(exits non-zero on mismatch); ui-lab's `pnpm smoke` does the same for
the generated TS bundle and downgrades skew-shaped failures to loud
`SKEW SKIP` warnings instead of red herrings.

### Mixed-target cargo check false alarms

When you see `error: This wasm target is unsupported by mio` while running `cargo check -p task-server -p task-app-web --target wasm32-unknown-unknown`, that's **not a real error** — it's mio (a native-only dep of `task-server`) being asked to compile for wasm. Check them separately:
```bash
cargo check -p task-server                                            # native
cargo check -p task-app-web --target wasm32-unknown-unknown           # wasm
```

### rust-analyzer staleness

rust-analyzer often reports "file not found" / "missing field" diagnostics that don't reflect actual compile state. **Trust `cargo check`, not the LSP**. Restart rust-analyzer (Ctrl+Shift+P → "rust-analyzer: Restart Server") to clear stale views after large file moves.

---

## Workflow

### Multi-step features

For anything bigger than one file, use the **research → plan → build → wire** pattern:

1. **Research agent** (with WebSearch/WebFetch + local repo reads) produces a concrete build spec — entities, components, interactions, file paths, exact prop shapes.
2. **Schema phase** (single agent, sequential) lands proto + crdt + db + seed data + tests.
3. **UI phase** (parallel agents) build dumb components. Often split: editor / canvas / surrounding-views.
4. **Wire phase** (you, integration) mounts the route, embeds in other features, verifies.

Past arcs that followed this pattern: knowledge, agent+git+Hermes, chat+Hermes-webui, project-management. Their commit messages are good references.

### Use subagents for parallel + breadth work

- 4 parallel agents wrote the per-feature dashboards in one pass (commit `e749ec8`).
- A research agent + parallel UI agents shipped the agent+git+Hermes arc (commit `c2974b8`).
- Don't run an agent for one-line changes; do those directly.

### Plans live in `plans/`

Open architectural follow-ups go in `plans/<topic>.md`. Each plan has: status, scope, background, what changes, sequencing into phases, acceptance criteria, risk register. See `plans/loro-text-editor-upgrade.md` for the canonical shape.

### Commit messages

Heredoc style, descriptive subject + body grouped by phase or layer. See `git log --oneline` for the prevailing format. Every arc commit includes:
- What landed per phase
- Verify status (cargo check / test results)
- v1 limitations documented in-line

### When changing fts-ui

The workspace dep is `path = "../FastTrackStudio/fts-ui/crates/fts-ui"`. Edits propagate on next `cargo check`. Commit the fts-ui change in its own repo (separate from Task-architect commits). Keep the API additive when possible — many features depend on the prelude shape.

---

## Tracking

This project does NOT use bd/beads or any external ticket tracker. The conventions we actually rely on:

- **`plans/<topic>.md`** — open architectural follow-ups. Each plan carries status, scope, background, what changes, sequenced phases, acceptance criteria, risk register. See `plans/loro-text-editor-upgrade.md` for the canonical shape. When you spot work that's bigger than a one-file fix, write the plan first.

- **Commit messages** — the activity log. Heredoc-style, descriptive subject + phase-grouped body. `git log --oneline` is your worklog. Past arcs (see commits `e749ec8` ui adoption, `c2974b8` agent+git, `66fe21d` chat+Hermes-webui, `b527715` knowledge) follow this shape — copy them.

- **In-line FUTURE comments** in code for narrow follow-ups too small to be a plan. Pattern: `// FUTURE: <one-line description of what's missing>`. Visible via `rg -t rust 'FUTURE:'`.

- **README.md / AGENTS.md** — slowly-changing canonical guidance. Update when conventions shift.

Don't introduce new tracking systems (markdown TODO files, scratchpads, GitHub Issues without explicit user request). The combination above has handled every multi-phase arc cleanly.

---

## Landing the Plane (Session Completion)

**When ending a work session**, complete ALL steps below.

1. **Write up loose ends**:
   - Big architectural follow-ups → new file in `plans/<topic>.md`
   - Narrow gaps → `// FUTURE:` comment at the call site
2. **Run quality gates** if code changed:
   - `cargo check -p task-ui` clean
   - `cargo check -p task-app-web --target wasm32-unknown-unknown` clean
   - `cargo test -p <touched-crate>` clean
3. **Update issue status** — close finished work, update in-progress items.
4. **Commit** — heredoc-style descriptive message; see `git log --oneline` for the format.
5. **Push** (if the user asked you to):
   ```bash
   git pull --rebase
   git push
   git status  # MUST show "up to date with origin"
   ```
6. **Clean up** — clear stashes, prune remote branches.
7. **Hand off** — write a short summary of state for the next session.

**Critical**: only push when explicitly authorized. Code review > impatience.

---

## Non-Interactive Shell Commands

**ALWAYS use non-interactive flags** with file operations to avoid hanging on confirmation prompts. Shell commands like `cp`, `mv`, and `rm` may be aliased to `-i` mode on some systems.

```bash
# Force overwrite without prompting
cp -f source dest           # NOT: cp source dest
mv -f source dest           # NOT: mv source dest
rm -f file                  # NOT: rm file

# For recursive operations
rm -rf directory            # NOT: rm -r directory
cp -rf source dest          # NOT: cp -r source dest
```

**Other commands that may prompt:**
- `scp` - use `-o BatchMode=yes`
- `ssh` - use `-o BatchMode=yes`
- `apt-get` - use `-y`
- `brew` - use `HOMEBREW_NO_AUTO_UPDATE=1`

---

## Where things live

| Need | Path |
|---|---|
| Add a new feature | `xtask new-feature <name>` (scaffolder) or copy `features/knowledge/` |
| fts-ui prelude reference | `~/Development/FastTrackStudio/fts-ui/crates/fts-ui/src/prelude.rs` |
| architect entity macro | `../architect/macros/architect/` |
| crdt codec helpers | `../architect/libs/crdt/src/codec.rs` |
| App shell + router | `crates/task-ui/src/app.rs` |
| Theming contexts | `crates/task-ui/src/theming.rs` |
| Per-feature routes | `crates/task-ui/src/feature_routes/<name>.rs` |
| Server entry + integrations | `apps/server/src/main.rs`, `apps/server/src/lib.rs` |
| Seed data | `apps/db/src/seed.rs` |
| Open plans / next-arc roadmap | `plans/*.md` |
| Research checkouts (read-only) | `~/Development/research/{logseq,obsidian-api,obsidian-developer-docs,obsidian-sample-plugin}` |
| Hermes deployment | `~/.starcommand/modules/selfhost/services/` — Hermes runs on `hermes.starcommand.live` (Host-rejected; SSH-tunnel to `localhost:9119`) |

---

## When in doubt

1. Match the most recent precedent. Last-shipped feature is `knowledge/`; before that was `agent + git + Hermes`. Both have heavily-commented protos and route handlers.
2. Don't invent — search. `rg -t rust "thing_you're_looking_for"` is your friend.
3. Trust `cargo check`, not rust-analyzer. Native and wasm32 are separate; check both.
4. When stuck, write the plan first in `plans/` and ask the user to review before launching agents.
