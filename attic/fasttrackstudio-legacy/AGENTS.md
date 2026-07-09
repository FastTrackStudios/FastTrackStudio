# Agent Instructions

This project uses **bd** (beads) for issue tracking. Run `bd onboard` to get started.

## Architecture Guides (READ FIRST)

Before writing code, review the relevant guides in `docs/`:

| Guide | When to Read |
|-------|-------------|
| [`docs/crate-facade-pattern.md`](docs/crate-facade-pattern.md) | Adding dependencies, creating new crates, or refactoring modules. Internal crates are private — always use the facade. |
| [`docs/roam-best-practices.md`](docs/roam-best-practices.md) | Working with RPC services, async patterns, streaming, locks. Based on [bearcove/ship](https://github.com/bearcove/ship) and [bearcove/moire](https://github.com/bearcove/moire). |
| [`docs/facet-guide.md`](docs/facet-guide.md) | Deriving `Facet`, serialization formats, roam type requirements. Based on [facet-rs/facet](https://github.com/facet-rs/facet). |
| [`docs/styx-guide.md`](docs/styx-guide.md) | Writing `.styx` config files. Based on [bearcove/styx](https://github.com/bearcove/styx). Use the `/styx` skill for interactive help. |
| [`docs/tracey-guide.md`](docs/tracey-guide.md) | Adding spec annotations (`r[impl ...]`, `r[verify ...]`), checking requirement coverage. Based on [bearcove/tracey](https://github.com/bearcove/tracey). Use the `/tracey` skill for interactive help. |

### Critical Rules

- **Facade crates only** — apps depend on `signal`, `daw`, `session`, never on internal crates like `signal-proto` or `daw-proto`
- **moire over tokio** — use `moire::task::spawn`, `moire::sync::Mutex`, etc. for instrumentation
- **No `Result<T,E>` in `#[roam::service]` traits** — use response enums
- **Never hold std locks across `.await`** — clone data out first
- **Name everything** — spawned tasks, channels, locks all need names for dashboard visibility

## Quick Reference

```bash
bd ready              # Find available work
bd show <id>          # View issue details
bd update <id> --claim  # Claim work
bd close <id>         # Complete work
bd dolt push          # Sync beads to remote
```

## Landing the Plane (Session Completion)

**When ending a work session**, you MUST complete ALL steps below. Work is NOT complete until `git push` succeeds.

**MANDATORY WORKFLOW:**

1. **File issues for remaining work** - Create issues for anything that needs follow-up
2. **Run quality gates** (if code changed) - Tests, linters, builds
3. **Update issue status** - Close finished work, update in-progress items
4. **PUSH TO REMOTE** - This is MANDATORY:
   ```bash
   git pull --rebase
   bd sync
   git push
   git status  # MUST show "up to date with origin"
   ```
5. **Clean up** - Clear stashes, prune remote branches
6. **Verify** - All changes committed AND pushed
7. **Hand off** - Provide context for next session

**CRITICAL RULES:**
- Work is NOT complete until `git push` succeeds
- NEVER stop before pushing - that leaves work stranded locally
- NEVER say "ready to push when you are" - YOU must push
- If push fails, resolve and retry until it succeeds

Use 'bd' for task tracking

## btca — Source Code Search

Use **btca** to query the actual source code of key dependencies before implementing features or debugging. Prefer this over web searches or docs that may be outdated.

```bash
btca ask -r <resource> -q "your question"
btca ask -r facet -r roam -q "How does roam use facet for serialization?"
btca resources   # list all available resources
```

### Relevant Resources for This Repo

| Resource | Repo | Description |
|----------|------|-------------|
| `facet` | facet-rs/facet | Rust reflection — shapes, derive macros, serialization, pretty-printing |
| `roam` | bearcove/roam | Rust-native RPC framework where Rust traits are the schema, with TS/Swift codegen |
| `tracey` | bearcove/tracey | Traceability tool linking requirements/specs to code implementations via annotations |
| `dodeca` | bearcove/dodeca | Fully incremental static site generator with query-based caching |
| `capn` | bearcove/capn | Dev automation via git pre-commit/pre-push hooks — formatting, validation, README gen |
| `peeps` | bearcove/peeps | Low-overhead instrumentation for tokio tasks, threads, locks, and RPC calls |
| `styx` | bearcove/styx | Data serialization format — cleaner alternative to JSON/YAML with schema support |
| `figue` | bearcove/figue | Config parsing from CLI args, env vars, and config files using facet reflection |

<!-- BEGIN BEADS INTEGRATION v:1 profile:full hash:d4f96305 -->
## Issue Tracking with bd (beads)

**IMPORTANT**: This project uses **bd (beads)** for ALL issue tracking. Do NOT use markdown TODOs, task lists, or other tracking methods.

### Why bd?

- Dependency-aware: Track blockers and relationships between issues
- Git-friendly: Dolt-powered version control with native sync
- Agent-optimized: JSON output, ready work detection, discovered-from links
- Prevents duplicate tracking systems and confusion

### Quick Start

**Check for ready work:**

```bash
bd ready --json
```

**Create new issues:**

```bash
bd create "Issue title" --description="Detailed context" -t bug|feature|task -p 0-4 --json
bd create "Issue title" --description="What this issue is about" -p 1 --deps discovered-from:bd-123 --json
```

**Claim and update:**

```bash
bd update <id> --claim --json
bd update bd-42 --priority 1 --json
```

**Complete work:**

```bash
bd close bd-42 --reason "Completed" --json
```

### Issue Types

- `bug` - Something broken
- `feature` - New functionality
- `task` - Work item (tests, docs, refactoring)
- `epic` - Large feature with subtasks
- `chore` - Maintenance (dependencies, tooling)

### Priorities

- `0` - Critical (security, data loss, broken builds)
- `1` - High (major features, important bugs)
- `2` - Medium (default, nice-to-have)
- `3` - Low (polish, optimization)
- `4` - Backlog (future ideas)

### Workflow for AI Agents

1. **Check ready work**: `bd ready` shows unblocked issues
2. **Claim your task atomically**: `bd update <id> --claim`
3. **Work on it**: Implement, test, document
4. **Discover new work?** Create linked issue:
   - `bd create "Found bug" --description="Details about what was found" -p 1 --deps discovered-from:<parent-id>`
5. **Complete**: `bd close <id> --reason "Done"`

### Auto-Sync

bd automatically syncs via Dolt:

- Each write auto-commits to Dolt history
- Use `bd dolt push`/`bd dolt pull` for remote sync
- No manual export/import needed!

### Important Rules

- ✅ Use bd for ALL task tracking
- ✅ Always use `--json` flag for programmatic use
- ✅ Link discovered work with `discovered-from` dependencies
- ✅ Check `bd ready` before asking "what should I work on?"
- ❌ Do NOT create markdown TODO lists
- ❌ Do NOT use external issue trackers
- ❌ Do NOT duplicate tracking systems

For more details, see README.md and docs/QUICKSTART.md.

## Landing the Plane (Session Completion)

**When ending a work session**, you MUST complete ALL steps below. Work is NOT complete until `git push` succeeds.

**MANDATORY WORKFLOW:**

1. **File issues for remaining work** - Create issues for anything that needs follow-up
2. **Run quality gates** (if code changed) - Tests, linters, builds
3. **Update issue status** - Close finished work, update in-progress items
4. **PUSH TO REMOTE** - This is MANDATORY:
   ```bash
   git pull --rebase
   bd dolt push
   git push
   git status  # MUST show "up to date with origin"
   ```
5. **Clean up** - Clear stashes, prune remote branches
6. **Verify** - All changes committed AND pushed
7. **Hand off** - Provide context for next session

**CRITICAL RULES:**
- Work is NOT complete until `git push` succeeds
- NEVER stop before pushing - that leaves work stranded locally
- NEVER say "ready to push when you are" - YOU must push
- If push fails, resolve and retry until it succeeds

<!-- END BEADS INTEGRATION -->
