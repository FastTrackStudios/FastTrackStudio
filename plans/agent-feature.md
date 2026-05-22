# agent feature

LLM-agent integration. Models projects, sessions, threads,
messages, tools, approvals, kanban, and the streaming
`AgentEvent` union over an `#[architect::rpc] trait
AgentService`. Backends slot in as siblings (Hermes
in-process, Codex CLI monitor, Claude CLI bridge, Pi,
custom). Binding crates layer per-feature prompts +
parsers on top (`agent-wiki` drives `wiki-proto`; future
`agent-task` drives the task feature).

Synthesized from three deep dives:
- [hermes-webui](https://github.com/nesquena/hermes-webui)
  — in-process Hermes UI (Python + SSE).
- [CodexMonitor](https://github.com/Dimillian/CodexMonitor)
  — Tauri app that monitors external Codex CLI logs.
- [llm_wiki](https://github.com/nashsu/llm_wiki) — for the
  prompt templates carried in `agent-wiki`.

## Two backend shapes

| Shape              | Example         | `dispatch_turn` semantics                  |
|--------------------|-----------------|--------------------------------------------|
| `InProcess`        | embedded Hermes | Async; backend owns the agent runtime      |
| `ExternalMonitor`  | Codex CLI       | Usually `Unsupported`; events come from logs |
| `CliBridge`        | claude CLI      | Spawns CLI per turn; parses stdout         |
| `Http`             | hosted Hermes / peer Task server | Standard SSE proxy        |

`AgentService` is the same trait for all four; the
implementation varies. UIs and CLIs depend only on the trait.

## Crate plan

```text
features/agent/
├── agent-proto/   ✅ shipped — wire contract
├── agent-wiki/    ✅ shipped — wiki bindings (prompts + parsers)
├── agent-codex/   ⏳ next — Codex backend (vendor CodexMonitor's app_server.rs)
├── agent-hermes/  ⏳ future — in-process Hermes backend
├── agent-task/    ⏳ future — task/kanban bindings
├── agent-cli/     ⏳ future — `task agent ...` CLI subcommands
├── agent-ui/      ⏳ future — Dioxus chat + kanban + diff viewer
└── agent/         ⏳ future — facade re-export
```

> Dropped: `agent-claude-cli`. Claude usage on Task happens
> through the user's editor / CLI, not as an in-app agent
> backend. If that changes later, add it back.

## Shipped: `agent-proto`

Module map (`features/agent/agent-proto/src/`):

| File           | What                                                              |
|----------------|-------------------------------------------------------------------|
| `service.rs`   | `AgentService` trait + architect-emitted client.                  |
| `backend.rs`   | `AgentBackend`, `BackendKind`, `BackendHealth`.                   |
| `profile.rs`   | `Profile`, `Personality`, `ModelConfig`, `ToolsetConfig`, `McpServerSpec`. |
| `project.rs`   | `Project`, `GitContext`, `ProjectSettings`.                       |
| `session.rs`   | `Session`, `SessionStatus`, `SourceTag`, `PendingTurn`, `UsageStats`, `CompressionState`, `WorktreeBacking`, `ComposerDraft`. |
| `message.rs`   | `Message`, `Role`, `ContentBlock` (multimodal: text/image/tool_use/tool_result/code). |
| `tool.rs`      | `ToolCall`, `ToolStatus`, `FileChange`, `CollabRouting`.          |
| `reasoning.rs` | `ReasoningBlock`.                                                 |
| `attachment.rs`| `AttachmentRef`, `Attachment`, `AttachmentKind`.                  |
| `approval.rs`  | `Approval`, `ApprovalKind`, `RiskLevel`, `ApprovalDecision`.      |
| `question.rs`  | `QuestionRequest`, `Question`, `QuestionOption`, `QuestionAnswer`.|
| `kanban.rs`    | `Board`, `BoardView`, `Card`, `CardLink`, `CardComment`, `BoardFilter`. |
| `event.rs`     | `AgentEvent` streaming union.                                     |
| `paths.rs`     | Disk layout for state-keeping backends.                           |
| `error.rs`     | `AgentError`.                                                     |

`AgentService` exposes ~50 methods: backend / profile /
project CRUD, session lifecycle + import-from-external,
turn dispatch + cancel + resume, message + tool + reasoning
+ attachment read, approval + question resolution, full
kanban CRUD, three subscription channels
(`session` / `board` / `global`).

## Shipped: `agent-wiki`

Binding library. Carries the prompt templates ported from
llm_wiki + parser signatures.

| File              | What                                                             |
|-------------------|------------------------------------------------------------------|
| `prompts.rs`      | 10 prompt constants loaded from `templates/*.txt`. `render()` helper for `{key}` substitution. |
| `templates/*.txt` | Verbatim ports of llm_wiki's `src/lib/{ingest,deep-research,lint,sweep-reviews,optimize-research-topic,dedup,vision-caption,output-language}.ts` prompt strings. |
| `parsers.rs`      | Signatures + types for FILE/REVIEW/LINT/JSON parsers. Bodies are `todo!()` until first backend lands. |
| `bridge.rs`       | Orchestration helpers — `run_ingest`, `run_lint`, `run_propose_research`, `run_sweep_reviews`, `run_dedup_detect`, `run_dedup_merge`. Signatures shipped, bodies `todo!()`. |

## Slices

### 1. ✅ Proto + wiki bindings (this commit)

`agent-proto` + `agent-wiki` compile cleanly.

### 2. `agent-codex` — first concrete backend

CodexMonitor already ships **1388 lines of working Rust**
that drive `codex app-server` (the official Codex daemon)
over JSON-RPC stdio. See
`~/Development/research/CodexMonitor/src-tauri/src/backend/app_server.rs`.

Plan:

1. Vendor `app_server.rs` + `events.rs` under
   `features/agent/agent-codex/vendor/` (subtree-style; keep
   upstream attribution).
2. Write `agent-codex/src/lib.rs` that implements
   `agent_proto::AgentService` by:
   - mapping `AgentBackend::kind = ExternalMonitor` ↔
     `CliBridge` (Codex supports both modes — read-only logs
     + active subprocess dispatch).
   - translating CodexMonitor's `AppServerEvent` →
     `agent_proto::AgentEvent`.
   - translating CodexMonitor's `WorkspaceInfo` →
     `agent_proto::Project`, `ThreadSummary` → `Session`,
     `ConversationItem` (8-variant union) → `Message` +
     `ToolCall` + `ReasoningBlock` + `Approval` +
     `QuestionRequest`.
3. `dispatch_turn` works end-to-end (spawn or attach to
   `codex app-server`, send a `start_thread` RPC, stream the
   response events).
4. `import_external_session` reads a Codex log file and
   materializes a `Session` + messages without spawning the
   daemon — useful for archived sessions.

Once shipped, this is the first usable agent backend and
gives us the surface to validate `agent-wiki`'s ingest /
lint / dedup flows end-to-end. **The wiki feature reaches
full llm_wiki parity at the end of this slice** — agent
loops are real, prompts are real, output parsing is real.

### 3. `agent-wiki` parser bodies + bridge wiring

With Codex driving real agent loops, port the strict
parsers from llm_wiki's TypeScript:

- `parse_ingest_blocks` — `---FILE:` / `---REVIEW:` block
  parser; the response is dropped if the first character
  isn't `-`.
- `parse_lint_blocks` — `---LINT: type | severity | title---`
  blocks.
- `parse_dedup_groups`, `parse_sweep_resolved` — strict JSON
  (no markdown fences).
- `parse_research_plan` — exactly-4-line `TOPIC:` + `QUERY:`
  format.

Then wire `bridge::run_ingest` / `run_lint` /
`run_propose_research` / `run_sweep_reviews` /
`run_dedup_*` so a single function call drives one full
pipeline. **At the end of this slice, Task has llm_wiki
parity** — same prompts, same parsers, same flows, against
the same `Wiki/raw/sources/` shape.

### 4. `agent-hermes` (in-process)

The big one. Embeds Hermes runtime (Rust SDK when
available; shell out to the Python entrypoint as fallback).
Streams events via `subscribe_session`. Owns approvals +
questions + kanban end-to-end.

This is where personalities, MCP servers, multi-profile
support, composer drafts, and the run-journal SSE replay
all live. After this slice, Task can act as a full Hermes
WebUI replacement (single-user).

### 5. `agent-cli` (`task agent ...`)

CLI surface — `task agent session list`,
`task agent dispatch <msg>`, `task agent kanban list`,
`task agent ingest <source-path>` (delegates to
`agent_wiki::bridge::run_ingest`).

### 6. `agent-ui` (Dioxus)

Chat view, kanban board, diff viewer for tool changes,
approval dialog, session sidebar. Uses
`subscribe_session` for live updates.

### 7. `agent-task` (binding)

Sister to `agent-wiki` — drives the future task feature's
proto from agent loops. Same shape: prompt templates +
parsers + bridge.

## On-disk layout

Mirrors Hermes:

```text
<state>/agent/
├── backends.json
├── profiles/<id>/
│   ├── config.json
│   ├── personalities/
│   └── secrets.enc
├── projects.json
├── sessions/<session-id>.json
├── messages/<session-id>/<message-id>.json
├── attachments/<sha256>
├── tools/<session-id>/<tool-call-id>.json
├── approvals/<session-id>/<approval-id>.json
├── questions/<session-id>/<request-id>.json
├── boards/<board-id>.json
├── boards/cards/<card-id>.json
├── boards/links.json
├── boards/comments/<card-id>.json
└── run_journal.sqlite   ← SSE replay for crash recovery
```

Path constants live in
`agent_proto::paths`. Backends are free to swap storage
(SQLite, Sled, remote object store) — paths are just
defaults.

## Open questions

- **Hermes embedding** — does Hermes ship a Rust SDK, or
  do we spawn its Python entrypoint? Affects `agent-hermes`
  shape considerably.
- **Multi-tenancy** — Hermes is single-user. Task may
  eventually want per-user agent state; defer until needed.
- **Tool-use schema** — Anthropic + OpenAI use different
  shapes. We've abstracted to `ContentBlock::ToolUse {
  input_json }`, but backends will need translation layers.
- **Subagent delegation** — `CollabRouting` is in the
  trait but the loop semantics aren't pinned down. First
  Hermes integration will likely shape this.
- **Federation** — peer Task servers exposing
  `AgentService` over HTTP/WS would let agents collaborate
  across vaults. Out of scope until the wiki feature lands
  federation first; same trait can be reused.

## Why this and not a thinner abstraction

A simple `chat(msg) -> stream<event>` would work for one
backend. The full surface buys:

- **Backend pluggability** — Hermes, Codex, Claude CLI all
  fit behind the same trait.
- **External-monitor parity** — read-only backends look
  the same to UIs.
- **Per-feature bindings** — `agent-wiki` (and future
  `agent-task`, etc.) layer cleanly without coupling
  agent-proto to any specific app domain.
- **Reuse with `architect::rpc`** — once we want remote
  agents, the same trait moves over vox without code
  duplication.
