+++
title = "Agent contract"
description = "Tracey-tracked rules the AgentRepo + AgentService implementations must hold."
weight = 100
+++

The agent feature is a **multi-provider AI-agent runtime + monitor**.
It tracks long-running coding/reasoning sessions across providers —
HermesAgent (the local-first one), Claude Code, OpenAI Codex,
custom Hermes worker pipelines, anything that emits a structured log
stream while it operates on a codebase. The model is provider-
agnostic at the wire level; provider-specific behaviour lives in
adapter crates that target a stable shape.

Driving use case: someone running 4 agent sessions in parallel —
two HermesAgent runs, one Claude Code session on a different
worktree, one Codex CLI session — wants a single dashboard that
shows live progress, tool calls, file diffs, cost, and lets them
intervene (cancel, message, branch the conversation). Mobile +
desktop + web surfaces all read the same Loro-backed store.

## Reference repos

When designing or implementing the agent feature, consult these
upstream projects for prior art. They are **read-only references**;
do not vendor or fork unless explicitly approved.

| Repo | URL | What to copy |
|---|---|---|
| **CodexMonitor** | <https://github.com/Dimillian/CodexMonitor> | The whole live-run dashboard pattern — log streaming, tool-call rendering, per-run state machine, diff visualization. Closest to our `AgentRun` view UX. MIT licensed. |
| **t3code** | <https://github.com/pingdotgg/t3code> | Wrapper UI patterns for invoking Claude Code from a custom shell. How they pipe IPC, persist sessions, render markdown live. |
| **hermes-desktop** | <https://github.com/fathah/hermes-desktop> | Desktop client patterns for chat-style agent interaction (Tauri / Electron layout, settings UX). |
| **hermes-webui** | <https://github.com/nesquena/hermes-webui> | Background-job dashboard pattern — queue lists, retry semantics, log tailing. Applies to our async-run queue. |

The spec aims at **feature parity with CodexMonitor** for the live-
run view, plus generalization to multiple agent kinds. Hermes-
specific behaviour (queue routing, worker pools) lives under
`agent.hermes.*`; provider-neutral behaviour under `agent.run.*` /
`agent.conversation.*`.

Rules are linked to source via `r[impl <id>]` and `r[verify <id>]`
annotations. Run `cargo xtask tracey-validate` to confirm coverage.

## Run lifecycle

r[agent.run.identity]
Every `AgentRun` has a server-assigned UUIDv4 primary key. `name`
is a short human label ("refactor auth middleware"). `kind`
identifies the agent backend: `claude-code`, `codex`, `hermes`,
`hermes-agent`, `gemini-cli`, `aider`, `cursor-agent`, `mock`. Repo
accepts any string but the UI offers autocomplete from the known
set.

r[agent.run.status-state-machine]
`AgentRun.status` is a closed enum with strict transitions:
- `queued` → `starting` → `running`
- `running` → `paused` (user-initiated) → `running` (resumed)
- `running` → `awaiting-input` (agent blocked on tool approval / question) → `running`
- terminal from any non-terminal: `completed`, `failed`, `cancelled`, `timed-out`
- terminal → no further transitions (a re-run creates a new `AgentRun` with `parent_run_id` linkage)

The service rejects illegal transitions with `InvalidInput`. The
storage layer accepts any string but the dispatcher validates.

r[agent.run.timing-fields]
Runs track `created_at`, `started_at`, `completed_at` (Option),
each a `DateTime<Utc>`. The UI computes elapsed = `now - started_at`
for live runs, `completed_at - started_at` for finished runs.
`queued_at` is implicit (= created_at).

r[agent.run.prompt]
`AgentRun.prompt` is the initial task description the agent received.
Stored as plain text (LWW). The prompt is the canonical input for
reproducing the run via `AgentService.rerun(run_id)`, which clones
the run with a new id and `parent_run_id=original`.

r[agent.run.working-context]
A run is bound to a working context: `worktree_path` (filesystem
path on the host the agent operates in) and an optional
`git_repo_connection_id` referencing a `GitRepoConnection`. The
worktree is the agent's CWD; the connection records the remote
(forgejo / github / etc.) for PR creation, branch policy, etc.

r[agent.run.parent-and-branch]
`AgentRun.parent_run_id` (Option<Uuid>) links to a parent run when
this run is a rerun, fork, or continuation. The UI renders a run
graph: rerun = same prompt different attempt, fork = branched
conversation, continuation = same agent picked up where it left off.

## Log streaming

r[agent.log-line.append-only]
`AgentLogLine` is the per-run event stream. Each row has `id`,
`run_id`, `seq` (u64 monotonic per run), `at` (DateTime), `level`
enum (`debug`, `info`, `warn`, `error`, `tool`, `assistant`, `user`,
`system`), `body` (text). Once written, log lines are immutable;
the repo rejects updates to existing rows.

r[agent.log-line.real-time-subscribe]
The service exposes `subscribe_logs(run_id)` which streams new log
lines via SSE / vox subscription as they arrive. Clients reconnect
with `?since_seq=N` to resume without gaps. The server holds the
last 1000 lines in memory for fast reconnect; older lines come
from the database.

r[agent.log-line.tool-payload]
Lines with `level=tool` carry structured JSON in `body` describing
the tool call: `{name, args, result, duration_ms, status}`. The UI
renders these specially (collapsible tree, diff view for file
edits). The schema is provider-agnostic — adapter crates normalize
provider-native tool shapes to this form. CodexMonitor's tool-row
component is the visual reference.

r[agent.log-line.assistant-streaming]
Lines with `level=assistant` may be appended with partial content
as the model streams tokens. Each appendage is a new row
(`seq` increments) with `streaming=true` until the final row
flips it to `false`. UI concatenates streaming rows by
`message_id` to render the message as it grows.

## Tool calls and approvals

r[agent.tool-call.entity]
A `ToolCall` entity (separate from the log line) captures
structured tool-call records that the UI can sort, filter, and
inspect outside the log stream: `id`, `run_id`, `seq`, `name`
(`Bash`, `Read`, `Edit`, …), `args_json`, `result_json` (Option),
`status` enum (`pending`, `approved`, `denied`, `running`, `ok`,
`error`), `started_at`, `completed_at` (Option),
`approval_required` (bool).

r[agent.tool-call.approval-flow]
When `approval_required=true` and `status=pending`, the run's
status flips to `awaiting-input`. The user approves via
`AgentService.approve_tool(tool_call_id)` which transitions to
`approved` → `running`; deny goes `denied` → run stays
`awaiting-input` until the agent provides an alternative or the
user cancels the run.

r[agent.tool-call.file-edit-diff]
For tools that modify files (`Edit`, `Write`, `NotebookEdit`), the
`args_json` includes `{path, before, after}` so the UI can render a
diff inline without re-reading the file. `result_json` includes
the post-write hash to detect divergence. Matches CodexMonitor's
diff rendering.

## Conversations

r[agent.conversation.entity]
`AgentConversation` is a chat-style thread separate from `AgentRun`.
A conversation lives over multiple runs (the user keeps prompting,
the agent keeps acting), or it can be ad-hoc with no associated
runs. Fields: `id`, `title`, `system_prompt`, `model_id`,
`pinned`, `archived_at` (Option), `created_at`, `updated_at`.

r[agent.conversation.message]
`ConversationMessage` rows: `id`, `conversation_id`, `seq`,
`role` enum (`user`, `assistant`, `tool`, `system`), `body`
(LoroText for character-level merge across peers), `tool_call_id`
(Option when role=tool), `model_id` (Option — set when role=assistant
to record which model produced it), `created_at`.

r[agent.conversation.run-link]
A `ConversationMessage` can spawn an `AgentRun` (the user asks
"refactor X" → the agent kicks off a run). The link is via
`AgentRun.spawned_from_message_id`. The UI shows the run inline in
the conversation with its live status badge.

## Cost and resource tracking

r[agent.run.token-and-cost]
`AgentRun.input_tokens`, `output_tokens`, `cache_read_tokens`,
`cache_creation_tokens` (all u64). `cost_cents_estimate` (i64) is
the running estimate of dollar cost based on the provider's price
table; recomputed on every log line that includes usage info.

r[agent.run.duration-and-tool-counts]
`AgentRun.tool_call_count` (u32) and `assistant_message_count`
(u32) are denormalized counters maintained by the service for
fast dashboard rendering. Match the row counts in
`tool_calls` / `conversation_messages` filtered by run.

r[agent.run.resource-limits]
A run optionally carries `max_tokens` (u64), `max_tool_calls` (u32),
`max_wall_seconds` (u32). When exceeded the service transitions the
run to `timed-out` and emits a final log line documenting which
limit triggered.

## Live update transport

r[agent.live-update.events]
Three event kinds flow to subscribed clients:
- `RunStateChanged(run_id, new_status)` — emit on every status flip.
- `LogAppended(run_id, log_line)` — emit per new log line.
- `ToolCallChanged(run_id, tool_call_id, new_status)` — emit when a tool moves through its states.

Clients use a vox subscription (or SSE fallback) keyed on
`run_id` or workspace_id. Resumable via `since_seq` per stream.

r[agent.live-update.workspace-scope]
A workspace-level subscription emits events across all runs in the
workspace, so a dashboard can update without N per-run sockets. The
server batches events within 50ms windows for high-volume runs.

## Multi-provider adapters

r[agent.adapter.contract]
Provider adapters (claude-code, codex, hermes, …) implement an
`AgentAdapter` trait: `start(prompt, working_dir) -> RunHandle`,
`subscribe(handle) -> EventStream`, `cancel(handle)`. The trait
normalizes provider-native events into the shared
`AgentLogLine` / `ToolCall` shapes.

r[agent.adapter.transport]
Adapters communicate with their backends via the backend's native
transport: subprocess + stdout/stderr for CLI tools (claude-code,
codex CLI — see t3code's IPC patterns), HTTP/SSE for hosted
(Anthropic API, OpenAI Responses API), and the Hermes job queue
for hermes-agent. Implementation detail is encapsulated; the rest
of the system sees only the normalized events.

r[agent.adapter.hermes-routing]
Hermes adapter routes runs to specific worker pools by `kind`
metadata. The user assigns a pool ("local-gpu", "remote-cpu") on
run creation; the adapter publishes to that pool's queue.
Worker availability surfaces via `AgentService.list_pools()`. The
queue / retry semantics mirror hermes-webui's job dashboard.

## Hermes-specific behaviour

r[agent.hermes.profile]
A `HermesProfile` ties a Hermes account to the workspace: account
ID, queue prefix, default model, default pool, and a sealed
credential reference (stored in the secrets table, not inline).
Multiple profiles allowed (work / personal / studio bot).

r[agent.hermes.scheduled-run]
A Hermes run may be scheduled (cron expression on
`HermesScheduledRun`) instead of one-shot. The service executes
the run on schedule, producing an `AgentRun` row per execution
linked back to the schedule by `scheduled_run_id`. Standard run
events apply.

## Git integration

r[agent.git-repo-connection.shape]
`GitRepoConnection` records remote-side metadata for a git project:
`provider` enum (`github`, `forgejo`, `gitlab`, `bitbucket`,
`gitea`), `owner`, `repo`, `default_branch`, `auth_kind` enum
(`ssh-key`, `https-token`, `gh-app`, `none`), `auth_ref` (opaque
pointer into secrets), `discoverable` (whether the agent should
list issues / PRs).

r[agent.git-repo-connection.pr-creation]
The service exposes `AgentService.open_pull_request(run_id, title,
body, base)`. Pre-conditions: the run must have completed
successfully, the worktree must be ahead of `base`, and the user
must have authorized PR creation (per-connection toggle). The
service handles branch push + provider-specific PR API. Failures
return a structured error including the provider's response.

## Cancellation and interruption

r[agent.service.cancel]
`AgentService.cancel(run_id)` signals the adapter to terminate the
agent process gracefully, then marks the run `cancelled`. Adapters
must honor the cancel within 10 seconds; on timeout the service
escalates to a hard kill and marks the run `cancelled` with a
note. Already-terminal runs are no-ops.

r[agent.service.pause-resume]
`pause(run_id)` / `resume(run_id)` for adapters that support
checkpointing (Claude Code, hermes-agent). Adapters that don't
support pause respond with `InvalidInput`; the UI hides the pause
button for those `kind`s.

## CRDT semantics

r[agent.crdt.run-fields-lww]
`AgentRun` scalar fields are LWW. Concurrent edits across peers
take the most recent commit by Loro clock. Status transitions are
service-mediated to avoid two clients trying to start the same run.

r[agent.crdt.log-append-only]
`AgentLogLine` is append-only — no merge concerns, no edit support
at the repo level. Seq numbers are server-assigned to avoid
collision across peers (the agent process always runs on one host
at a time).

r[agent.crdt.conversation-message-text]
`ConversationMessage.body` is `LoroText` for live character-level
merge while a human is typing into an existing message. Assistant
messages once finalized are effectively immutable but use the
same container shape for uniformity.

## Dashboard UX (CodexMonitor parity)

r[agent.dashboard.run-list]
The dashboard's primary view is a list of runs sortable by:
status (live first, then queued, then terminal by completion
time desc), kind, cost, duration. Filter facets: kind, status,
workspace, model, has-error, awaiting-input. CodexMonitor's
"all sessions" view is the visual target.

r[agent.dashboard.split-detail]
Selecting a run opens a split detail view with three panes:
- Logs (streaming, with level filters)
- Tool calls (sortable table; click a row to expand diff/result)
- Conversation (chat-style alongside, if linked)

Each pane subscribes to its slice of events independently so a
silent pane doesn't block the others.

r[agent.dashboard.notifications]
When a run transitions to a terminal state, awaiting-input, or
crosses a resource limit, the system emits a notification through
the notifications feature. The user configures which transitions
notify per `kind` and per workspace.

## What this spec does NOT cover (yet)

- **Multi-agent orchestration**: agent A spawning agent B with
  message-passing between them. Out of scope until v2 — current
  spec covers single-agent runs with conversation linkage.
- **Sandboxed execution environments**: Docker / Firecracker /
  worktree isolation policy. The `worktree_path` field assumes the
  caller has already chosen the sandbox.
- **Web-only agents** that don't bind to a worktree (research /
  search agents). Modeled as `AgentRun` with `worktree_path=None`
  — works today, but the dashboard's diff/file view degrades to
  empty; that's expected.
- **Audit / compliance log export**: regulated environments need
  immutable signed logs. Future feature, separate spec.
- **Per-org rate limits + billing**: multi-tenant cost capping.
  Out of scope for v1, single-tenant only.
