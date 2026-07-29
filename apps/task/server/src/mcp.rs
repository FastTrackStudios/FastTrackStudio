//! MCP server — Task as a tool surface for any agent.
//!
//! Mounted at `POST /org/{slug}/mcp` (MCP Streamable HTTP transport,
//! JSON-RPC 2.0 over a single POST; we answer inline rather than
//! upgrading to SSE because every tool here returns promptly).
//!
//! This is the other half of the agent story. `agent_router` lets
//! Task *drive* agents; this lets an agent drive **Task** — create a
//! task, move a calendar event, capture a reminder, read a note —
//! against the same in-process backends the vox layer serves, so an
//! agent's writes are indistinguishable from the UI's.
//!
//! Because it speaks MCP rather than anything Task-specific, the same
//! endpoint serves the Hermes gateway (`mcp_servers:` in its config),
//! Claude Code, Codex, or any other MCP client.
//!
//! **Orientation**: `initialize` returns `instructions`, which MCP
//! clients fold into the agent's system prompt. That's how the agent
//! learns it's inside Task — which org, what today's date is, and the
//! vault's conventions. [`server_instructions`] is that text.
//!
//! **Auth**: `Authorization: Bearer <token>`, accepting either the
//! static `TASK_MCP_TOKEN` or a real architect-auth session token
//! (same rule as [`crate::watch_bridge`]).
//!
//! **Safety**: v1 exposes no deletion. Tasks are completed by status
//! change, events are cancelled explicitly, and captures land as
//! `suggested` for one-tap acceptance unless the caller opts out —
//! an agent should be able to propose freely without polluting a
//! trusted queue.

use axum::{
    Json,
    extract::{Path, State},
    http::{HeaderMap, StatusCode},
    response::{IntoResponse, Response},
};
use serde_json::{Value, json};

use crate::AppState;

/// MCP revisions we can speak. We echo the client's requested
/// version when it's one of these, else answer with the newest —
/// the spec's negotiation rule.
const SUPPORTED_PROTOCOLS: &[&str] = &["2025-06-18", "2025-03-26", "2024-11-05"];
const LATEST_PROTOCOL: &str = "2025-06-18";

/// Cap on rows returned by any list tool. Agent context is the
/// scarce resource — a 400-task vault must not land in one message.
const DEFAULT_LIMIT: usize = 50;
const MAX_LIMIT: usize = 200;

/// Vault id the org's `VaultSync` backend is keyed by (single-vault
/// per org today, same constant the UI passes).
const VAULT_ID: &str = "default";

// ── JSON-RPC envelope ────────────────────────────────────────────

/// JSON-RPC error codes we emit (the standard set plus MCP's use of
/// `-32602` for bad tool arguments).
mod code {
    pub const PARSE: i32 = -32700;
    pub const INVALID_REQUEST: i32 = -32600;
    pub const METHOD_NOT_FOUND: i32 = -32601;
    pub const INVALID_PARAMS: i32 = -32602;
    pub const INTERNAL: i32 = -32603;
}

fn rpc_result(id: Value, result: Value) -> Value {
    json!({ "jsonrpc": "2.0", "id": id, "result": result })
}

fn rpc_error(id: Value, code: i32, message: impl Into<String>) -> Value {
    json!({
        "jsonrpc": "2.0",
        "id": id,
        "error": { "code": code, "message": message.into() },
    })
}

/// A tool's answer. MCP models tool failure as a *successful*
/// response with `isError: true` — the agent is meant to read the
/// message and adapt, not to see a transport error.
fn tool_ok(value: &Value) -> Value {
    json!({
        "content": [{ "type": "text", "text": compact(value) }],
        "isError": false,
    })
}

fn tool_err(message: impl Into<String>) -> Value {
    json!({
        "content": [{ "type": "text", "text": message.into() }],
        "isError": true,
    })
}

/// Tool payloads are JSON the model reads as text; pretty-printing
/// costs tokens for no comprehension gain.
fn compact(v: &Value) -> String {
    serde_json::to_string(v).unwrap_or_else(|_| "null".to_string())
}

// ── Tool catalog ─────────────────────────────────────────────────

/// One exposed tool: MCP name, one-line purpose, and the JSON Schema
/// the client validates arguments against.
pub struct ToolDef {
    pub name: &'static str,
    pub description: &'static str,
    pub schema: fn() -> Value,
}

/// Shorthand for a JSON Schema object.
fn obj(props: Value, required: &[&str]) -> Value {
    json!({
        "type": "object",
        "properties": props,
        "required": required,
        "additionalProperties": false,
    })
}

fn s_(desc: &str) -> Value {
    json!({ "type": "string", "description": desc })
}

fn i_(desc: &str) -> Value {
    json!({ "type": "integer", "description": desc })
}

fn b_(desc: &str) -> Value {
    json!({ "type": "boolean", "description": desc })
}

/// Every tool this server exposes, in the order the agent sees them.
///
/// Descriptions are written *for the model*: they say when to reach
/// for the tool, not merely what it does, because that's what drives
/// correct selection.
#[must_use]
pub fn tool_catalog() -> Vec<ToolDef> {
    vec![
        ToolDef {
            name: "task_context",
            description: "Orient yourself: today's date and time, the org you're working in, \
                          and a count of what's open (tasks due, events today, inbox items). \
                          Call this first when the user's request depends on 'now' or 'what's \
                          on my plate'.",
            schema: || obj(json!({}), &[]),
        },
        ToolDef {
            name: "list_tasks",
            description: "List tasks. Defaults to open tasks only. Filter by status, or by \
                          'due_on_or_before' (ISO YYYY-MM-DD) to answer 'what's due today/this \
                          week'. Returns ids you must pass to update_task.",
            schema: || {
                obj(
                    json!({
                        "status": s_("Exact status to match, e.g. 'open', 'in-progress', 'done'. Omit for all open tasks."),
                        "due_on_or_before": s_("ISO date (YYYY-MM-DD). Keeps tasks due or scheduled on/before it."),
                        "query": s_("Case-insensitive substring match on the title."),
                        "limit": i_("Max rows (default 50, max 200)."),
                    }),
                    &[],
                )
            },
        },
        ToolDef {
            name: "create_task",
            description: "Create a task from natural language. The text is parsed the same way \
                          the app's quick-add is: '#tag', '@context', '[[Project]]', '!high', and \
                          dates like 'tomorrow', 'next monday', '2026-08-01' are extracted, and \
                          the remainder becomes the title. Explicit fields override what the text \
                          implies.",
            schema: || {
                obj(
                    json!({
                        "text": s_("The task in natural language, e.g. 'Email Dana about the invoice tomorrow !high @work'."),
                        "due": s_("Override the due date (ISO YYYY-MM-DD)."),
                        "scheduled": s_("When you plan to work on it (ISO YYYY-MM-DD)."),
                        "priority": s_("'none' | 'low' | 'normal' | 'high' | 'critical'."),
                    }),
                    &["text"],
                )
            },
        },
        ToolDef {
            name: "update_task",
            description: "Change a task by id: mark it done, reschedule it, re-prioritize, or \
                          retitle. Only the fields you pass change. Get ids from list_tasks.",
            schema: || {
                obj(
                    json!({
                        "id": s_("Task UUID from list_tasks."),
                        "status": s_("New status, e.g. 'open', 'in-progress', 'done'."),
                        "title": s_("New title."),
                        "due": s_("New due date (ISO YYYY-MM-DD); pass an empty string to clear."),
                        "scheduled": s_("New scheduled date (ISO YYYY-MM-DD); empty string clears."),
                        "priority": s_("'none' | 'low' | 'normal' | 'high' | 'critical'."),
                    }),
                    &["id"],
                )
            },
        },
        ToolDef {
            name: "list_events",
            description: "List calendar events, optionally windowed by date. Always call this \
                          before rescheduling anything so you're moving real events and can see \
                          what a new time would collide with.",
            schema: || {
                obj(
                    json!({
                        "from": s_("ISO date (YYYY-MM-DD). Keeps events starting on/after it."),
                        "to": s_("ISO date (YYYY-MM-DD). Keeps events starting on/before it."),
                        "limit": i_("Max rows (default 50, max 200)."),
                    }),
                    &[],
                )
            },
        },
        ToolDef {
            name: "create_event",
            description: "Put something on the calendar. Times are RFC-3339 with offset \
                          (e.g. '2026-07-25T14:00:00-05:00'); for an all-day event pass \
                          plain dates and all_day: true.",
            schema: || {
                obj(
                    json!({
                        "title": s_("What the event is."),
                        "start": s_("RFC-3339 start (inclusive), or YYYY-MM-DD when all_day."),
                        "end": s_("RFC-3339 end (exclusive), or YYYY-MM-DD when all_day."),
                        "all_day": b_("True for an all-day event."),
                        "description": s_("Optional notes on the event."),
                    }),
                    &["title", "start", "end"],
                )
            },
        },
        ToolDef {
            name: "reschedule_event",
            description: "Move an existing event to a new time, keeping everything else. This is \
                          the tool for 'rearrange my schedule' — call list_events first, then one \
                          reschedule per event you're moving.",
            schema: || {
                obj(
                    json!({
                        "id": s_("Event id from list_events."),
                        "start": s_("New RFC-3339 start."),
                        "end": s_("New RFC-3339 end."),
                    }),
                    &["id", "start", "end"],
                )
            },
        },
        ToolDef {
            name: "cancel_event",
            description: "Remove an event from the calendar. Confirm with the user before \
                          cancelling anything you didn't create in this conversation.",
            schema: || obj(json!({ "id": s_("Event id from list_events.") }), &["id"]),
        },
        ToolDef {
            name: "capture_inbox",
            description: "Capture a thought, reminder, or follow-up into the inbox — the right \
                          home for anything not yet shaped into a task. Captures land as \
                          'suggested' for the user to accept with one tap, so capture freely; \
                          pass accepted: true only when the user explicitly asked for it.",
            schema: || {
                obj(
                    json!({
                        "body": s_("The note, verbatim. Markdown."),
                        "accepted": b_("Skip the suggestion queue and file it as an open item."),
                        "resurface_on": s_("ISO date to bring it back up (a reminder)."),
                    }),
                    &["body"],
                )
            },
        },
        ToolDef {
            name: "list_inbox",
            description: "What's waiting in the inbox for review today — the user's unprocessed \
                          captures. Useful for 'what did I say I'd look at'. Pass status \
                          'suggested' to see captures (including your own) still awaiting the \
                          user's accept.",
            schema: || {
                obj(
                    json!({
                        "status": s_("Show every item with this status instead of today's review queue, e.g. 'suggested', 'open'."),
                        "limit": i_("Max rows (default 50, max 200)."),
                    }),
                    &[],
                )
            },
        },
        ToolDef {
            name: "list_projects",
            description: "The user's projects. Use it to attach work to the right project, or to \
                          answer 'what am I working on'.",
            schema: || {
                obj(
                    json!({ "limit": i_("Max rows (default 50, max 200).") }),
                    &[],
                )
            },
        },
        ToolDef {
            name: "list_goals",
            description: "The user's goals — the longer horizon above projects. Consult before \
                          advising on priorities.",
            schema: || {
                obj(
                    json!({ "limit": i_("Max rows (default 50, max 200).") }),
                    &[],
                )
            },
        },
        ToolDef {
            name: "search_vault",
            description: "Find notes by path. The vault is the user's own writing — meeting \
                          notes, journals, references. Returns paths to pass to read_note.",
            schema: || {
                obj(
                    json!({
                        "query": s_("Case-insensitive substring matched against the file path."),
                        "limit": i_("Max rows (default 50, max 200)."),
                    }),
                    &["query"],
                )
            },
        },
        ToolDef {
            name: "read_note",
            description: "Read one note's markdown by vault-relative path (from search_vault).",
            schema: || {
                obj(
                    json!({ "path": s_("Vault-relative path, e.g. 'Records/notes/standup.md'.") }),
                    &["path"],
                )
            },
        },
        ToolDef {
            name: "append_note",
            description: "Append markdown to the end of an existing note. Use for adding to a \
                          running log or daily note; it never overwrites what's there.",
            schema: || {
                obj(
                    json!({
                        "path": s_("Vault-relative path of an existing note."),
                        "text": s_("Markdown to append. A blank line is inserted before it."),
                    }),
                    &["path", "text"],
                )
            },
        },
    ]
}

/// The catalog as MCP's `tools/list` payload.
fn tools_list_payload() -> Value {
    let tools: Vec<Value> = tool_catalog()
        .iter()
        .map(|t| {
            json!({
                "name": t.name,
                "description": t.description,
                "inputSchema": (t.schema)(),
            })
        })
        .collect();
    json!({ "tools": tools })
}

/// The system-prompt text MCP clients inject on connect. This is
/// what makes the agent aware it is *inside* Task rather than
/// holding a bag of unrelated tools.
#[must_use]
pub fn server_instructions(slug: &str, now: chrono::DateTime<chrono::Local>) -> String {
    format!(
        "You are connected to Task — the user's personal operating system. Task is a \
local-first vault of markdown notes with structured overlays on top: tasks, projects, \
goals, a calendar, and a capture inbox. You are not looking at a generic database; \
these are the user's real commitments, and changes you make show up immediately in \
their app.\n\
\n\
Working context: org `{slug}`. Right now it is {stamp} ({weekday}). Resolve every \
relative date the user says — \"tomorrow\", \"next Tuesday\", \"end of the month\" — \
against that, and pass absolute ISO dates to tools.\n\
\n\
How to work here:\n\
- Never invent an id. Call `list_tasks` / `list_events` first, then act on what came back.\n\
- Prefer `create_task` with the user's own phrasing — it parses tags, contexts, \
projects, priority, and dates the same way the app's quick-add does.\n\
- Anything half-formed — a reminder, a follow-up, a thought worth keeping — belongs in \
`capture_inbox`, not in a task. Captures arrive as suggestions the user accepts with one \
tap, so err on the side of capturing.\n\
- Rescheduling means `list_events` for the window, then one `reschedule_event` per move. \
Say what you're about to move before you move it.\n\
- Read before you write: `search_vault` then `read_note`. The user's notes are usually \
better context than your assumptions.\n\
- Confirm before destructive or wide-reaching changes (cancelling events you didn't just \
create, bulk status changes). Ordinary additions don't need a confirmation round-trip.",
        stamp = now.format("%A, %B %-d, %Y at %-I:%M %p %:z"),
        weekday = now.format("%A"),
    )
}

// ── HTTP surface ─────────────────────────────────────────────────

/// `POST /org/{slug}/mcp`.
pub async fn mcp_handler(
    State(state): State<AppState>,
    Path(slug): Path<String>,
    headers: HeaderMap,
    body: String,
) -> Response {
    let request: Value = match serde_json::from_str(&body) {
        Ok(v) => v,
        Err(e) => {
            return Json(rpc_error(
                Value::Null,
                code::PARSE,
                format!("invalid JSON: {e}"),
            ))
            .into_response();
        }
    };
    // The current MCP revision dropped JSON-RPC batching.
    if request.is_array() {
        return Json(rpc_error(
            Value::Null,
            code::INVALID_REQUEST,
            "batched requests are not supported",
        ))
        .into_response();
    }
    let id = request.get("id").cloned().unwrap_or(Value::Null);
    let Some(method) = request.get("method").and_then(Value::as_str) else {
        return Json(rpc_error(id, code::INVALID_REQUEST, "missing `method`")).into_response();
    };
    let params = request.get("params").cloned().unwrap_or_else(|| json!({}));

    // Notifications carry no id and take no response body.
    if id.is_null() && method.starts_with("notifications/") {
        return StatusCode::ACCEPTED.into_response();
    }

    match method {
        // `initialize` is unauthenticated on purpose: a client must be
        // able to discover the server (and learn it needs a token)
        // before it can be told its token is wrong.
        "initialize" => {
            let requested = params
                .get("protocolVersion")
                .and_then(Value::as_str)
                .unwrap_or(LATEST_PROTOCOL);
            let version = if SUPPORTED_PROTOCOLS.contains(&requested) {
                requested
            } else {
                LATEST_PROTOCOL
            };
            Json(rpc_result(
                id,
                json!({
                    "protocolVersion": version,
                    "capabilities": { "tools": { "listChanged": false } },
                    "serverInfo": { "name": format!("task/{slug}"), "version": env!("CARGO_PKG_VERSION") },
                    "instructions": server_instructions(&slug, chrono::Local::now()),
                }),
            ))
            .into_response()
        }
        "ping" => Json(rpc_result(id, json!({}))).into_response(),
        "tools/list" => match authenticate(&state, &slug, &headers).await {
            Ok(_) => Json(rpc_result(id, tools_list_payload())).into_response(),
            Err(e) => Json(rpc_error(id, code::INVALID_REQUEST, e)).into_response(),
        },
        "tools/call" => {
            let org = match authenticate(&state, &slug, &headers).await {
                Ok(org) => org,
                Err(e) => return Json(rpc_error(id, code::INVALID_REQUEST, e)).into_response(),
            };
            let Some(name) = params.get("name").and_then(Value::as_str) else {
                return Json(rpc_error(id, code::INVALID_PARAMS, "missing tool `name`"))
                    .into_response();
            };
            let args = params
                .get("arguments")
                .cloned()
                .unwrap_or_else(|| json!({}));
            // The domain backends are the same sync trait impls the
            // vox layer serves, and they're written for architect's
            // blocking dispatcher — `vault_sync` in particular takes
            // `blocking_read` locks, which panics on an async worker.
            // Run the whole dispatch on the blocking pool, which also
            // keeps a vault-walking tool off the reactor.
            let called = name.to_string();
            let dispatched =
                tokio::task::spawn_blocking(move || call_tool(&org, &called, &args)).await;
            let outcome = match dispatched {
                Ok(r) => r,
                Err(e) => Err(ToolFailure::Message(format!("tool panicked: {e}"))),
            };
            let payload = match outcome {
                Ok(v) => tool_ok(&v),
                // Unknown tool is a protocol error; anything else is a
                // tool-level failure the model should read and retry.
                Err(ToolFailure::Unknown) => {
                    return Json(rpc_error(
                        id,
                        code::METHOD_NOT_FOUND,
                        format!("unknown tool `{name}`"),
                    ))
                    .into_response();
                }
                Err(ToolFailure::Message(m)) => tool_err(m),
            };
            Json(rpc_result(id, payload)).into_response()
        }
        // Advertised-as-absent capabilities: answer emptily rather
        // than erroring, so clients that probe anyway stay happy.
        "resources/list" => Json(rpc_result(id, json!({ "resources": [] }))).into_response(),
        "prompts/list" => Json(rpc_result(id, json!({ "prompts": [] }))).into_response(),
        other => Json(rpc_error(
            id,
            code::METHOD_NOT_FOUND,
            format!("unsupported method `{other}`"),
        ))
        .into_response(),
    }
}

/// Bearer check → the org's backend state. Mirrors
/// [`crate::watch_bridge`]'s rule with `TASK_MCP_TOKEN` as the static
/// device token.
async fn authenticate(
    state: &AppState,
    slug: &str,
    headers: &HeaderMap,
) -> Result<crate::OrgAppState, String> {
    let token = crate::watch_bridge::bearer(headers).ok_or("missing bearer token")?;
    let org = state
        .org(slug)
        .ok_or_else(|| format!("org `{slug}` not hosted"))?;

    let static_token = std::env::var("TASK_MCP_TOKEN").unwrap_or_default();
    if !static_token.is_empty() && token == static_token {
        return Ok(org);
    }
    match org
        .auth
        .auth
        .current_session(architect_auth::CurrentSession { token })
        .await
    {
        Ok(_) => Ok(org),
        Err(_) => Err("invalid token".to_string()),
    }
}

/// Why a tool call didn't produce a result.
enum ToolFailure {
    /// No such tool — a protocol-level error.
    Unknown,
    /// The tool ran and failed, or its arguments were wrong. The
    /// model sees this text.
    Message(String),
}

impl From<String> for ToolFailure {
    fn from(m: String) -> Self {
        Self::Message(m)
    }
}

// ── Argument helpers ─────────────────────────────────────────────

fn arg_str(args: &Value, key: &str) -> Option<String> {
    args.get(key)
        .and_then(Value::as_str)
        .map(str::trim)
        .filter(|s| !s.is_empty())
        .map(str::to_owned)
}

fn required_str(args: &Value, key: &str) -> Result<String, ToolFailure> {
    arg_str(args, key).ok_or_else(|| ToolFailure::Message(format!("`{key}` is required")))
}

fn arg_bool(args: &Value, key: &str) -> bool {
    args.get(key).and_then(Value::as_bool).unwrap_or(false)
}

/// A list tool's row cap: caller's value, clamped, defaulted.
fn arg_limit(args: &Value) -> usize {
    args.get("limit")
        .and_then(Value::as_u64)
        .map_or(DEFAULT_LIMIT, |n| (n as usize).clamp(1, MAX_LIMIT))
}

/// Distinguish "field absent, leave it alone" from "field present and
/// empty, clear it" — the difference between not touching a due date
/// and removing one.
fn optional_field(args: &Value, key: &str) -> Option<Option<String>> {
    match args.get(key) {
        None | Some(Value::Null) => None,
        Some(Value::String(s)) if s.trim().is_empty() => Some(None),
        Some(Value::String(s)) => Some(Some(s.trim().to_string())),
        Some(_) => None,
    }
}

/// The date part of an RFC-3339 or ISO timestamp, for windowing.
fn date_of(stamp: &str) -> &str {
    stamp.split('T').next().unwrap_or(stamp)
}

/// Turn a backend error into something the model can act on.
///
/// Domain errors debug-print as bare variants (`NotFound`,
/// `AlreadyExists(..)`) — true but useless to an agent deciding what
/// to do next. Naming the operation and the subject turns a dead end
/// into a next step.
fn backend_err(what: &str, subject: &str, e: &impl std::fmt::Debug) -> ToolFailure {
    let raw = format!("{e:?}");
    let msg = if raw.starts_with("NotFound") {
        format!(
            "no {what} matching `{subject}`. Call the matching list/search tool first and use a value from its result."
        )
    } else if raw.starts_with("AlreadyExists") {
        format!("a {what} already exists at `{subject}`.")
    } else {
        format!("couldn't {what} `{subject}`: {raw}")
    };
    ToolFailure::Message(msg)
}

// ── Tool implementations ─────────────────────────────────────────

/// Dispatch one tool call. Synchronous on purpose — the backends
/// are architect's blocking-dispatcher trait impls; the caller runs
/// this on `spawn_blocking`.
#[allow(clippy::too_many_lines)]
fn call_tool(org: &crate::OrgAppState, name: &str, args: &Value) -> Result<Value, ToolFailure> {
    use goal::GoalService as _;
    use inbox_proto::Inbox as _;
    use project::ProjectService as _;
    use scheduling_proto::service::calendar_events::CalendarEvents as _;
    use task::TaskService as _;
    use vault_proto::VaultSync as _;

    match name {
        "task_context" => {
            let now = chrono::Local::now();
            let today = now.format("%Y-%m-%d").to_string();
            let open = org
                .tasks
                .query(task::TaskListFilter {
                    open_only: true,
                    ..Default::default()
                })
                .map_err(|e| ToolFailure::Message(format!("{e:?}")))?;
            let due_today = open
                .iter()
                .filter(|t| {
                    t.due
                        .as_deref()
                        .is_some_and(|d| date_of(d) <= today.as_str())
                        || t.scheduled
                            .as_deref()
                            .is_some_and(|d| date_of(d) <= today.as_str())
                })
                .count();
            let events_today = org
                .scheduling
                .list_events()
                .map(|evs| evs.iter().filter(|e| date_of(&e.start) == today).count())
                .unwrap_or(0);
            let inbox_open = org
                .inbox
                .review_queue(today.clone())
                .map(|q| q.len())
                .unwrap_or(0);
            Ok(json!({
                "org": org.slug,
                "now": now.to_rfc3339(),
                "today": today,
                "weekday": now.format("%A").to_string(),
                "open_tasks": open.len(),
                "tasks_due_or_scheduled_today": due_today,
                "events_today": events_today,
                "inbox_awaiting_review": inbox_open,
            }))
        }

        "list_tasks" => {
            let status = arg_str(args, "status");
            let filter = task::TaskListFilter {
                // An explicit status wins; without one we show what's
                // actionable, which is what "my tasks" means.
                open_only: status.is_none(),
                status,
                due_on_or_before: arg_str(args, "due_on_or_before"),
                ..Default::default()
            };
            let query = arg_str(args, "query").map(|q| q.to_lowercase());
            let rows = org
                .tasks
                .query(filter)
                .map_err(|e| ToolFailure::Message(format!("{e:?}")))?;
            let out: Vec<Value> = rows
                .iter()
                .filter(|t| {
                    query
                        .as_ref()
                        .is_none_or(|q| t.title.to_lowercase().contains(q))
                })
                .take(arg_limit(args))
                .map(task_json)
                .collect();
            Ok(json!({ "count": out.len(), "tasks": out }))
        }

        "create_task" => {
            let text = required_str(args, "text")?;
            let mut draft = task::capture(&text);
            if let Some(due) = arg_str(args, "due") {
                draft.due = Some(due);
            }
            if let Some(scheduled) = arg_str(args, "scheduled") {
                draft.scheduled = Some(scheduled);
            }
            if let Some(priority) = arg_str(args, "priority") {
                draft.priority = priority;
            }
            let created = org
                .tasks
                .create(draft)
                .map_err(|e| backend_err("task", &text, &e))?;
            Ok(task_json(&created))
        }

        "update_task" => {
            let id = required_str(args, "id")?;
            let uuid = id
                .parse::<uuid::Uuid>()
                .map_err(|_| ToolFailure::Message(format!("`{id}` is not a task id")))?;
            let mut current = org
                .tasks
                .get(uuid)
                .map_err(|e| backend_err("task", &id, &e))?;
            if let Some(status) = arg_str(args, "status") {
                current.status = status;
            }
            if let Some(title) = arg_str(args, "title") {
                current.title = title;
            }
            if let Some(priority) = arg_str(args, "priority") {
                current.priority = priority;
            }
            if let Some(due) = optional_field(args, "due") {
                current.due = due;
            }
            if let Some(scheduled) = optional_field(args, "scheduled") {
                current.scheduled = scheduled;
            }
            let saved = org
                .tasks
                .update(current)
                .map_err(|e| backend_err("task", &id, &e))?;
            Ok(task_json(&saved))
        }

        "list_events" => {
            let from = arg_str(args, "from");
            let to = arg_str(args, "to");
            let mut rows = org
                .scheduling
                .list_events()
                .map_err(|e| ToolFailure::Message(format!("{e:?}")))?;
            rows.retain(|e| {
                let day = date_of(&e.start);
                from.as_deref().is_none_or(|f| day >= f) && to.as_deref().is_none_or(|t| day <= t)
            });
            rows.sort_by(|a, b| a.start.cmp(&b.start));
            let out: Vec<Value> = rows.iter().take(arg_limit(args)).map(event_json).collect();
            Ok(json!({ "count": out.len(), "events": out }))
        }

        "create_event" => {
            let event = scheduling_proto::CalEvent {
                id: uuid::Uuid::new_v4().to_string(),
                title: required_str(args, "title")?,
                start: required_str(args, "start")?,
                end: required_str(args, "end")?,
                all_day: arg_bool(args, "all_day"),
                color: "primary".to_string(),
                description: arg_str(args, "description"),
                recurrence: None,
            };
            org.scheduling
                .upsert_event(&event)
                .map_err(|e| ToolFailure::Message(format!("{e:?}")))?;
            Ok(event_json(&event))
        }

        "reschedule_event" => {
            let id = required_str(args, "id")?;
            let start = required_str(args, "start")?;
            let end = required_str(args, "end")?;
            let mut event = org
                .scheduling
                .list_events()
                .map_err(|e| ToolFailure::Message(format!("{e:?}")))?
                .into_iter()
                .find(|e| e.id == id)
                .ok_or_else(|| ToolFailure::Message(format!("no event with id `{id}`")))?;
            let was = format!("{} → {}", event.start, event.end);
            event.start = start;
            event.end = end;
            org.scheduling
                .upsert_event(&event)
                .map_err(|e| ToolFailure::Message(format!("{e:?}")))?;
            let mut out = event_json(&event);
            out["moved_from"] = json!(was);
            Ok(out)
        }

        "cancel_event" => {
            let id = required_str(args, "id")?;
            org.scheduling
                .delete_event(&id)
                .map_err(|e| backend_err("event", &id, &e))?;
            Ok(json!({ "cancelled": id }))
        }

        "capture_inbox" => {
            let body = required_str(args, "body")?;
            let now = chrono::Utc::now().to_rfc3339();
            let mut item = inbox_proto::InboxItem::capture(
                uuid::Uuid::new_v4().to_string(),
                body,
                "agent",
                now,
            );
            if !arg_bool(args, "accepted") {
                item.status = inbox_proto::InboxItem::STATUS_SUGGESTED.to_string();
            }
            item.resurface_on = arg_str(args, "resurface_on");
            org.inbox
                .upsert_inbox_item(&item)
                .map_err(|e| ToolFailure::Message(format!("{e:?}")))?;
            Ok(json!({
                "id": item.id,
                "status": item.status,
                "resurface_on": item.resurface_on,
                "note": if item.status == inbox_proto::InboxItem::STATUS_SUGGESTED {
                    "captured as a suggestion — the user accepts or dismisses it in their inbox"
                } else {
                    "filed as an open inbox item"
                },
            }))
        }

        "list_inbox" => {
            // The review queue is "what surfaces today", which by
            // design excludes suggestions — so an agent couldn't see
            // its own captures. `status` opens the full list.
            let rows = match arg_str(args, "status") {
                Some(status) => {
                    let all = org
                        .inbox
                        .list_inbox()
                        .map_err(|e| ToolFailure::Message(format!("{e:?}")))?;
                    all.into_iter()
                        .filter(|i| i.status.eq_ignore_ascii_case(&status))
                        .collect()
                }
                None => {
                    let today = chrono::Local::now().format("%Y-%m-%d").to_string();
                    org.inbox
                        .review_queue(today)
                        .map_err(|e| ToolFailure::Message(format!("{e:?}")))?
                }
            };
            let out: Vec<Value> = rows
                .iter()
                .take(arg_limit(args))
                .map(|i| {
                    json!({
                        "id": i.id,
                        "body": i.body,
                        "kind": i.kind,
                        "status": i.status,
                        "created": i.created,
                    })
                })
                .collect();
            Ok(json!({ "count": out.len(), "items": out }))
        }

        "list_projects" => {
            let rows = org
                .projects
                .list()
                .map_err(|e| ToolFailure::Message(format!("{e:?}")))?;
            let out: Vec<Value> = rows
                .iter()
                .take(arg_limit(args))
                .map(|p| {
                    json!({
                        "id": p.id.to_string(),
                        "title": p.title,
                        "status": p.status,
                        "path": p.path,
                    })
                })
                .collect();
            Ok(json!({ "count": out.len(), "projects": out }))
        }

        "list_goals" => {
            let rows = org
                .goals
                .list()
                .map_err(|e| ToolFailure::Message(format!("{e:?}")))?;
            let out: Vec<Value> = rows
                .iter()
                .take(arg_limit(args))
                .map(|g| {
                    json!({
                        "id": g.id.to_string(),
                        "title": g.title,
                        "status": g.status,
                        "path": g.path,
                    })
                })
                .collect();
            Ok(json!({ "count": out.len(), "goals": out }))
        }

        "search_vault" => {
            let query = required_str(args, "query")?.to_lowercase();
            let manifest = org
                .vault_sync
                .manifest(VAULT_ID)
                .map_err(|e| ToolFailure::Message(format!("{e:?}")))?;
            let mut hits: Vec<&str> = manifest
                .files
                .iter()
                .map(|f| f.path.as_str())
                .filter(|p| p.to_lowercase().contains(&query))
                .collect();
            hits.sort_unstable();
            let limit = arg_limit(args);
            let total = hits.len();
            hits.truncate(limit);
            Ok(json!({
                "count": hits.len(),
                "truncated": total > hits.len(),
                "paths": hits,
            }))
        }

        "read_note" => {
            let path = required_str(args, "path")?;
            let bytes = org
                .vault_sync
                .get_file(VAULT_ID, &path)
                .map_err(|e| backend_err("note", &path, &e))?;
            let text = String::from_utf8_lossy(&bytes.0).to_string();
            Ok(json!({ "path": path, "content": text }))
        }

        "append_note" => {
            let path = required_str(args, "path")?;
            let text = required_str(args, "text")?;
            let existing = org
                .vault_sync
                .get_file(VAULT_ID, &path)
                .map_err(|e| backend_err("note", &path, &e))?;
            let mut body = String::from_utf8_lossy(&existing.0).to_string();
            if !body.ends_with('\n') {
                body.push('\n');
            }
            body.push('\n');
            body.push_str(&text);
            body.push('\n');
            org.vault_sync
                .put_file(
                    VAULT_ID,
                    &path,
                    body.into_bytes(),
                    vault_proto::IfMatch::Force,
                )
                .map_err(|e| ToolFailure::Message(format!("{e:?}")))?;
            Ok(json!({ "path": path, "appended": true }))
        }

        _ => Err(ToolFailure::Unknown),
    }
}

/// A task, trimmed to the fields an agent reasons over. The full row
/// carries time entries, recurrence anchors, and relation graphs that
/// would only burn context.
fn task_json(t: &task::TaskInfo) -> Value {
    json!({
        "id": t.id.to_string(),
        "title": t.title,
        "status": t.status,
        "priority": t.priority,
        "due": t.due,
        "scheduled": t.scheduled,
        "tags": t.tags.0,
        "contexts": t.contexts.0,
        "projects": t.projects.0,
        "path": t.path,
    })
}

fn event_json(e: &scheduling_proto::CalEvent) -> Value {
    json!({
        "id": e.id,
        "title": e.title,
        "start": e.start,
        "end": e.end,
        "all_day": e.all_day,
        "description": e.description,
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn catalog_entries_are_well_formed() {
        let catalog = tool_catalog();
        assert!(catalog.len() >= 15, "catalog shrank unexpectedly");
        let mut seen = std::collections::HashSet::new();
        for tool in &catalog {
            assert!(seen.insert(tool.name), "duplicate tool `{}`", tool.name);
            assert!(
                tool.description.len() > 40,
                "`{}` needs a description the model can select on",
                tool.name
            );
            let schema = (tool.schema)();
            assert_eq!(schema["type"], "object", "`{}` schema", tool.name);
            assert!(schema["properties"].is_object(), "`{}` schema", tool.name);
            // Every declared `required` name must exist in properties,
            // or clients reject the tool at registration.
            for req in schema["required"].as_array().expect("required array") {
                let key = req.as_str().expect("required entry is a string");
                assert!(
                    schema["properties"].get(key).is_some(),
                    "`{}` requires `{key}` but doesn't declare it",
                    tool.name
                );
            }
        }
    }

    #[test]
    fn tools_list_payload_matches_the_catalog() {
        let payload = tools_list_payload();
        let tools = payload["tools"].as_array().expect("tools array");
        assert_eq!(tools.len(), tool_catalog().len());
        assert!(tools.iter().all(|t| t["inputSchema"].is_object()));
        assert!(tools.iter().any(|t| t["name"] == "create_task"));
    }

    #[test]
    fn instructions_carry_the_org_and_the_current_date() {
        let now = chrono::Local::now();
        let text = server_instructions("acme", now);
        assert!(text.contains("org `acme`"));
        assert!(text.contains(&now.format("%Y").to_string()));
        assert!(text.contains(&now.format("%A").to_string()));
        // The orientation the whole feature exists for.
        assert!(text.contains("Task"));
        assert!(text.contains("capture_inbox"));
    }

    #[test]
    fn optional_field_separates_absent_from_cleared() {
        let args = json!({ "due": "2026-08-01", "scheduled": "", "priority": "high" });
        assert_eq!(
            optional_field(&args, "due"),
            Some(Some("2026-08-01".to_string()))
        );
        // Present but empty = clear it.
        assert_eq!(optional_field(&args, "scheduled"), Some(None));
        // Absent = don't touch it.
        assert_eq!(optional_field(&args, "missing"), None);
    }

    #[test]
    fn limits_are_clamped_and_defaulted() {
        assert_eq!(arg_limit(&json!({})), DEFAULT_LIMIT);
        assert_eq!(arg_limit(&json!({ "limit": 5 })), 5);
        assert_eq!(arg_limit(&json!({ "limit": 100_000 })), MAX_LIMIT);
        assert_eq!(arg_limit(&json!({ "limit": 0 })), 1);
    }

    #[test]
    fn arg_helpers_trim_and_reject_blank() {
        let args = json!({ "text": "  hi  ", "blank": "   " });
        assert_eq!(arg_str(&args, "text").as_deref(), Some("hi"));
        assert_eq!(arg_str(&args, "blank"), None);
        assert!(required_str(&args, "blank").is_err());
    }

    #[test]
    fn date_of_takes_the_day_from_either_stamp_form() {
        assert_eq!(date_of("2026-07-25T14:00:00-05:00"), "2026-07-25");
        assert_eq!(date_of("2026-07-25"), "2026-07-25");
    }

    #[test]
    fn tool_results_use_the_mcp_content_envelope() {
        let ok = tool_ok(&json!({ "count": 1 }));
        assert_eq!(ok["isError"], false);
        assert_eq!(ok["content"][0]["type"], "text");
        assert_eq!(ok["content"][0]["text"], r#"{"count":1}"#);

        let err = tool_err("no event with id `x`");
        assert_eq!(err["isError"], true);
        assert!(
            err["content"][0]["text"]
                .as_str()
                .expect("text")
                .contains("no event")
        );
    }

    #[test]
    fn backend_errors_become_actionable_text() {
        #[derive(Debug)]
        #[allow(dead_code)]
        enum FakeErr {
            NotFound,
            AlreadyExists(String),
            Io(String),
        }
        let msg = match backend_err("note", "nope.md", &FakeErr::NotFound) {
            ToolFailure::Message(m) => m,
            ToolFailure::Unknown => panic!("wrong variant"),
        };
        // Names the thing AND the way out — a bare "NotFound" leaves
        // the model with nothing to do next.
        assert!(msg.contains("nope.md"), "{msg}");
        assert!(msg.contains("list/search tool"), "{msg}");

        let msg = match backend_err("task", "a.md", &FakeErr::AlreadyExists("a.md".into())) {
            ToolFailure::Message(m) => m,
            ToolFailure::Unknown => panic!("wrong variant"),
        };
        assert!(msg.contains("already exists"), "{msg}");

        // Anything unrecognized keeps the raw detail rather than
        // swallowing it.
        let msg = match backend_err("note", "x.md", &FakeErr::Io("disk full".into())) {
            ToolFailure::Message(m) => m,
            ToolFailure::Unknown => panic!("wrong variant"),
        };
        assert!(msg.contains("disk full"), "{msg}");
    }

    #[test]
    fn rpc_envelopes_carry_the_request_id() {
        let ok = rpc_result(json!(7), json!({ "a": 1 }));
        assert_eq!(ok["jsonrpc"], "2.0");
        assert_eq!(ok["id"], 7);
        let err = rpc_error(json!("abc"), code::METHOD_NOT_FOUND, "nope");
        assert_eq!(err["id"], "abc");
        assert_eq!(err["error"]["code"], code::METHOD_NOT_FOUND);
    }
}
