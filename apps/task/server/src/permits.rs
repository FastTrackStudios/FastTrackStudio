//! The org lane's permit tables — one per mounted vox service — plus the
//! coverage / dry-run reporting that makes the gate's blind spots visible.
//!
//! # Why this module exists
//!
//! [`crate::org_layer_router`] mounts ~70 services; until now exactly two
//! (`VaultSyncRpc`, `MediaService`) carried a [`ServicePermits`] table, and
//! the other ~68 fell through the gate's [`UnlistedPolicy::Allow`]
//! *silently*. Nothing at boot said "66 of your services are unchecked", so
//! the gap sat there. This module (a) supplies a table for every mounted
//! service, (b) derives the mounted-service list ONCE so the permit tables,
//! the schema stamps, and the router can't drift apart, and (c) reports
//! coverage + a static dry-run at boot.
//!
//! [`UnlistedPolicy::Allow`]: architect::permissions_gate::UnlistedPolicy
//!
//! # The model (see `libs/architect/permissions`)
//!
//! A [`MethodPermit`] says "this method needs `<action>` on `<resource>`".
//! The gate checks the *coarse* resource — `vault/{path}` widens to
//! `vault/**` — because the method-level gate runs before argument decode;
//! argument-exact checks are the service impl's job (same engine, finer
//! resource). Methods of a tabled service that are NOT listed are DENIED
//! (fail-closed), which is why [`coverage`] reports them by name.
//!
//! ## Actions used here
//!
//! Only `read` / `write` / `comment` / `download` — the four the built-in
//! `member` role grants. **`admin` is deliberately absent.** The org lane
//! runs `RoleEngine::with_default_user_role("member")` and nothing calls
//! `set_member`, so today *every* validated user is a member and *nobody*
//! is an owner: a single `admin` permit would lock every human out of that
//! method the moment enforcement is switched on. Admin-tier permits wait
//! for per-row membership sync (see `plans/architect-permissions.md`).
//!
//! `.audited()` marks the methods worth an audit line even when allowed:
//! deletes, money movements, outbound sends, downloads.
//!
//! ## Resource namespaces
//!
//! One per domain, path-shaped, so role rules can later be written per
//! domain (`Rule::new("finance/**", &["read"])`):
//!
//! | prefix | lane |
//! |---|---|
//! | `vault/**` | vault files + the read-only vault graph |
//! | `media/**`, `attachments/**` | blobs |
//! | `doc/**` | per-file CRDT sync + presence |
//! | `public/**` | reachable WITHOUT a session (see below) |
//! | everything else | one prefix per feature slice |
//!
//! Resources are written as plain `domain/**` rather than `domain/{arg}`
//! templates: the gate widens `{arg}` to `**` anyway, and inventing an
//! argument name that does not match the trait would bake in a wrong
//! interpolation the day arg-level checks land. `vault/{path}` and
//! `media/{content_hash}` keep their templates because those names were
//! verified against their traits.
//!
//! ## `public/**` — the un-authenticated hole, made explicit
//!
//! `RoleEngine` denies [`Principal::Anonymous`] everything, so a table on
//! `AuthService` would make sign-in impossible the moment enforcement is
//! on — the caller signing IN has no identity yet. Rather than leaving
//! those services untabled (invisible again), their permits point at
//! `public/…` and the gate's engine gains a [`ScopeEngine`] granting
//! `public/**` to everyone. Two services are public:
//!
//! - **`AuthService`** — every method takes the session token as an
//!   ARGUMENT and validates it itself (`org_members_for_token`,
//!   `current_session`); the gate has nothing to add and would only break
//!   the sign-in path.
//! - **`PermissionsService`** — the capability oracle answers *about the
//!   caller*, from the same engine + resolver the gate enforces with. An
//!   anonymous caller gets an empty manifest, which is the correct answer,
//!   not a leak.
//!
//! [`ScopeEngine`]: architect_permissions::ScopeEngine

use std::collections::HashMap;
use std::sync::{Arc, Mutex};

use architect_permissions::{
    Action, AuditEvent, AuditSink, MethodPermit, PermissionEngine, Principal, Resource,
    ServicePermits,
};
use vox::ServiceDescriptor;

// ── Table-building helpers ───────────────────────────────────────────────

const fn rd(m: &'static str, r: &'static str) -> MethodPermit {
    MethodPermit::new(m, Action::READ, r)
}
const fn wr(m: &'static str, r: &'static str) -> MethodPermit {
    MethodPermit::new(m, Action::WRITE, r)
}
/// Write + audit line even on allow — deletes, money, outbound effects.
const fn wa(m: &'static str, r: &'static str) -> MethodPermit {
    MethodPermit::new(m, Action::WRITE, r).audited()
}
/// Comment-tier write: allowed to `member` AND grantable to share guests.
const fn cm(m: &'static str, r: &'static str) -> MethodPermit {
    MethodPermit::new(m, Action::COMMENT, r)
}
/// Bulk content leaving the server.
const fn dl(m: &'static str, r: &'static str) -> MethodPermit {
    MethodPermit::new(m, Action::DOWNLOAD, r).audited()
}

/// `table!(CONST, "service", "resource/**", [rd "list", wa "delete"])`
macro_rules! table {
    ($name:ident, $service:literal, $res:literal, [$($f:ident $m:literal),* $(,)?]) => {
        const $name: ServicePermits = ServicePermits {
            service: $service,
            methods: &[$($f($m, $res)),*],
        };
    };
}

/// The resource glob every principal — including [`Principal::Anonymous`] —
/// holds, via the gate's public [`architect_permissions::ScopeEngine`].
pub const PUBLIC_GLOB: &str = "public/**";

// ── Platform lane ────────────────────────────────────────────────────────

// Self-guarding: every method validates the token it is handed. See the
// module docs on `public/**`.
table!(AUTH, "auth", "public/auth", [
    rd "sign_up_email_password", rd "sign_in_email_password", rd "current_session",
    rd "refresh_session", rd "whoami", rd "sign_out", rd "list_org_members",
]);

// The capability oracle — answers about the caller only.
table!(PERMISSIONS, "permissions", "public/permissions", [rd "can", rd "capabilities"]);

table!(ATTACHMENTS, "attachments", "attachments/**", [
    wr "initiate_upload", wr "complete_upload", dl "get_download_url",
]);

/// Unchanged from the original hand-written table (verified arg names).
const MEDIA: ServicePermits = ServicePermits {
    service: "media",
    methods: &[
        MethodPermit::new("stat", Action::READ, "media/{content_hash}"),
        MethodPermit::new("read", Action::READ, "media/{content_hash}").audited(),
    ],
};

/// Unchanged from the original hand-written table (verified arg names).
const VAULT: ServicePermits = ServicePermits {
    service: "vault-sync",
    methods: &[
        MethodPermit::new("manifest", Action::READ, "vault/**"),
        MethodPermit::new("get_file", Action::READ, "vault/{path}"),
        MethodPermit::new("put_file", Action::WRITE, "vault/{path}"),
        MethodPermit::new("delete_file", Action::WRITE, "vault/{path}"),
        MethodPermit::new("folder_index", Action::READ, "vault/**"),
        MethodPermit::new("set_folder", Action::WRITE, "vault/{path}"),
        MethodPermit::new("open_collab", Action::READ, "vault/{path}"),
        MethodPermit::new("base_views", Action::READ, "vault/{path}"),
    ],
};

table!(VAULT_GRAPH, "vault-graph", "vault/**", [
    rd "backlinks", rd "links", rd "orphans", rd "unresolved", rd "deadends", rd "tags",
]);

table!(SHARE, "share", "shares/**", [
    wa "create_link", rd "list_links", rd "links_for_note",
    wa "set_link_disabled", wa "delete_link",
]);

// Per-file CRDT: `sync` mutates the doc; presence is ephemeral and never
// persisted (see `crate::presence`).
table!(DOC_SYNC, "doc-sync", "doc/**", [wr "sync"]);
table!(DOC_PRESENCE, "doc-presence", "doc/presence/**", [rd "presence"]);

// ── Agent lane ───────────────────────────────────────────────────────────

table!(AGENT_TASKS, "agent-tasks", "agent/tasks/**", [
    rd "read_queue", wr "claim_agent_task", wr "set_agent_task_status",
    wr "complete_agent_task", wr "link_agent_task_to_session", rd "list_agent_task_links",
]);

table!(AGENT_SESSIONS, "agent-sessions", "agent/sessions/**", [
    wr "create_session", rd "read_session", rd "list_sessions", wr "rename_session",
    wr "pin_session", wr "archive_session", wa "delete_session", wr "save_composer_draft",
]);

// `dispatch_turn` runs a model turn (spends tokens, may touch the repo).
table!(AGENT_TURNS, "agent-turns", "agent/turns/**", [
    wa "dispatch_turn", wr "cancel_turn", wr "resume_session",
]);

table!(AGENT_THREADS, "agent-threads", "agent/threads/**", [
    rd "list_messages", rd "read_message", cm "append_note",
]);

// The three `subscribe_*(id, tx)` calls collapsed into one unfiltered
// `#[subscribe]` stream — the envelope names its session and subscribers
// filter client-side — so this is now one read over every agent event.
table!(AGENT_SUBSCRIPTIONS, "agent-subscriptions", "agent/events/**", [
    rd "events",
]);

table!(AGENT_DISCOVERY, "agent-discovery", "agent/discovery/**", [
    rd "list_models", rd "list_skills", rd "list_capabilities", rd "backend_health",
]);

table!(AGENT_ROUTINES, "agent-routines", "agent/routines/**", [
    rd "list_routines", wr "create_routine", wr "set_routine_paused",
    wa "run_routine", wa "delete_routine",
]);

// ── Work lane (projects / goals / milestones / workstreams / tasks) ──────

table!(PROJECT, "project", "projects/**", [
    rd "list", rd "get", rd "get_by_path", wr "create", wr "update", wr "rename", wa "delete",
]);
table!(PROJECT_STREAM, "project-stream", "projects/**", [rd "events"]);
table!(GOAL, "goal", "goals/**", [
    rd "list", rd "get", rd "get_by_path", wr "create", wr "update", wr "rename", wa "delete",
]);
table!(GOAL_STREAM, "goal-stream", "goals/**", [rd "events"]);
table!(MILESTONE, "milestone", "milestones/**", [
    rd "list", rd "get", rd "get_by_path", wr "create", wr "update", wr "rename", wa "delete",
]);
table!(MILESTONE_STREAM, "milestone-stream", "milestones/**", [rd "events"]);
table!(WORKSTREAM, "workstream", "workstreams/**", [
    rd "list", rd "get", rd "get_by_path", wr "create", wr "update",
    wr "set_status", wa "delete", rd "rollup",
]);
table!(WORKSTREAM_STREAM, "workstream-stream", "workstreams/**", [rd "events"]);
table!(TASK, "task", "tasks/**", [
    rd "list", rd "get", rd "get_by_path", wr "create", wr "update", wr "try_claim",
    rd "reverse_relations", rd "reverse_relations_batch", rd "query", wr "rename", wa "delete",
]);
table!(TASK_STREAM, "task-stream", "tasks/**", [rd "events"]);

table!(TIMER, "timer", "timer/**", [
    wr "start_timer", wr "stop_timer", rd "active_timer", wr "switch_timer", wr "log_session",
    rd "resolve_rate", rd "list_sessions", wr "update_session", wa "delete_session",
    wa "set_org_member_rate", wa "set_project_member_rate",
    rd "list_org_member_rates", rd "list_project_member_rates",
]);
table!(TIMER_STREAM, "timer-stream", "timer/**", [rd "events"]);

table!(THREADS, "threads", "threads/**", [
    rd "list_threads", rd "get_thread", cm "create_thread", rd "list_messages",
    cm "post_message", wr "set_resolved", wa "delete_thread", wa "delete_message",
]);

table!(PREFS, "prefs", "prefs/**", [rd "get", wr "set"]);

// ── Scheduling lane ──────────────────────────────────────────────────────

table!(DAY_TEMPLATES, "day-templates", "scheduling/day-templates/**", [
    rd "list_day_templates", rd "get_day_template", wr "upsert_day_template",
    wa "delete_day_template",
]);
table!(DAY_PLANS, "day-plans", "scheduling/day-plans/**", [
    rd "get_day_plan", wr "upsert_day_plan", wa "delete_day_plan",
]);
table!(CALENDAR_EVENTS, "calendar-events", "scheduling/events/**", [
    rd "list_events", wr "upsert_event", wa "delete_event",
]);
table!(EVENT_TYPES, "event-types", "scheduling/event-types/**", [
    rd "list_event_types", rd "get_event_type", wr "upsert_event_type", wa "delete_event_type",
]);
table!(SCHEDULES, "schedules", "scheduling/schedules/**", [
    rd "list_schedules", rd "get_schedule", wr "upsert_schedule", wa "delete_schedule",
]);
table!(SLOTS, "slots", "scheduling/slots/**", [rd "list_open_slots"]);
table!(BOOKINGS, "bookings", "scheduling/bookings/**", [
    rd "list_bookings", rd "get_booking", wr "create_booking", wr "update_booking_status",
]);
// The slice's one stream: attaching is a read over the whole
// scheduling resource — the event names the sub-resource and
// subscribers filter client-side.
table!(SCHEDULING_STREAM, "scheduling-events", "scheduling/**", [rd "events"]);

// ── Knowledge lane ───────────────────────────────────────────────────────

table!(INBOX, "inbox", "inbox/**", [
    rd "list_inbox", rd "review_queue", rd "get_inbox_item",
    wr "upsert_inbox_item", wa "delete_inbox_item",
]);
table!(INBOX_STREAM, "inbox-stream", "inbox/**", [rd "events"]);
table!(RECALL, "recall", "recall/**", [
    rd "list_cards", rd "review_queue", wr "upsert_card", wa "delete_card",
]);
table!(RECALL_STREAM, "recall-stream", "recall/**", [rd "events"]);
table!(CONTACTS, "contacts", "contacts/**", [
    rd "list_contacts", rd "get_contact", wr "upsert_contact", wa "delete_contact",
    rd "list_accounts", wr "upsert_account", wa "delete_account", wa "sync_account",
]);
table!(CONTACTS_STREAM, "contacts-stream", "contacts/**", [rd "events"]);
table!(TAGS, "tags", "tags/**", [
    rd "list_tags", rd "get_tag", wr "upsert_tag", wa "delete_tag",
]);
table!(SCRIPTURE, "scripture", "scripture/**", [
    rd "translations", rd "chapter", rd "verse", rd "compare", rd "chapter_backlinks",
    rd "lexicon", rd "word_study", rd "occurrences", rd "original_editions", rd "interlinear",
    rd "study", rd "cross_refs", rd "topics_of", rd "verses_for_topic",
]);
table!(LINKS, "links", "links/**", [
    wr "create", wa "delete", rd "get", rd "links_for", rd "graph",
]);
table!(COLLECTION, "collection", "collections/**", [
    wr "create", rd "get", rd "list", wr "add_item", wr "remove_item", wr "reorder",
]);
table!(RESOURCES, "resources", "resources/**", [rd "transcript"]);

// ── Finance lane (every mutation audited) ────────────────────────────────

table!(INVOICING, "invoicing", "finance/invoicing/**", [
    wa "generate_invoice", rd "list_invoices", rd "get_invoice", wa "delete_invoice",
    wa "record_invoice_payment", rd "uninvoiced", wa "mark_sent", wa "void_with_credit",
    wa "record_payment", wa "refund_payment", wa "run_schedule_once",
]);
table!(LEDGER, "ledger", "finance/ledger/**", [
    wa "post_transaction", rd "account_transactions", rd "balances", rd "books", rd "accounts",
]);

// ── Wiki lane ────────────────────────────────────────────────────────────

table!(WIKI_SCHEMA, "wiki-schema", "wiki/schema/**", [
    wa "bootstrap", rd "read_schema", rd "read_purpose", wr "write_schema",
    wr "write_purpose", rd "health",
]);
table!(WIKI_CATALOG, "wiki-catalog", "wiki/catalog/**", [
    rd "read_index", wa "rebuild_index", wr "append_log",
]);
table!(WIKI_RAW, "wiki-raw", "wiki/raw/**", [
    wa "import_raw_source", rd "list_raw_sources", rd "read_raw_source",
    wa "delete_raw_source", wr "rescan_sources",
]);
table!(WIKI_GRAPH, "wiki-graph", "wiki/graph/**", [
    wr "build_graph", rd "relevance", rd "clusters", rd "gaps",
]);
table!(WIKI_PAGES, "wiki-pages", "wiki/pages/**", [
    rd "list_pages", rd "read_page", wr "write_page",
]);
table!(WIKI_INGEST, "wiki-ingest", "wiki/ingest/**", [
    wr "enqueue_ingest", rd "list_ingest", wr "claim_next_ingest", wr "record_analysis",
    wr "record_pages", wr "fail_ingest", wr "cancel_ingest", wr "retry_ingest",
]);
table!(WIKI_LINT, "wiki-lint", "wiki/lint/**", [
    wr "lint", rd "list_findings", wr "resolve_finding",
]);
table!(WIKI_SEARCH, "wiki-search", "wiki/search/**", [rd "search"]);
table!(WIKI_WATCHER, "wiki-watcher", "wiki/watcher/**", [wr "set_watch", rd "is_watching"]);
table!(WIKI_MULTIMODAL, "wiki-multimodal", "wiki/multimodal/**", [wr "extract_images"]);
table!(WIKI_REVIEW, "wiki-review", "wiki/review/**", [
    wr "enqueue_review", rd "list_review", wa "apply_review",
]);

// ── Home lane (locations / inventory / meals / fitness) ──────────────────

table!(LOCATIONS, "locations", "locations/**", [
    rd "list", rd "get", wr "create", wr "update", wr "rename", wa "delete",
]);
table!(INVENTORY, "inventory", "inventory/**", [
    rd "list", rd "list_at", rd "get", wr "create", wr "update", wr "rename", wa "delete",
    wr "set_status", wr "set_condition", wr "set_location",
]);
table!(COOKBOOK, "cookbook", "mealplan/cookbook/**", [
    rd "list", rd "get", wr "create", wr "update", wr "rename", wa "delete", wr "import",
]);
table!(MEALPLAN, "mealplan", "mealplan/plan/**", [
    rd "list", rd "get", wr "create", wr "update", wr "rename", wa "delete",
    wr "cook", wr "skip", rd "can_cook", wr "cook_recipe",
]);
table!(PANTRY, "pantry", "mealplan/pantry/**", [
    rd "list", rd "get", wr "create", wr "update", wr "rename", wa "delete",
    wr "consume", wr "restock", wr "open", rd "find_by_barcode", rd "resolve_barcode",
    wr "add_stock", wr "consume_stock", wr "transfer_stock", wr "inventory_set",
]);
table!(SHOPPING, "shopping", "mealplan/shopping/**", [
    rd "list", rd "get", wr "create", wr "update", wa "delete",
    wr "add_missing_for_recipe", wr "add_low_stock", wr "add_expired_or_overdue",
    wr "clear", wr "mark_purchased",
]);
table!(SUBSTITUTIONS, "substitutions", "mealplan/substitutions/**", [
    rd "list", rd "get", wr "create", wr "update", wa "delete", rd "for_item",
]);
table!(BODY, "body", "fitness/body/**", [
    rd "list", rd "get", rd "find_by_kind", wr "create", wr "update", wa "delete", wr "log_entry",
]);
table!(EXERCISES, "exercises", "fitness/exercises/**", [
    rd "list", rd "get", rd "find_by_name", wr "create", wr "update", wr "rename", wa "delete",
]);
table!(WORKOUTS, "workouts", "fitness/workouts/**", [
    rd "list_routines", rd "get_routine", wr "create_routine", wr "update_routine",
    wa "delete_routine", rd "list_sessions", rd "get_session", wr "create_session",
    wr "update_session", wa "delete_session", wr "log_set", wr "start_from_routine",
]);
table!(INTAKE, "intake", "fitness/intake/**", [
    rd "list", rd "get", rd "for_day", wr "create", wr "update", wa "delete",
    wr "log_recipe", wr "log_pantry", wr "log_freeform", wr "log_entry",
]);

// ── Outside-world lane (email / forge) ───────────────────────────────────

table!(EMAIL, "email", "email/**", [
    rd "accounts", rd "list_folders", rd "fetch_envelopes", rd "fetch_message",
    dl "fetch_attachment", wr "set_flags", wr "move_message", wa "delete_message",
    wr "append_draft", wa "send",
]);
table!(FORGE_REPOS, "forge-repos", "forge/repos/**", [rd "list_repos", rd "get_repo"]);
table!(FORGE_ISSUES, "forge-issues", "forge/issues/**", [
    rd "list_issues", rd "get_issue", wr "create_issue", wr "update_issue",
    rd "list_comments", cm "add_comment",
]);
table!(FORGE_REVIEWS, "forge-reviews", "forge/reviews/**", [
    rd "list_pull_requests", rd "get_pull_request", wr "create_pull_request",
    wr "update_pull_request", rd "list_reviews", wr "request_reviewers",
    wa "merge_pull_request",
]);
table!(FORGE_CONNECTIONS, "forge-connections", "forge/connections/**", [
    rd "list_connected_repos", rd "repos_for_project",
]);

// ── `#[subscribe]` stream siblings ───────────────────────────────────────
//
// Each stream is a separate vox service with its own descriptor, so it
// needs its own table. Attaching to a change feed is a read over the whole
// resource: the streams are unfiltered — subscribers filter client-side —
// so a subscriber sees every change the org produces for that domain.
table!(VAULT_STREAM, "vault-sync-stream", "vault/**", [rd "changes"]);
table!(WIKI_STREAM, "wiki-events", "wiki/**", [rd "changes"]);
table!(EMAIL_STREAM, "email-stream", "email/**", [rd "changes"]);
table!(FORGE_ISSUES_STREAM, "forge-issues-stream", "forge/issues/**", [rd "issue_events"]);
table!(FORGE_REVIEWS_STREAM, "forge-reviews-stream", "forge/reviews/**", [rd "review_events"]);

// ── The mount list ───────────────────────────────────────────────────────

/// One service as [`crate::org_layer_router`] mounts it: its descriptor and
/// the permit table the gate installs for it.
///
/// `permits: None` means "mounted, deliberately ungated" — it would show up
/// in [`coverage`] as untabled. Nothing uses it today (every mounted
/// service has a table); it exists so a future mount that genuinely cannot
/// be tabled is recorded as a decision instead of an omission.
pub struct Mount {
    pub descriptor: &'static ServiceDescriptor,
    pub permits: Option<ServicePermits>,
}

const fn m(descriptor: &'static ServiceDescriptor, permits: ServicePermits) -> Mount {
    Mount {
        descriptor,
        permits: Some(permits),
    }
}

/// Every service [`crate::org_layer_router`] mounts, paired with its permit
/// table. **Keep in lockstep with that function** — `permits_cover_router`
/// (in `tests/`) fails the build if the counts diverge.
#[must_use]
pub fn mounts() -> Vec<Mount> {
    vec![
        // Platform
        m(architect_auth::auth_service_service_descriptor(), AUTH),
        m(
            architect_permissions_proto::permissions_service_service_descriptor(),
            PERMISSIONS,
        ),
        m(
            attachments_proto::attachment_service_service_descriptor(),
            ATTACHMENTS,
        ),
        m(media_proto::media_service_service_descriptor(), MEDIA),
        m(vault_proto::descriptor(), VAULT),
        m(vault_proto::stream_descriptor(), VAULT_STREAM),
        m(vault_proto::vault_graph_rpc_service_descriptor(), VAULT_GRAPH),
        m(share_proto::share_service_service_descriptor(), SHARE),
        m(crdt::sync::doc_sync_service_descriptor(), DOC_SYNC),
        m(crdt::sync::doc_presence_service_descriptor(), DOC_PRESENCE),
        // Agent
        m(
            agent_proto::service::tasks::agent_task_queue_rpc_service_descriptor(),
            AGENT_TASKS,
        ),
        m(
            agent_proto::service::sessions::sessions_rpc_service_descriptor(),
            AGENT_SESSIONS,
        ),
        m(
            agent_proto::service::turn_dispatch::turn_dispatch_rpc_service_descriptor(),
            AGENT_TURNS,
        ),
        m(
            agent_proto::service::threads::threads_rpc_service_descriptor(),
            AGENT_THREADS,
        ),
        m(
            agent_proto::service::subscriptions::subscriptions_stream_service_descriptor(),
            AGENT_SUBSCRIPTIONS,
        ),
        m(
            agent_proto::service::discovery::discovery_rpc_service_descriptor(),
            AGENT_DISCOVERY,
        ),
        m(
            agent_proto::service::routines::routines_rpc_service_descriptor(),
            AGENT_ROUTINES,
        ),
        // Work
        m(project::project_service_descriptor(), PROJECT),
        m(project::project_stream_descriptor(), PROJECT_STREAM),
        m(goal::goal_service_descriptor(), GOAL),
        m(goal::goal_stream_descriptor(), GOAL_STREAM),
        m(milestone::milestone_service_descriptor(), MILESTONE),
        m(milestone::milestone_stream_descriptor(), MILESTONE_STREAM),
        m(workstream::workstream_service_descriptor(), WORKSTREAM),
        m(workstream::workstream_stream_descriptor(), WORKSTREAM_STREAM),
        m(task::task_service_descriptor(), TASK),
        m(task::task_stream_descriptor(), TASK_STREAM),
        m(
            timer_proto::service::timer_service_rpc_service_descriptor(),
            TIMER,
        ),
        m(timer_proto::timer_stream_descriptor(), TIMER_STREAM),
        m(
            threads::service::threads_service_rpc_service_descriptor(),
            THREADS,
        ),
        m(prefs_proto::service::prefs_service_rpc_service_descriptor(), PREFS),
        // Scheduling
        m(
            scheduling_proto::service::day_templates::day_templates_rpc_service_descriptor(),
            DAY_TEMPLATES,
        ),
        m(
            scheduling_proto::service::day_plans::day_plans_rpc_service_descriptor(),
            DAY_PLANS,
        ),
        m(
            scheduling_proto::service::calendar_events::calendar_events_rpc_service_descriptor(),
            CALENDAR_EVENTS,
        ),
        m(
            scheduling_proto::service::event_types::event_types_rpc_service_descriptor(),
            EVENT_TYPES,
        ),
        m(
            scheduling_proto::service::schedules::schedules_rpc_service_descriptor(),
            SCHEDULES,
        ),
        m(
            scheduling_proto::service::slots::slots_rpc_service_descriptor(),
            SLOTS,
        ),
        m(
            scheduling_proto::service::bookings::bookings_rpc_service_descriptor(),
            BOOKINGS,
        ),
        m(
            scheduling_proto::scheduling_events_stream_descriptor(),
            SCHEDULING_STREAM,
        ),
        // Knowledge
        m(inbox_proto::service::inbox::inbox_rpc_service_descriptor(), INBOX),
        m(inbox_proto::inbox_stream_descriptor(), INBOX_STREAM),
        m(
            recall_proto::service::recall::recall_rpc_service_descriptor(),
            RECALL,
        ),
        m(recall_proto::recall_stream_descriptor(), RECALL_STREAM),
        m(
            contacts_proto::service::contacts::contacts_rpc_service_descriptor(),
            CONTACTS,
        ),
        m(contacts_proto::contacts_stream_descriptor(), CONTACTS_STREAM),
        m(tag_proto::service::tags::tag_service_rpc_service_descriptor(), TAGS),
        m(scripture::scripture_service_descriptor(), SCRIPTURE),
        m(links::links_service_descriptor(), LINKS),
        m(collection::collection_service_descriptor(), COLLECTION),
        m(
            resources_proto::resources_service_rpc_service_descriptor(),
            RESOURCES,
        ),
        // Finance
        m(
            finance_proto::service::invoicing::invoicing_rpc_service_descriptor(),
            INVOICING,
        ),
        m(
            finance_proto::service::ledger::ledger_rpc_service_descriptor(),
            LEDGER,
        ),
        // Wiki
        m(wiki_proto::service::schema::schema_rpc_service_descriptor(), WIKI_SCHEMA),
        m(
            wiki_proto::service::catalog::catalog_rpc_service_descriptor(),
            WIKI_CATALOG,
        ),
        m(
            wiki_proto::service::raw_layer::raw_layer_rpc_service_descriptor(),
            WIKI_RAW,
        ),
        m(wiki_proto::service::graph::graph_rpc_service_descriptor(), WIKI_GRAPH),
        m(wiki_proto::service::pages::pages_rpc_service_descriptor(), WIKI_PAGES),
        m(
            wiki_proto::service::ingest::ingest_rpc_service_descriptor(),
            WIKI_INGEST,
        ),
        m(wiki_proto::service::lint::lint_rpc_service_descriptor(), WIKI_LINT),
        m(
            wiki_proto::service::search::search_rpc_service_descriptor(),
            WIKI_SEARCH,
        ),
        m(
            wiki_proto::service::events::events_stream_service_descriptor(),
            WIKI_STREAM,
        ),
        m(
            wiki_proto::service::watcher::watcher_rpc_service_descriptor(),
            WIKI_WATCHER,
        ),
        m(
            wiki_proto::service::multimodal::multimodal_rpc_service_descriptor(),
            WIKI_MULTIMODAL,
        ),
        m(
            wiki_proto::service::review::review_rpc_service_descriptor(),
            WIKI_REVIEW,
        ),
        // Home
        m(locations::locations_service_descriptor(), LOCATIONS),
        m(inventory::inventory_service_descriptor(), INVENTORY),
        m(cookbook::cookbook_service_descriptor(), COOKBOOK),
        m(mealplan::mealplan_service_descriptor(), MEALPLAN),
        m(pantry::pantry_service_descriptor(), PANTRY),
        m(
            mealplan::shopping::shopping_service_rpc_service_descriptor(),
            SHOPPING,
        ),
        m(
            mealplan::substitutions::substitution_service_rpc_service_descriptor(),
            SUBSTITUTIONS,
        ),
        m(body::body_service_descriptor(), BODY),
        m(exercises::exercises_service_descriptor(), EXERCISES),
        m(workouts::workouts_service_descriptor(), WORKOUTS),
        m(intake::intake_service_descriptor(), INTAKE),
        // Outside world
        m(email_proto::descriptor(), EMAIL),
        m(email_proto::stream_descriptor(), EMAIL_STREAM),
        m(git_proto::repo::repo_catalog_rpc_service_descriptor(), FORGE_REPOS),
        m(
            git_proto::issues::issue_tracker_rpc_service_descriptor(),
            FORGE_ISSUES,
        ),
        m(
            git_proto::reviews::review_surface_rpc_service_descriptor(),
            FORGE_REVIEWS,
        ),
        m(
            git_proto::issues::issue_tracker_stream_service_descriptor(),
            FORGE_ISSUES_STREAM,
        ),
        m(
            git_proto::reviews::review_surface_stream_service_descriptor(),
            FORGE_REVIEWS_STREAM,
        ),
        m(
            git_proto::connections::repo_connections_rpc_service_descriptor(),
            FORGE_CONNECTIONS,
        ),
    ]
}

/// Every mounted service's descriptor — the single list
/// [`crate::schema_stamps`] and the permit gate both fold over.
#[must_use]
pub fn mounted_descriptors() -> Vec<&'static ServiceDescriptor> {
    mounts().into_iter().map(|m| m.descriptor).collect()
}

/// Install every permit table on `gate`.
///
/// Registering a table makes that service's UNLISTED methods fail-closed,
/// which is why [`coverage`] must stay clean — and why it is logged at
/// boot.
#[must_use]
pub fn install(
    mut gate: architect::permissions_gate::PermissionsGate,
) -> architect::permissions_gate::PermissionsGate {
    for mount in mounts() {
        if let Some(table) = mount.permits {
            gate = gate.permit(mount.descriptor, table);
        }
    }
    gate
}

// ── Coverage ─────────────────────────────────────────────────────────────

/// What the permit tables do and do not cover. Computed statically from
/// the descriptors — no I/O, no allocation beyond the report itself.
#[derive(Debug, Default)]
pub struct Coverage {
    /// Services [`mounts`] lists.
    pub services: usize,
    /// …of which carry a permit table.
    pub tabled: usize,
    /// Mounted services with NO table (they fall through `UnlistedPolicy`).
    pub untabled: Vec<&'static str>,
    /// Methods across all mounted services.
    pub methods: usize,
    /// …of which a permit names.
    pub permitted: usize,
    /// `(service, method)` present on the descriptor but missing from its
    /// table — **fail-closed once enforcing**.
    pub uncovered: Vec<(&'static str, &'static str)>,
    /// `(service, method)` named by a table but absent from the descriptor
    /// — a typo or a renamed method; the permit is dead and the real
    /// method (if any) is silently fail-closed.
    pub phantom: Vec<(&'static str, &'static str)>,
}

impl Coverage {
    /// Nothing missing and nothing dead.
    #[must_use]
    pub fn is_complete(&self) -> bool {
        self.untabled.is_empty() && self.uncovered.is_empty() && self.phantom.is_empty()
    }
}

#[must_use]
pub fn coverage() -> Coverage {
    let mut c = Coverage::default();
    for mount in mounts() {
        c.services += 1;
        c.methods += mount.descriptor.methods.len();
        let name = mount.descriptor.service_name;
        let Some(table) = mount.permits else {
            c.untabled.push(name);
            continue;
        };
        c.tabled += 1;
        for method in mount.descriptor.methods {
            if table.methods.iter().any(|p| p.method == method.method_name) {
                c.permitted += 1;
            } else {
                c.uncovered.push((name, method.method_name));
            }
        }
        for permit in table.methods {
            if !mount
                .descriptor
                .methods
                .iter()
                .any(|d| d.method_name == permit.method)
            {
                c.phantom.push((name, permit.method));
            }
        }
    }
    c
}

// ── Dry run ──────────────────────────────────────────────────────────────

/// "If enforcement were on right now, what would break?" — answered
/// statically by replaying every permit through the live engine with two
/// synthetic principals. Pure in-memory glob checks; cheap enough to run
/// at every boot.
#[derive(Debug, Default)]
pub struct DryRun {
    /// Methods a signed-in member may call.
    pub member_allowed: usize,
    /// `(service, method, reason)` a signed-in member may NOT call — the
    /// list that has to be empty (or deliberate) before enforcing.
    pub member_denied: Vec<(&'static str, &'static str, String)>,
    /// `(service, method)` callable with NO session at all — the
    /// `public/**` surface. Must be exactly the auth + permissions
    /// oracle methods.
    pub anonymous_allowed: Vec<(&'static str, &'static str)>,
    /// Methods that would be denied for EVERY principal because their
    /// service has a table but they are not in it.
    pub fail_closed: usize,
}

/// The user id used for the "ordinary signed-in user" probe. Any id works
/// — `RoleEngine::with_default_user_role("member")` maps unknown-but-
/// validated users to `member`, which is what every real user is today.
const PROBE_USER: &str = "dry-run-probe";

#[must_use]
pub fn dry_run(engine: &dyn PermissionEngine) -> DryRun {
    let member = Principal::User {
        user_id: PROBE_USER.to_owned(),
    };
    let anon = Principal::Anonymous;
    let mut out = DryRun::default();
    for mount in mounts() {
        let name = mount.descriptor.service_name;
        let Some(table) = mount.permits else { continue };
        for method in mount.descriptor.methods {
            let Some(permit) = table
                .methods
                .iter()
                .find(|p| p.method == method.method_name)
            else {
                out.fail_closed += 1;
                continue;
            };
            let resource = permit.coarse_resource();
            let action = Action::new(permit.action);
            match engine.check(&member, &resource, &action) {
                architect_permissions::Decision::Allow => out.member_allowed += 1,
                architect_permissions::Decision::Deny { reason } => {
                    out.member_denied.push((name, permit.method, reason));
                }
            }
            if engine.check(&anon, &resource, &action).allowed() {
                out.anonymous_allowed.push((name, permit.method));
            }
        }
    }
    out
}

/// Boot-time report — one summary line per org plus a warn line for every
/// gap. Called from [`crate::OrgAppState::new`] after the gate is built.
pub fn log_coverage(slug: &str, gate: &architect::permissions_gate::PermissionsGate, enforce: bool) {
    let c = coverage();
    let d = dry_run(gate.engine().as_ref());
    let mode = if enforce { "ENFORCING" } else { "observe-only" };
    tracing::info!(
        org = %slug,
        mode,
        services = c.services,
        tabled = c.tabled,
        methods = c.methods,
        permitted = c.permitted,
        member_allowed = d.member_allowed,
        anonymous_allowed = d.anonymous_allowed.len(),
        "permissions gate: {}/{} services have permit tables ({}/{} methods)",
        c.tabled,
        c.services,
        c.permitted,
        c.methods,
    );
    if !c.untabled.is_empty() {
        tracing::warn!(
            org = %slug,
            count = c.untabled.len(),
            services = %c.untabled.join(", "),
            "permissions gate: services with NO permit table — unchecked (UnlistedPolicy::Allow)",
        );
    }
    if !c.uncovered.is_empty() {
        tracing::warn!(
            org = %slug,
            count = c.uncovered.len(),
            methods = %join_pairs(&c.uncovered),
            "permissions gate: methods missing from their service's table — FAIL-CLOSED once enforcing",
        );
    }
    if !c.phantom.is_empty() {
        tracing::error!(
            org = %slug,
            count = c.phantom.len(),
            methods = %join_pairs(&c.phantom),
            "permissions gate: permits naming methods that do not exist (typo/rename) — dead rules",
        );
    }
    if !d.member_denied.is_empty() {
        tracing::warn!(
            org = %slug,
            count = d.member_denied.len(),
            methods = %d.member_denied.iter().map(|(s, m, _)| format!("{s}/{m}")).collect::<Vec<_>>().join(", "),
            "permissions gate dry-run: a signed-in member would be DENIED these — do not enforce until this is intentional",
        );
    }
    tracing::info!(
        org = %slug,
        methods = %join_pairs(&d.anonymous_allowed),
        "permissions gate dry-run: reachable without a session (public surface)",
    );
}

fn join_pairs(v: &[(&'static str, &'static str)]) -> String {
    v.iter()
        .map(|(s, m)| format!("{s}/{m}"))
        .collect::<Vec<_>>()
        .join(", ")
}

// ── Would-deny ledger + audit sink ───────────────────────────────────────

/// One distinct would-be denial, with a hit count.
#[derive(Debug, Clone, Default)]
pub struct DenyCount {
    pub count: u64,
    pub reason: String,
}

/// Bounded tally of everything the gate refused (or WOULD have refused, in
/// observe-only). This is the runtime half of the dry-run: the static
/// [`dry_run`] says what the rules imply, the ledger says what real
/// clients actually did.
#[derive(Debug, Default)]
pub struct DenyLedger {
    inner: Mutex<LedgerInner>,
}

#[derive(Debug, Default)]
struct LedgerInner {
    entries: HashMap<String, DenyCount>,
    /// Denials dropped because [`DenyLedger::CAP`] distinct reasons were
    /// already recorded.
    overflow: u64,
}

impl DenyLedger {
    /// Distinct reasons kept. Past this the tally still counts, it just
    /// stops growing — a gate log flooded by one broken client must not
    /// become a memory leak.
    pub const CAP: usize = 512;

    fn record(&self, reason: &str) {
        let Ok(mut g) = self.inner.lock() else { return };
        if let Some(e) = g.entries.get_mut(reason) {
            e.count += 1;
            return;
        }
        if g.entries.len() >= Self::CAP {
            g.overflow += 1;
            return;
        }
        g.entries.insert(
            reason.to_owned(),
            DenyCount {
                count: 1,
                reason: reason.to_owned(),
            },
        );
    }

    /// Snapshot, most-frequent first.
    #[must_use]
    pub fn snapshot(&self) -> (Vec<DenyCount>, u64) {
        let Ok(g) = self.inner.lock() else {
            return (Vec::new(), 0);
        };
        let mut v: Vec<DenyCount> = g.entries.values().cloned().collect();
        v.sort_by(|a, b| b.count.cmp(&a.count));
        (v, g.overflow)
    }
}

/// The org lane's [`AuditSink`]: makes observe-only mode *informative*.
///
/// The gate emits two events for an observe-only denial (see
/// `architect::permissions_gate`):
///
/// 1. the real decision — `allowed: false`, carrying principal, coarse
///    resource, action and the engine's reason (which embeds the
///    principal, e.g. `user:abc is not a member`);
/// 2. a marker — `resource: "gate"`, `action: "would-deny"`, **`allowed:
///    true`**, whose `reason` is the full deny message *including
///    `(service/method)`*.
///
/// The stock [`architect_permissions::TracingAudit`] logs (2) as
/// `info!("permission allow")` and **drops the reason entirely** — so the
/// one line that names the service and method never reaches the operator.
/// This sink logs it at WARN with the reason intact and tallies it in the
/// [`DenyLedger`], which is what makes the observe-only rollout auditable.
pub struct GateAudit {
    enforcing: bool,
    ledger: Arc<DenyLedger>,
    /// Role the org's engine assigns any validated user — mirrors
    /// `RoleEngine::with_default_user_role` in
    /// `crate::build_org_permissions_gate`. Logged so a deny line says
    /// which rule set was consulted (`AuditEvent` carries no role field).
    default_role: &'static str,
}

/// The marker resource/action the gate uses for observe-only would-denies.
const GATE_MARKER_RESOURCE: &str = "gate";
const GATE_MARKER_ACTION: &str = "would-deny";

impl GateAudit {
    #[must_use]
    pub fn new(enforcing: bool, ledger: Arc<DenyLedger>, default_role: &'static str) -> Self {
        Self {
            enforcing,
            ledger,
            default_role,
        }
    }
}

impl AuditSink for GateAudit {
    fn record(&self, e: AuditEvent) {
        let reason = e.reason.unwrap_or_default();
        if e.resource == GATE_MARKER_RESOURCE && e.action == GATE_MARKER_ACTION {
            // Observe-only marker: the ONLY event carrying service/method.
            self.ledger.record(&reason);
            tracing::warn!(
                target: "task_server::permissions",
                mode = "observe-only",
                default_role = self.default_role,
                reason = %reason,
                "permissions gate: WOULD DENY (allowed through — enforcement is off)",
            );
            return;
        }
        if !e.allowed {
            self.ledger.record(&reason);
            let mode = if self.enforcing {
                "enforcing"
            } else {
                "observe-only"
            };
            // In observe-only this pairs with the marker line above; when
            // enforcing it is the whole record (the gate emits no marker),
            // so it carries the principal/resource/action explicitly.
            tracing::warn!(
                target: "task_server::permissions",
                mode,
                principal = %e.principal,
                default_role = self.default_role,
                resource = %e.resource,
                action = %e.action,
                reason = %reason,
                "permissions gate: DENY",
            );
            return;
        }
        tracing::info!(
            target: "task_server::permissions",
            principal = %e.principal,
            resource = %e.resource,
            action = %e.action,
            "permissions gate: audited allow",
        );
    }
}

// ── Operator report ──────────────────────────────────────────────────────

/// The JSON body of `GET /server/permissions` — everything an operator
/// needs to answer "what breaks if I set `TASK_ENFORCE_PERMISSIONS=1`".
#[must_use]
pub fn report_json(
    enforce: bool,
    engine: &dyn PermissionEngine,
    ledger: &DenyLedger,
) -> serde_json::Value {
    let c = coverage();
    let d = dry_run(engine);
    let (denies, overflow) = ledger.snapshot();
    serde_json::json!({
        "mode": if enforce { "enforcing" } else { "observe-only" },
        "enforce_env": "TASK_ENFORCE_PERMISSIONS",
        "coverage": {
            "services": c.services,
            "tabled": c.tabled,
            "untabled": c.untabled,
            "methods": c.methods,
            "permitted": c.permitted,
            "uncovered": c.uncovered.iter().map(|(s, m)| format!("{s}/{m}")).collect::<Vec<_>>(),
            "phantom": c.phantom.iter().map(|(s, m)| format!("{s}/{m}")).collect::<Vec<_>>(),
            "complete": c.is_complete(),
        },
        "dry_run": {
            "member_allowed": d.member_allowed,
            "member_denied": d.member_denied.iter()
                .map(|(s, m, r)| serde_json::json!({ "method": format!("{s}/{m}"), "reason": r }))
                .collect::<Vec<_>>(),
            "anonymous_allowed": d.anonymous_allowed.iter()
                .map(|(s, m)| format!("{s}/{m}")).collect::<Vec<_>>(),
            "fail_closed": d.fail_closed,
        },
        "observed_denials": {
            "distinct": denies.len(),
            "dropped_over_cap": overflow,
            "top": denies.iter().take(50)
                .map(|d| serde_json::json!({ "count": d.count, "reason": d.reason }))
                .collect::<Vec<_>>(),
        },
    })
}

/// Human-readable coverage summary — `task doctor`'s permissions check.
/// Static (no server, no network): it folds the same tables the server
/// would install.
#[must_use]
pub fn coverage_summary() -> String {
    let c = coverage();
    let mut out = format!(
        "Permissions coverage: {}/{} services tabled, {}/{} methods permitted",
        c.tabled, c.services, c.permitted, c.methods
    );
    if !c.untabled.is_empty() {
        out.push_str(&format!(
            "\n  UNGATED services ({}): {}",
            c.untabled.len(),
            c.untabled.join(", ")
        ));
    }
    if !c.uncovered.is_empty() {
        out.push_str(&format!(
            "\n  FAIL-CLOSED methods once enforcing ({}): {}",
            c.uncovered.len(),
            join_pairs(&c.uncovered)
        ));
    }
    if !c.phantom.is_empty() {
        out.push_str(&format!(
            "\n  DEAD permits (method not on the descriptor) ({}): {}",
            c.phantom.len(),
            join_pairs(&c.phantom)
        ));
    }
    if c.is_complete() {
        out.push_str("\n  OK — every mounted service and method is covered.");
    }
    out
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Every mounted service has a table, every descriptor method has a
    /// permit, and no permit names a method that does not exist. This is
    /// the guard that keeps the 2/71 gap from reappearing: add a service
    /// to `org_layer_router` without a table and this fails.
    #[test]
    fn coverage_is_complete() {
        let c = coverage();
        assert!(
            c.untabled.is_empty(),
            "mounted services with no permit table: {:?}",
            c.untabled
        );
        assert!(
            c.uncovered.is_empty(),
            "methods missing a permit (fail-closed once enforcing): {:?}",
            c.uncovered
        );
        assert!(
            c.phantom.is_empty(),
            "permits naming non-existent methods: {:?}",
            c.phantom
        );
        assert_eq!(c.services, c.tabled);
    }

    /// The dry-run against the real org engine: an ordinary signed-in
    /// member must be able to call EVERYTHING, and only the deliberate
    /// `public/**` surface may be reachable anonymously. If this fails,
    /// flipping `TASK_ENFORCE_PERMISSIONS=1` would lock users out.
    #[test]
    fn member_may_call_everything_and_public_surface_is_minimal() {
        let engine = crate::org_permission_engine();
        let d = dry_run(engine.as_ref());
        assert!(
            d.member_denied.is_empty(),
            "a signed-in member would be denied: {:?}",
            d.member_denied
        );
        assert_eq!(d.fail_closed, 0);
        let public: Vec<&str> = d
            .anonymous_allowed
            .iter()
            .map(|(s, _)| *s)
            .collect::<std::collections::BTreeSet<_>>()
            .into_iter()
            .collect();
        assert_eq!(
            public,
            vec!["AuthService", "PermissionsService"],
            "the anonymous surface changed — every entry must be a service \
             that authenticates its own callers",
        );
    }

    /// No `admin` permits: nobody holds the `owner` role on the org lane
    /// today (no `set_member` calls), so an admin permit is a lockout.
    #[test]
    fn no_admin_permits() {
        for mount in mounts() {
            let Some(table) = mount.permits else { continue };
            for p in table.methods {
                assert_ne!(
                    p.action,
                    Action::ADMIN,
                    "{}/{} requires `admin`, which no user holds",
                    table.service,
                    p.method
                );
            }
        }
    }

    #[test]
    fn ledger_counts_and_caps() {
        let l = DenyLedger::default();
        l.record("a");
        l.record("a");
        l.record("b");
        let (v, over) = l.snapshot();
        assert_eq!(v.len(), 2);
        assert_eq!(v[0].count, 2);
        assert_eq!(over, 0);
        for i in 0..DenyLedger::CAP {
            l.record(&format!("r{i}"));
        }
        let (v, over) = l.snapshot();
        assert_eq!(v.len(), DenyLedger::CAP);
        assert!(over > 0);
    }
}
