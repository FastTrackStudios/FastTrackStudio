//! Org-wide presence — Discord-style status + activity roster.
//!
//! One presence channel per org, riding the app's shared per-org vox
//! connection (`crdt::use_presence_channel` over the fixed
//! [`PRESENCE_DOC_ID`]). Three layers:
//!
//! - **Live peers** — every open app publishes a [`PresenceEntry`]
//!   under a per-session client key (a minted Uuid). The entry tracks
//!   the current route ("viewing Projects"), the manual status from
//!   the picker, and a simple idle heuristic. Values ride Loro's
//!   `EphemeralStore` and expire on their own ~30s after a client
//!   goes quiet — there is no explicit leave.
//! - **Derived entries** — agents and time-trackers that don't run
//!   this UI still show up: the roster polls
//!   [`crate::feeds::fetch_agent_sessions`] (sessions in
//!   `Running` / `AwaitingUser`) and open timer sessions every 60s
//!   and merges them in, de-duped against live entries by name.
//! - **UI** — [`PresenceRoster`] (sidebar section),
//!   [`PresenceStatusPicker`] (DND/Available picker + display name,
//!   next to the org switcher), and [`ProjectPresenceStrip`] (who's
//!   on this project, for `/projects/:id`).
//!
//! Wiring order matters: [`provide_org_presence`] must run **after**
//! the app root's `use_app_reactive` (the channel hook pulls the
//! shared `Connection<vox::Caller>` from context) and **before** the
//! router mount (everything below consumes the provided contexts).
//!
//! Sharp edge, inherited from the framework: never sweep/prune the
//! presence store from a render path — `use_presence_channel` already
//! runs the expiry sweep in a task; this module only ever calls
//! `states()` / `set()`.

use std::collections::{HashMap, HashSet};

use crdt::loro::LoroValue;
use dioxus::prelude::*;
use fts_ui::prelude::*;
use uuid::Uuid;

use crate::orgs::{OrgMeta, OrgSelection, selected_slugs};
use crate::routes::Route;

/// The org-wide presence channel's doc id — ASCII `task-org-presenc`.
/// Fixed: each org has its own `LayerRouter`/`PresenceHost`, so the
/// same id on different orgs never crosses streams.
///
/// NOTE: **duplicated** in `apps/server/src/presence.rs`
/// (`task_server::presence::PRESENCE_DOC_ID`) — the UI crate can't
/// depend on the server binary and no shared proto crate fits a
/// transport-level constant today. Keep the two in sync.
pub const PRESENCE_DOC_ID: Uuid = Uuid::from_u128(0x7461736b_2d6f_7267_2d70_726573656e63);

/// Republish cadence for the heartbeat — a third of the timeout, so a
/// healthy tab refreshes its entry well before expiry while a closed
/// one drops within ~one timeout.
const HEARTBEAT_MS: u64 = PRESENCE_TIMEOUT_MS as u64 / 3;

/// Ephemeral timeout — mirrors the server host's. A peer that goes
/// quiet for this long vanishes from everyone's roster.
pub const PRESENCE_TIMEOUT_MS: i64 = 30_000;

/// No route change for this long ⇒ idle. Deliberately simple v1: real
/// input-level activity tracking (mousemove/keydown listeners) is a
/// follow-up; route navigation is the one cross-platform signal we
/// already have.
const IDLE_AFTER_MS: i64 = 5 * 60 * 1_000;

/// localStorage key for the persisted display name (web only; native
/// falls back to the default each launch until auth lands).
#[cfg(target_arch = "wasm32")]
const NAME_STORAGE_KEY: &str = "task.presence.name";
const DEFAULT_NAME: &str = "anonymous";

// ── typed payload ───────────────────────────────────────────────────

/// Who's publishing — a human at a UI, or an agent.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum PresenceKind {
    Human,
    Agent,
}

impl PresenceKind {
    fn as_str(self) -> &'static str {
        match self {
            Self::Human => "human",
            Self::Agent => "agent",
        }
    }

    fn parse(s: &str) -> Option<Self> {
        match s {
            "human" => Some(Self::Human),
            "agent" => Some(Self::Agent),
            _ => None,
        }
    }
}

/// Discord-style status. `Active`/`Idle` are automatic; `Dnd`/
/// `Available` are manual picker overrides.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum PresenceStatus {
    Active,
    Idle,
    Dnd,
    Available,
}

impl PresenceStatus {
    fn as_str(self) -> &'static str {
        match self {
            Self::Active => "active",
            Self::Idle => "idle",
            Self::Dnd => "dnd",
            Self::Available => "available",
        }
    }

    fn parse(s: &str) -> Option<Self> {
        match s {
            "active" => Some(Self::Active),
            "idle" => Some(Self::Idle),
            "dnd" => Some(Self::Dnd),
            "available" => Some(Self::Available),
            _ => None,
        }
    }

    /// Tailwind class for the status dot.
    fn dot_class(self) -> &'static str {
        match self {
            Self::Active => "bg-emerald-500",
            Self::Idle => "bg-amber-400",
            Self::Dnd => "bg-red-500",
            Self::Available => "bg-sky-500",
        }
    }

    fn label(self) -> &'static str {
        match self {
            Self::Active => "Active",
            Self::Idle => "Idle",
            Self::Dnd => "Do not disturb",
            Self::Available => "Available",
        }
    }
}

/// One roster row — the typed shape every presence value carries on
/// the wire (as a `LoroValue::Map`; see [`Self::encode`]).
#[derive(Clone, Debug, PartialEq)]
pub struct PresenceEntry {
    /// Display name (de-dupe key against derived entries).
    pub name: String,
    pub kind: PresenceKind,
    pub status: PresenceStatus,
    /// What they're doing — "viewing Projects", a pending agent turn…
    pub activity: Option<String>,
    /// Project in focus (`/projects/:id`, or an agent/timer binding).
    pub project_id: Option<Uuid>,
    /// Task in focus (`/tasks/:id`).
    pub task_id: Option<Uuid>,
    /// Unix ms when this presence began (session join / turn start).
    pub since_ms: i64,
}

impl PresenceEntry {
    /// Encode as a `LoroValue::Map`. `None` fields are simply absent —
    /// decode treats missing keys as `None`.
    #[must_use]
    pub fn encode(&self) -> LoroValue {
        let mut m: Vec<(String, LoroValue)> = vec![
            ("name".into(), LoroValue::from(self.name.clone())),
            ("kind".into(), LoroValue::from(self.kind.as_str())),
            ("status".into(), LoroValue::from(self.status.as_str())),
            ("since_ms".into(), LoroValue::from(self.since_ms)),
        ];
        if let Some(activity) = &self.activity {
            m.push(("activity".into(), LoroValue::from(activity.clone())));
        }
        if let Some(id) = self.project_id {
            m.push(("project_id".into(), LoroValue::from(id.to_string())));
        }
        if let Some(id) = self.task_id {
            m.push(("task_id".into(), LoroValue::from(id.to_string())));
        }
        LoroValue::Map(m.into())
    }

    /// Decode a peer's value. Tolerant: `name` + parseable `kind`/
    /// `status` are required, everything else defaults — an unknown or
    /// malformed value (newer schema, foreign publisher) yields `None`
    /// and the roster just skips it.
    #[must_use]
    pub fn decode(value: &LoroValue) -> Option<Self> {
        let LoroValue::Map(map) = value else {
            return None;
        };
        let get_str = |key: &str| -> Option<String> {
            match map.get(key) {
                Some(LoroValue::String(s)) => Some(s.to_string()),
                _ => None,
            }
        };
        let name = get_str("name")?;
        let kind = PresenceKind::parse(&get_str("kind")?)?;
        let status = PresenceStatus::parse(&get_str("status")?)?;
        let since_ms = match map.get("since_ms") {
            Some(LoroValue::I64(v)) => *v,
            _ => 0,
        };
        Some(Self {
            name,
            kind,
            status,
            activity: get_str("activity").filter(|s| !s.is_empty()),
            project_id: get_str("project_id").and_then(|s| Uuid::parse_str(&s).ok()),
            task_id: get_str("task_id").and_then(|s| Uuid::parse_str(&s).ok()),
            since_ms,
        })
    }
}

// ── local identity + channel wiring ─────────────────────────────────

/// What the status picker drives. `Auto` lets the idle heuristic pick
/// `Active`/`Idle`; the other two are explicit overrides.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ManualStatus {
    Auto,
    Available,
    Dnd,
}

/// This client's presence identity — provided at the app root by
/// [`provide_org_presence`], consumed by the publisher + picker.
#[derive(Clone, Copy)]
pub struct PresenceLocal {
    /// Display name, persisted to localStorage (`task.presence.name`).
    pub name: Signal<String>,
    /// Manual status override from the picker.
    pub manual: Signal<ManualStatus>,
    /// Idle flag, owned by the publisher's ticker.
    pub idle: Signal<bool>,
    /// This session's presence key — a Uuid minted per app launch.
    pub client_key: Signal<String>,
}

impl PresenceLocal {
    /// The status the publisher writes right now.
    fn effective_status(&self) -> PresenceStatus {
        match *self.manual.read() {
            ManualStatus::Dnd => PresenceStatus::Dnd,
            ManualStatus::Available => PresenceStatus::Available,
            ManualStatus::Auto => {
                if *self.idle.read() {
                    PresenceStatus::Idle
                } else {
                    PresenceStatus::Active
                }
            }
        }
    }
}

/// Join the org-wide presence channel and provide the identity
/// contexts. Call once at the app root, **after** the connection
/// provider (`use_app_reactive`) — the channel hook reads the shared
/// `Connection<vox::Caller>` from context — and before the router.
pub fn provide_org_presence() -> crdt::Presence {
    let name = use_signal(load_saved_name);
    let manual = use_signal(|| ManualStatus::Auto);
    let idle = use_signal(|| false);
    let client_key = use_signal(stable_client_key);
    use_context_provider(|| PresenceLocal {
        name,
        manual,
        idle,
        client_key,
    });
    crdt::use_presence_channel(PRESENCE_DOC_ID, PRESENCE_TIMEOUT_MS)
}

/// Publish this client's [`PresenceEntry`] — mount once inside the
/// router layout (it needs `use_route`). Renders nothing. Re-publishes
/// on route change, name edit, picker change, and idle flips; the
/// presence driver handles re-announce after reconnects.
#[component]
pub fn PresencePublisher() -> Element {
    let route = use_route::<Route>();
    let title = crate::nav::route_title(&route).to_string();
    let (project_id, task_id) = match &route {
        Route::ProjectDetailRoute { id } => (Uuid::parse_str(id).ok(), None),
        Route::TaskDetailRoute { id } => (None, Some(*id)),
        _ => (None, None),
    };
    // ReadSignal props make the route parts reactive inside the
    // inner component's effects (plain props wouldn't re-run them).
    rsx! {
        PublisherEffect { title, project_id, task_id }
    }
}

#[component]
fn PublisherEffect(
    title: ReadSignal<String>,
    project_id: ReadSignal<Option<Uuid>>,
    task_id: ReadSignal<Option<Uuid>>,
) -> Element {
    let presence = crdt::use_presence();
    let local = use_context::<PresenceLocal>();
    let joined_ms = use_hook(now_ms);
    let mut last_nav = use_signal(now_ms);

    // Every navigation counts as activity: stamp it and clear idle.
    use_effect(move || {
        let _ = title.read();
        let _ = project_id.read();
        let _ = task_id.read();
        last_nav.set(now_ms());
        let mut idle = local.idle;
        if *idle.peek() {
            idle.set(false);
        }
    });

    // Idle ticker — v1 heuristic: no route change for 5 minutes.
    use_future(move || async move {
        loop {
            architect::sleep(architect::Duration::from_secs(30)).await;
            let is_idle = now_ms() - *last_nav.peek() >= IDLE_AFTER_MS;
            let mut idle = local.idle;
            if *idle.peek() != is_idle {
                idle.set(is_idle);
            }
        }
    });

    // The publisher: re-runs when any read signal changes (route parts,
    // name, manual status, idle, and the channel's peer slot becoming
    // ready — `presence.set` reads it). `beat` is the heartbeat: an
    // ephemeral entry expires PRESENCE_TIMEOUT_MS after its last
    // publish, so an idle-but-open tab must republish (fresh
    // timestamp, same content) or it falls off everyone's roster.
    let mut beat = use_signal(|| 0u64);
    use_future(move || async move {
        loop {
            architect::sleep(architect::Duration::from_millis(HEARTBEAT_MS)).await;
            beat += 1;
        }
    });
    use_effect(move || {
        let _ = beat();
        let name = display_name(&local.name.read());
        let entry = PresenceEntry {
            name,
            kind: PresenceKind::Human,
            status: local.effective_status(),
            activity: Some(format!("viewing {}", title.read())),
            project_id: *project_id.read(),
            task_id: *task_id.read(),
            since_ms: joined_ms,
        };
        let key = local.client_key.peek().clone();
        presence.set(&key, entry.encode());
    });

    rsx! {}
}

// ── roster ──────────────────────────────────────────────────────────

/// Sidebar roster: live peers + derived agent/timer entries, humans
/// first, with status dots and the current activity.
#[component]
pub fn PresenceRoster() -> Element {
    let presence = crdt::use_presence();
    let selection = use_context::<Signal<OrgSelection>>();
    let org_list = use_context::<Signal<Vec<OrgMeta>>>();

    // Derived layer: poll agent sessions + open timers every 60s
    // (and on org-selection change — the resource reads both signals).
    let mut tick = use_signal(|| 0u64);
    use_future(move || async move {
        loop {
            architect::sleep(architect::Duration::from_secs(60)).await;
            tick += 1;
        }
    });
    let derived = use_resource(move || {
        let _ = tick();
        let slugs = selected_slugs(&selection.read(), &org_list.read());
        async move { fetch_derived_entries(&slugs).await }
    });

    // Live peers (reactive — `states()` re-renders on every presence
    // change), then merge derived rows de-duped by name. Render keys:
    // live rows use the peer's store key, derived rows a synthetic
    // position-stable key — names are NOT unique.
    let mut entries: Vec<(String, PresenceEntry)> = decode_states(&presence.states());
    let live_names: HashSet<String> = entries.iter().map(|(_, e)| e.name.clone()).collect();
    if let Some(rows) = &*derived.read_unchecked() {
        entries.extend(
            rows.iter()
                .filter(|e| !live_names.contains(&e.name))
                .enumerate()
                .map(|(i, e)| (format!("derived-{i}-{}", e.name), e.clone())),
        );
    }
    sort_entries(&mut entries);
    let count = entries.len();

    rsx! {
        SidebarGroup {
            SidebarGroupLabel { "Online — {count}" }
            SidebarGroupContent {
                if entries.is_empty() {
                    div { class: "px-2 py-1 text-xs text-muted-foreground", "Nobody around right now" }
                } else {
                    for (row_key, entry) in entries {
                        PresenceRow { key: "{row_key}", entry }
                    }
                }
            }
        }
    }
}

/// One roster row: status dot, name, muted activity line.
#[component]
fn PresenceRow(entry: PresenceEntry) -> Element {
    let dot = entry.status.dot_class();
    let title = entry.status.label();
    let kind_badge = match entry.kind {
        PresenceKind::Human => None,
        PresenceKind::Agent => Some("bot"),
    };
    rsx! {
        div { class: "flex items-center gap-2 rounded-md px-2 py-1",
            span {
                class: "h-2 w-2 shrink-0 rounded-full {dot}",
                title: "{title}",
            }
            div { class: "flex min-w-0 flex-col",
                div { class: "flex items-center gap-1.5",
                    span { class: "truncate text-xs font-medium text-foreground", "{entry.name}" }
                    if let Some(badge) = kind_badge {
                        span { class: "rounded bg-muted px-1 text-[10px] uppercase tracking-wide text-muted-foreground",
                            "{badge}"
                        }
                    }
                }
                if let Some(activity) = entry.activity {
                    span { class: "truncate text-[11px] text-muted-foreground", "{activity}" }
                }
            }
        }
    }
}

/// Inline strip for `/projects/:id` — live peers whose entry points at
/// this project. Renders nothing when nobody's here.
///
/// Exported but not yet mounted in `pages/project_detail.rs` (that
/// file is owned by a concurrent change); wire it as
/// `ProjectPresenceStrip { project_id }` near the page header.
#[component]
pub fn ProjectPresenceStrip(project_id: Uuid) -> Element {
    let presence = crdt::use_presence();
    let mut here: Vec<(String, PresenceEntry)> = decode_states(&presence.states())
        .into_iter()
        .filter(|(_, e)| e.project_id == Some(project_id))
        .collect();
    sort_entries(&mut here);
    if here.is_empty() {
        return rsx! {};
    }
    let count = here.len();
    rsx! {
        div { class: "flex flex-wrap items-center gap-2 rounded-lg border border-border bg-card/40 px-2.5 py-1 text-xs text-muted-foreground",
            span { if count == 1 { "1 person here" } else { "{count} people here" } }
            for (peer_key, entry) in here {
                span { key: "{peer_key}", class: "flex items-center gap-1",
                    span { class: "h-1.5 w-1.5 rounded-full {entry.status.dot_class()}" }
                    span { class: "text-foreground", "{entry.name}" }
                }
            }
        }
    }
}

// ── status picker ───────────────────────────────────────────────────

/// Manual status + display name, sidebar-footer sized (lives next to
/// the org switcher). The dot on the trigger shows the *effective*
/// status (idle wins over Auto).
#[component]
pub fn PresenceStatusPicker() -> Element {
    let local = use_context::<PresenceLocal>();
    let mut name = local.name;
    let mut manual = local.manual;
    let mut open = use_signal(|| false);

    let effective = local.effective_status();
    let dot = effective.dot_class();
    let current = *manual.read();
    let options = [
        (ManualStatus::Auto, "Active (auto)", PresenceStatus::Active),
        (
            ManualStatus::Available,
            "Available",
            PresenceStatus::Available,
        ),
        (ManualStatus::Dnd, "Do not disturb", PresenceStatus::Dnd),
    ];

    // The input edits a local draft; the committed name (what the
    // publisher effect reads and what hits localStorage) follows with
    // a debounce. Committing per keystroke republished presence on
    // every character — the resulting re-render storm raced the
    // controlled input and snapped deletions back.
    let mut draft = use_signal(|| name.peek().clone());
    let mut commit_gen = use_signal(|| 0u64);
    use_future(move || async move {
        let mut seen = 0u64;
        loop {
            architect::sleep(architect::Duration::from_millis(400)).await;
            let gen = *commit_gen.peek();
            if gen != seen {
                seen = gen;
                let v = draft.peek().clone();
                if v != *name.peek() {
                    save_name(&v);
                    name.set(v);
                }
            }
        }
    });
    let mut commit_now = move || {
        let v = draft.peek().clone();
        if v != *name.peek() {
            save_name(&v);
            name.set(v);
        }
    };

    rsx! {
        HStack { gap: "1", class: "items-center w-full",
            input {
                class: "min-w-0 flex-1 rounded-full border border-border bg-card px-2.5 py-1.5 text-xs text-foreground outline-none placeholder:text-muted-foreground focus-visible:border-ring",
                placeholder: "Your name",
                value: "{draft}",
                oninput: move |e| {
                    draft.set(e.value());
                    commit_gen += 1;
                },
                onblur: move |_| commit_now(),
                onkeydown: move |e| {
                    if e.key() == Key::Enter {
                        commit_now();
                    }
                },
            }
            Dropdown {
                open: open(),
                on_open_change: move |o| open.set(o),
                DropdownTrigger {
                    button {
                        r#type: "button",
                        class: "flex items-center gap-1.5 rounded-full border border-border bg-card px-2.5 py-1.5 text-xs font-semibold text-foreground hover:bg-accent",
                        title: "Presence status",
                        span { class: "h-2 w-2 rounded-full {dot}" }
                        span { "{effective.label()}" }
                    }
                }
                DropdownContent { side: "top", align: "end", width: "w-48",
                    DropdownLabel { "Status" }
                    for (idx, (value, label, status)) in options.into_iter().enumerate() {
                        DropdownItem {
                            key: "{label}",
                            value: label.to_string(),
                            index: idx,
                            on_select: move |_| {
                                manual.set(value);
                                open.set(false);
                            },
                            div { class: "flex w-full items-center justify-between gap-2",
                                span { class: "flex items-center gap-2",
                                    span { class: "h-2 w-2 rounded-full {status.dot_class()}" }
                                    span { "{label}" }
                                }
                                if current == value {
                                    span { class: "text-xs text-primary", "●" }
                                }
                            }
                        }
                    }
                }
            }
        }
    }
}

// ── derived entries (no-heartbeat participants) ─────────────────────

/// Roster rows for participants that don't publish presence
/// themselves: agent sessions mid-turn and open timer sessions.
/// Best-effort — a down org just contributes nothing.
async fn fetch_derived_entries(slugs: &[String]) -> Vec<PresenceEntry> {
    use agent_proto::session::SessionStatus;

    let mut out = Vec::new();

    if let Ok(sessions) = crate::feeds::fetch_agent_sessions(slugs).await {
        for (_slug, s) in sessions {
            if s.archived
                || !matches!(
                    s.status,
                    SessionStatus::Running | SessionStatus::AwaitingUser
                )
            {
                continue;
            }
            let label = if s.subagent_nickname.trim().is_empty() {
                s.title.trim()
            } else {
                s.subagent_nickname.trim()
            };
            let name = if label.is_empty() {
                "Agent".to_string()
            } else {
                format!("Agent {label}")
            };
            let what = s
                .pending
                .as_ref()
                .map(|p| p.user_message.trim().to_string())
                .filter(|m| !m.is_empty())
                .unwrap_or_else(|| s.project_id.clone());
            let since_ms = s
                .pending
                .as_ref()
                .map_or(s.updated_at, |p| p.started_at)
                .timestamp_millis();
            out.push(PresenceEntry {
                name,
                kind: PresenceKind::Agent,
                status: PresenceStatus::Active,
                activity: (!what.is_empty()).then(|| truncate(&what, 80)),
                project_id: Uuid::parse_str(&s.project_id).ok(),
                task_id: None,
                since_ms,
            });
        }
    }

    for slug in slugs {
        let Ok(sessions) = crate::feeds::fetch_org_sessions(slug).await else {
            continue;
        };
        for s in sessions.into_iter().filter(|s| s.end_time.is_none()) {
            let desc = s.description.trim().to_string();
            let name = if desc.is_empty() {
                format!("Timer ({slug})")
            } else {
                format!("Timer — {}", truncate(&desc, 40))
            };
            out.push(PresenceEntry {
                name,
                kind: PresenceKind::Human,
                status: PresenceStatus::Active,
                activity: Some(if desc.is_empty() {
                    "tracking time".to_string()
                } else {
                    format!("tracking: {}", truncate(&desc, 60))
                }),
                project_id: s.project_id,
                task_id: None,
                since_ms: s.start_time.timestamp_millis(),
            });
        }
    }

    out
}

// ── helpers ─────────────────────────────────────────────────────────

/// Decode every peer's state, skipping anything that isn't a valid
/// [`PresenceEntry`]. Pure read — no pruning here (render path!).
/// Decode live peers, keeping each peer's store key (the per-session
/// client uuid) — render keys MUST come from it, never from the
/// display name: two tabs both named "anonymous" are distinct peers,
/// and duplicate sibling keys panic the renderer.
fn decode_states(states: &HashMap<String, LoroValue>) -> Vec<(String, PresenceEntry)> {
    states
        .iter()
        .filter_map(|(k, v)| PresenceEntry::decode(v).map(|e| (k.clone(), e)))
        .collect()
}

/// Humans before agents, then case-insensitive by name.
fn sort_entries(entries: &mut [(String, PresenceEntry)]) {
    entries.sort_by(|(_, a), (_, b)| {
        (a.kind != PresenceKind::Human, a.name.to_lowercase())
            .cmp(&(b.kind != PresenceKind::Human, b.name.to_lowercase()))
    });
}

fn display_name(raw: &str) -> String {
    let trimmed = raw.trim();
    if trimmed.is_empty() {
        DEFAULT_NAME.to_string()
    } else {
        trimmed.to_string()
    }
}

fn now_ms() -> i64 {
    chrono::Utc::now().timestamp_millis()
}

fn truncate(s: &str, max: usize) -> String {
    if s.chars().count() <= max {
        s.to_string()
    } else {
        let cut: String = s.chars().take(max).collect();
        format!("{}…", cut.trim_end())
    }
}

// ── name persistence (localStorage on web) ──────────────────────────

#[cfg(target_arch = "wasm32")]
fn load_saved_name() -> String {
    let stored = web_sys::window()
        .and_then(|w| w.local_storage().ok().flatten())
        .and_then(|s| s.get_item(NAME_STORAGE_KEY).ok().flatten());
    match stored {
        Some(name) if !name.trim().is_empty() => name,
        _ => DEFAULT_NAME.to_string(),
    }
}

#[cfg(target_arch = "wasm32")]
fn save_name(name: &str) {
    if let Some(storage) = web_sys::window().and_then(|w| w.local_storage().ok().flatten()) {
        let _ = storage.set_item(NAME_STORAGE_KEY, name.trim());
    }
}

/// Per-tab stable presence key. sessionStorage is exactly the right
/// scope: survives refresh (same tab = same person, the new session
/// overwrites its old entry in place) but is distinct per tab, so two
/// tabs are two roster rows on purpose. A fresh tab mints + stores.
#[cfg(target_arch = "wasm32")]
fn stable_client_key() -> String {
    const KEY: &str = "task.presence.client";
    let storage = web_sys::window().and_then(|w| w.session_storage().ok().flatten());
    if let Some(s) = &storage {
        if let Ok(Some(existing)) = s.get_item(KEY) {
            if !existing.is_empty() {
                return existing;
            }
        }
    }
    let minted = Uuid::new_v4().to_string();
    if let Some(s) = &storage {
        let _ = s.set_item(KEY, &minted);
    }
    minted
}

#[cfg(not(target_arch = "wasm32"))]
fn stable_client_key() -> String {
    Uuid::new_v4().to_string()
}

#[cfg(not(target_arch = "wasm32"))]
fn load_saved_name() -> String {
    DEFAULT_NAME.to_string()
}

#[cfg(not(target_arch = "wasm32"))]
fn save_name(_name: &str) {
    // Native persistence rides the future settings store; presence
    // still works per-launch with the default name.
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn doc_id_spells_task_org_presenc() {
        assert_eq!(&PRESENCE_DOC_ID.as_bytes()[..], b"task-org-presenc");
    }

    #[test]
    fn entry_round_trips_through_loro_value() {
        let entry = PresenceEntry {
            name: "Cody".into(),
            kind: PresenceKind::Human,
            status: PresenceStatus::Dnd,
            activity: Some("viewing Projects".into()),
            project_id: Some(Uuid::new_v4()),
            task_id: Some(Uuid::new_v4()),
            since_ms: 1_765_000_000_123,
        };
        let decoded = PresenceEntry::decode(&entry.encode()).expect("decodes");
        assert_eq!(decoded, entry);
    }

    #[test]
    fn entry_round_trips_with_optional_fields_absent() {
        let entry = PresenceEntry {
            name: "Agent triage".into(),
            kind: PresenceKind::Agent,
            status: PresenceStatus::Active,
            activity: None,
            project_id: None,
            task_id: None,
            since_ms: 0,
        };
        let decoded = PresenceEntry::decode(&entry.encode()).expect("decodes");
        assert_eq!(decoded, entry);
    }

    #[test]
    fn decode_rejects_foreign_values() {
        assert_eq!(PresenceEntry::decode(&LoroValue::from("just a name")), None);
        assert_eq!(PresenceEntry::decode(&LoroValue::Null), None);
        // A map missing the required keys is also rejected.
        let map = LoroValue::Map(vec![("name".to_string(), LoroValue::from("x"))].into());
        assert_eq!(PresenceEntry::decode(&map), None);
    }

    #[test]
    fn decode_tolerates_malformed_optionals() {
        let map = LoroValue::Map(
            vec![
                ("name".to_string(), LoroValue::from("x")),
                ("kind".to_string(), LoroValue::from("human")),
                ("status".to_string(), LoroValue::from("active")),
                // Wrong types / unparseable — defaulted, not fatal.
                ("since_ms".to_string(), LoroValue::from("soon")),
                ("project_id".to_string(), LoroValue::from("not-a-uuid")),
            ]
            .into(),
        );
        let decoded = PresenceEntry::decode(&map).expect("decodes");
        assert_eq!(decoded.since_ms, 0);
        assert_eq!(decoded.project_id, None);
    }

    #[test]
    fn sort_puts_humans_first_then_alphabetical() {
        let mk = |name: &str, kind| PresenceEntry {
            name: name.into(),
            kind,
            status: PresenceStatus::Active,
            activity: None,
            project_id: None,
            task_id: None,
            since_ms: 0,
        };
        let mut entries = vec![
            mk("zeta-agent", PresenceKind::Agent),
            mk("Bob", PresenceKind::Human),
            mk("Agent alpha", PresenceKind::Agent),
            mk("alice", PresenceKind::Human),
        ];
        sort_entries(&mut entries);
        let names: Vec<&str> = entries.iter().map(|e| e.name.as_str()).collect();
        assert_eq!(names, vec!["alice", "Bob", "Agent alpha", "zeta-agent"]);
    }
}
