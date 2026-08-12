//! The Files explorer (issue #266) — one browser over the Files RPC
//! surface, mounted two ways:
//!
//! - **Standalone pane** ([`FilesPane`], the shell's `/files` route):
//!   the org's File Roots down the side, the selected root's live tree
//!   in the middle, and the **Drive** surface (loose files outside any
//!   root) as a sibling choice.
//! - **Note-embedded widget** ([`widgets`]): a `type: file-root` note
//!   *is* its explorer, and any other note embeds one with
//!   `experience: files` — narrowed to a **root slice** when the note
//!   names one.
//!
//! Both mount the same [`Explorer`]; the pane adds the root picker.
//!
//! **Badges come from live RPC data, never from local guesswork**
//! (glossary vocabulary throughout):
//!
//! - *Stub* — [`files_proto::BrowseEntry::stub`]: the version store
//!   tracks the path but the live tree doesn't hold it. Browsing a
//!   240 GB project never means downloading it.
//! - *Divergent* — [`files_proto::BrowseEntry::divergent`]: concurrent
//!   saves survive side by side; the badge rides the entry until
//!   someone resolves it (issue #267).
//! - *Project Version* — [`files_proto::FileRootInfo::project_version`]:
//!   the root's current lineage (its highest-numbered `ProjectVersion`
//!   Vault entity, issue #261), so "Project NEW final2" folders stay
//!   dead.
//! - *Versions* — a file's [`files_proto::ChainEntry`] count, fetched
//!   when a row is opened (the chain is derived per file, so it is a
//!   click, not a listing column).
//!
//! Live updates ride the service's `#[subscribe] fn events` stream: a
//! Session checkpoint taken anywhere — another device, the CLI, the
//! cadence engine — re-reads the listing in place, with no refresh.

use dioxus::prelude::*;
use files_proto::{
    BrowseEntry, ChainEntry, FileRootInfo, FilesEvent, FilesServiceClient,
    FilesServiceStreamClient, ProjectVersion,
};
use fts_ui::prelude::*;
use task_ui_core::orgs::{OrgMeta, OrgSelection};
use task_widgets::{WidgetCtx, WidgetMatch, WidgetSpec, WidgetTarget};
use uuid::Uuid;

/// The note frontmatter `type:` that makes a note a File Root page.
const NOTE_TYPE: &str = "file-root";

// ── RPC ───────────────────────────────────────────────────────────

async fn client(org: &str) -> Result<FilesServiceClient, String> {
    task_ui_core::vox_clients::establish_for::<FilesServiceClient>(org).await
}

async fn fetch_roots(org: &str) -> Result<Vec<FileRootInfo>, String> {
    client(org)
        .await?
        .list_roots()
        .await
        .map_err(|e| e.to_string())
}

async fn fetch_entries(org: &str, scope: &Location) -> Result<Vec<BrowseEntry>, String> {
    let c = client(org).await?;
    match scope {
        Location::Root { id, subpath } => c.browse(*id, subpath.clone()).await,
        Location::Drive { path } => c.drive_browse(path.clone()).await,
    }
    .map_err(|e| e.to_string())
}

async fn fetch_chain(org: &str, root: Uuid, path: String) -> Result<Vec<ChainEntry>, String> {
    client(org)
        .await?
        .chain(root, path)
        .await
        .map_err(|e| e.to_string())
}

// ── Scope ─────────────────────────────────────────────────────────

/// Where the explorer is currently looking. Root browsing and Drive
/// browsing are distinct surfaces (glossary), not two modes of one
/// call — they answer different RPCs and only root browsing carries
/// version-store badges.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Location {
    /// Inside a File Root's live tree, at a root-relative subpath
    /// (empty = the root itself).
    Root { id: Uuid, subpath: String },
    /// Loose files outside any root, at an absolute path.
    Drive { path: String },
}

impl Location {
    fn root_id(&self) -> Option<Uuid> {
        match self {
            Self::Root { id, .. } => Some(*id),
            Self::Drive { .. } => None,
        }
    }

    /// The path as displayed / navigated: root-relative inside a root,
    /// absolute on Drive.
    fn path(&self) -> &str {
        match self {
            Self::Root { subpath, .. } => subpath,
            Self::Drive { path } => path,
        }
    }

    fn with_path(&self, path: String) -> Self {
        match self {
            Self::Root { id, .. } => Self::Root {
                id: *id,
                subpath: path,
            },
            Self::Drive { .. } => Self::Drive { path },
        }
    }

    /// Descend into `name`.
    fn child(&self, name: &str) -> Self {
        let joined = match self {
            Self::Root { subpath, .. } if subpath.is_empty() => name.to_owned(),
            Self::Root { subpath, .. } => format!("{subpath}/{name}"),
            Self::Drive { path } => format!("{}/{name}", path.trim_end_matches('/')),
        };
        self.with_path(joined)
    }
}

/// Path segments of `location` below `floor` (the slice the explorer
/// is pinned to), as (label, path) pairs — the breadcrumb's data.
fn crumbs(location: &Location, floor: &str) -> Vec<(String, String)> {
    let path = location.path();
    let rest = path.strip_prefix(floor).unwrap_or(path);
    let rest = rest.trim_matches('/');
    if rest.is_empty() {
        return Vec::new();
    }
    let mut out = Vec::new();
    let mut acc = floor.trim_end_matches('/').to_owned();
    for part in rest.split('/') {
        if acc.is_empty() {
            acc = part.to_owned();
        } else {
            acc = format!("{acc}/{part}");
        }
        out.push((part.to_owned(), acc.clone()));
    }
    out
}

/// A byte count for a listing row. Stubs have no live-tree size — that
/// absence is information (nothing to read locally), so it renders as a
/// dash rather than `0 B`.
fn human_size(entry: &BrowseEntry) -> String {
    let Some(bytes) = entry.size else {
        return "—".to_owned();
    };
    const UNITS: [&str; 5] = ["B", "KB", "MB", "GB", "TB"];
    #[allow(clippy::cast_precision_loss)]
    let mut value = bytes as f64;
    let mut unit = 0;
    while value >= 1024.0 && unit + 1 < UNITS.len() {
        value /= 1024.0;
        unit += 1;
    }
    if unit == 0 {
        format!("{bytes} B")
    } else {
        format!("{value:.1} {}", UNITS[unit])
    }
}

/// The root's Project Version badge label ("Project Version 2 · client
/// cut") — the projection of its current [`ProjectVersion`] entity
/// (issue #261).
fn project_version_label(version: &ProjectVersion) -> String {
    match &version.label {
        Some(label) if !label.is_empty() => {
            format!("Project Version {} · {label}", version.number)
        }
        _ => format!("Project Version {}", version.number),
    }
}

// ── The explorer ──────────────────────────────────────────────────

/// Props for [`Explorer`].
#[derive(Props, Clone, PartialEq)]
pub struct ExplorerProps {
    /// Org slug whose Files service to talk to.
    pub org: String,
    /// Where to start.
    pub start: Location,
    /// A slice the explorer is pinned to: navigation never rises above
    /// this path. Empty = the whole root (or the Drive path given).
    #[props(default)]
    pub floor: String,
    /// Root header (name + badges) to render above the listing.
    #[props(default)]
    pub root: Option<FileRootInfo>,
    /// Compact chrome for the note-embedded mount.
    #[props(default)]
    pub embedded: bool,
}

/// The listing itself: breadcrumb, rows, badges, and the per-file
/// version chain. Mounted by both the pane and the note widget.
#[component]
pub fn Explorer(props: ExplorerProps) -> Element {
    let org = props.org.clone();
    let floor = props.floor.clone();
    let mut location = use_signal(|| props.start.clone());
    // Follow prop changes (the pane swaps roots underneath us).
    let start = props.start.clone();
    use_effect(use_reactive!(|start| location.set(start)));

    let mut entries = {
        let org = org.clone();
        use_resource(move || {
            let org = org.clone();
            let here = location.read().clone();
            async move { fetch_entries(&org, &here).await }
        })
    };

    // Live: a checkpoint (or a new root) anywhere re-reads this
    // listing, so a save on another device lands here without a
    // refresh.
    {
        let org = org.clone();
        architect::use_stream(
            move |tx| {
                let org = org.clone();
                async move {
                    let Ok(stream) =
                        task_ui_core::vox_clients::establish_for::<FilesServiceStreamClient>(&org)
                            .await
                    else {
                        return false;
                    };
                    stream.events(tx).await.is_ok()
                }
            },
            move |event: FilesEvent| {
                let mut entries = entries;
                let touched = match &event {
                    FilesEvent::Checkpointed(info) => Some(info.root_id),
                    FilesEvent::RootCreated(root) => Some(root.id),
                    // Curation (issue #261) doesn't move a live tree,
                    // but it does move the badges this pane renders
                    // (a chain's names, a root's lineage), so it is a
                    // reason to re-read too.
                    FilesEvent::VersionNamed(v) | FilesEvent::VersionUnnamed(v) => Some(v.root_id),
                    FilesEvent::ProjectVersionStarted(pv) => Some(pv.root_id),
                    // Snapshots are ephemeral captures, never listing
                    // changes — but the snapshots panel re-reads (#260).
                    FilesEvent::Snapshotted(snap) => Some(snap.root_id),
                    // A file flipping between resident and stub is a
                    // listing change in exactly one root (issue #263).
                    FilesEvent::HydrationChanged(change) => Some(change.root_id),
                };
                // Drive listings have no root id, so any Files event is
                // a reason to re-read (loose files move under roots).
                match location.peek().root_id() {
                    Some(id) if touched != Some(id) => {}
                    _ => entries.restart(),
                }
            },
        );
    }

    // The opened file's version chain — fetched on demand, because a
    // chain is derived per file (ADR 0001) and a listing must not pay
    // for it.
    let mut opened = use_signal(|| Option::<String>::None);
    let chain = {
        let org = org.clone();
        use_resource(move || {
            let org = org.clone();
            let here = location.read().clone();
            let opened = opened.read().clone();
            async move {
                match (here.root_id(), opened) {
                    (Some(root), Some(name)) => {
                        let path = match here.path() {
                            "" => name,
                            base => format!("{base}/{name}"),
                        };
                        Some(fetch_chain(&org, root, path).await)
                    }
                    _ => None,
                }
            }
        })
    };

    let listing = entries.read_unchecked().clone();
    // Hoisted out of the row loop: reading a signal per row would hold
    // a `Ref` across the comparison (and re-read N times).
    let opened_name: Option<String> = opened.read().clone();
    let opened_chain: Option<Result<Vec<ChainEntry>, String>> =
        chain.read_unchecked().clone().flatten();
    let crumb_items = crumbs(&location.read().clone(), &floor);
    let at_floor = crumb_items.is_empty();
    let padding = if props.embedded { "p-3" } else { "p-4" };

    rsx! {
        div { class: "flex flex-col gap-3 rounded-lg border border-border/40 bg-card/40 {padding}",
            if let Some(root) = &props.root {
                RootHeader { root: root.clone(), slice: floor.clone() }
            }
            // Breadcrumb — never above the pinned slice.
            div { class: "flex items-center gap-1 text-xs text-muted-foreground flex-wrap",
                button {
                    class: "rounded px-1.5 py-0.5 hover:bg-muted/50 disabled:opacity-60",
                    disabled: at_floor,
                    onclick: {
                        let floor = floor.clone();
                        move |_| {
                            let next = location.peek().with_path(floor.clone());
                            location.set(next);
                            opened.set(None);
                        }
                    },
                    if floor.is_empty() { "Root" } else { "{floor}" }
                }
                for (label , path) in crumb_items {
                    span { "/" }
                    button {
                        class: "rounded px-1.5 py-0.5 hover:bg-muted/50",
                        onclick: move |_| {
                            let next = location.peek().with_path(path.clone());
                            location.set(next);
                            opened.set(None);
                        },
                        "{label}"
                    }
                }
            }
            {match listing {
                None => rsx! {
                    task_ui_core::states::LoadingState { rows: 3 }
                },
                Some(Err(e)) => rsx! {
                    task_ui_core::states::ErrorState {
                        message: e,
                        on_retry: move |()| entries.restart(),
                    }
                },
                Some(Ok(rows)) if rows.is_empty() => rsx! {
                    task_ui_core::states::EmptyState {
                        title: "Nothing here yet",
                        hint: "Files saved into this folder show up on the next Session checkpoint.",
                    }
                },
                Some(Ok(rows)) => rsx! {
                    div { class: "flex flex-col divide-y divide-border/30",
                        for entry in rows {
                            EntryRow {
                                key: "{entry.name}",
                                entry: entry.clone(),
                                open: opened_name.as_deref() == Some(entry.name.as_str()),
                                onopen: {
                                    let entry = entry.clone();
                                    move |()| {
                                        if entry.is_dir {
                                            let next = location.peek().child(&entry.name);
                                            location.set(next);
                                            opened.set(None);
                                        } else if opened.peek().as_deref() == Some(entry.name.as_str()) {
                                            opened.set(None);
                                        } else {
                                            opened.set(Some(entry.name.clone()));
                                        }
                                    }
                                },
                                chain: if opened_name.as_deref() == Some(entry.name.as_str()) {
                                    opened_chain.clone()
                                } else {
                                    None
                                },
                            }
                        }
                    }
                },
            }}
        }
    }
}

/// The root's name, flavor, and Project Version badge, plus the slice
/// the explorer is pinned to (when it is).
#[component]
fn RootHeader(root: FileRootInfo, slice: String) -> Element {
    rsx! {
        div { class: "flex items-baseline gap-2 flex-wrap",
            span { class: "text-sm font-medium", "{root.name}" }
            if let Some(badge) = &root.project_version {
                Badge { variant: BadgeVariant::Secondary, "{project_version_label(badge)}" }
            }
            if !slice.is_empty() {
                Badge { variant: BadgeVariant::Outline, "slice: {slice}" }
            }
            span { class: "text-xs text-muted-foreground truncate", "{root.path}" }
        }
    }
}

/// One listing row: name, badges, size, and — while open — the file's
/// version chain.
#[component]
fn EntryRow(
    entry: BrowseEntry,
    open: bool,
    onopen: EventHandler<()>,
    chain: Option<Result<Vec<ChainEntry>, String>>,
) -> Element {
    let icon = if entry.is_dir { "📁" } else { "📄" };
    rsx! {
        div { class: "flex flex-col gap-1 py-1.5",
            button {
                class: "flex items-center gap-2 text-left text-sm hover:text-foreground/90",
                onclick: move |_| onopen.call(()),
                span { class: "opacity-70", "{icon}" }
                span { class: if entry.stub { "truncate text-muted-foreground" } else { "truncate" },
                    "{entry.name}"
                }
                if entry.stub {
                    // Tracked by the root's version store, not resident
                    // here — hydration on demand is issue #263.
                    Badge { variant: BadgeVariant::Outline, "Stub" }
                }
                if entry.divergent {
                    // Concurrent saves survive side by side; resolution
                    // (pick A / pick B / keep both) is issue #267.
                    Badge { variant: BadgeVariant::Destructive, "Divergent" }
                }
                span { class: "ml-auto tabular-nums text-xs text-muted-foreground",
                    "{human_size(&entry)}"
                }
            }
            if open && !entry.is_dir {
                {match chain {
                    None => rsx! {
                        Text { variant: TextVariant::Muted, class: "pl-6 text-xs", "Reading version chain…" }
                    },
                    Some(Err(e)) => rsx! {
                        div { class: "pl-6 text-xs text-destructive", "No chain: {e}" }
                    },
                    Some(Ok(versions)) if versions.is_empty() => rsx! {
                        div { class: "pl-6 text-xs text-muted-foreground",
                            "No saved versions yet — this file has never been checkpointed."
                        }
                    },
                    Some(Ok(versions)) => rsx! {
                        div { class: "pl-6 flex flex-col gap-0.5",
                            div { class: "flex items-center gap-2",
                                Badge { variant: BadgeVariant::Secondary,
                                    "{versions.len()} version"
                                    if versions.len() != 1 { "s" }
                                }
                            }
                            for version in versions.iter().take(10) {
                                div { class: "flex items-center gap-2 text-xs text-muted-foreground tabular-nums",
                                    span { class: "font-mono", "{short_id(&version.commit_id)}" }
                                    span { class: "truncate", "{version.path}" }
                                    if let Some(from) = &version.renamed_from {
                                        Badge { variant: BadgeVariant::Outline, "renamed from {from}" }
                                    }
                                }
                            }
                        }
                    },
                }}
            }
        }
    }
}

/// First 12 hex chars of a commit id — enough to identify a saved state
/// on screen.
fn short_id(commit_id: &str) -> String {
    commit_id.chars().take(12).collect()
}

// ── The standalone pane ───────────────────────────────────────────

/// What the pane is showing. Three states, not `Option<Uuid>`: "nothing
/// chosen yet" and "the user chose Drive" are different answers, and
/// collapsing them makes every roots refresh look like a fresh mount.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum Selection {
    /// Before the roots list has landed — the only state the
    /// auto-select effect may act on.
    Unset,
    /// The Drive surface (loose files outside any root).
    Drive,
    Root(Uuid),
}

/// The shell's `/files` pane: the org's roots down the side, the
/// selected root (or Drive) in the middle.
#[component]
pub fn FilesPane() -> Element {
    let selection = use_context::<Signal<OrgSelection>>();
    let org_list = use_context::<Signal<Vec<OrgMeta>>>();
    let org =
        use_memo(move || task_ui_core::orgs::active_slug(&selection.read(), &org_list.read()));

    let mut roots = use_resource(move || async move {
        let slug = org();
        if slug.is_empty() {
            return Ok(Vec::new());
        }
        fetch_roots(&slug).await
    });

    // A new root anywhere joins the picker without a refresh.
    architect::use_stream(
        move |tx| async move {
            let slug = org();
            if slug.is_empty() {
                return false;
            }
            let Ok(stream) =
                task_ui_core::vox_clients::establish_for::<FilesServiceStreamClient>(&slug).await
            else {
                return false;
            };
            stream.events(tx).await.is_ok()
        },
        move |event: FilesEvent| {
            let mut roots = roots;
            if matches!(event, FilesEvent::RootCreated(_)) {
                roots.restart();
            }
        },
    );

    let mut selected = use_signal(|| Selection::Unset);
    let rows = roots.read_unchecked().clone();
    let known: Vec<FileRootInfo> = match &rows {
        Some(Ok(list)) => list.clone(),
        _ => Vec::new(),
    };
    // Land on the first root once the list arrives — ONLY from
    // `Unset`. The effect re-runs on every `roots` completion (a
    // `RootCreated` anywhere restarts it), so a two-state selection
    // that spelled Drive as "nothing selected" would yank a user off
    // Drive and drop the path they had typed (PR #288 review).
    use_effect(move || {
        if *selected.peek() != Selection::Unset {
            return;
        }
        if let Some(Ok(list)) = &*roots.read() {
            if let Some(first) = list.first() {
                selected.set(Selection::Root(first.id));
            }
        }
    });

    let selection = selected();
    let active = match selection {
        Selection::Root(id) => known.iter().find(|r| r.id == id).cloned(),
        Selection::Unset | Selection::Drive => None,
    };
    // A selected root that has vanished from the list (deleted
    // elsewhere) leaves the sidebar unhighlighted; say so rather than
    // silently showing Drive.
    let missing_root = matches!(selection, Selection::Root(_)) && active.is_none();

    rsx! {
        div { class: "mx-auto w-full max-w-6xl flex flex-col gap-6 p-4 sm:p-6 lg:p-10",
            div { class: "flex flex-col gap-1",
                Heading { level: HeadingLevel::H1, "Files" }
                Text {
                    variant: TextVariant::Muted,
                    "File Roots, their live trees, and the Drive surface for loose files."
                }
            }
            if let Some(Err(e)) = &rows {
                task_ui_core::states::ErrorState {
                    message: e.clone(),
                    title: "Couldn't list File Roots",
                    on_retry: move |()| roots.restart(),
                }
            }
            div { class: "grid gap-4 md:grid-cols-[16rem_1fr]",
                div { class: "flex flex-col gap-1",
                    for root in known.iter().cloned() {
                        button {
                            key: "{root.id}",
                            class: if selection == Selection::Root(root.id) {
                                "rounded-md border border-border/60 bg-muted/40 px-3 py-2 text-left text-sm"
                            } else {
                                "rounded-md border border-transparent px-3 py-2 text-left text-sm hover:bg-muted/20"
                            },
                            onclick: move |_| selected.set(Selection::Root(root.id)),
                            div { class: "flex items-center gap-2",
                                span { class: "truncate", "{root.name}" }
                                if let Some(badge) = &root.project_version {
                                    Badge { variant: BadgeVariant::Secondary, "v{badge.number}" }
                                }
                            }
                            div { class: "truncate text-xs text-muted-foreground", "{root.path}" }
                        }
                    }
                    button {
                        class: if selection == Selection::Drive {
                            "rounded-md border border-border/60 bg-muted/40 px-3 py-2 text-left text-sm"
                        } else {
                            "rounded-md border border-transparent px-3 py-2 text-left text-sm hover:bg-muted/20"
                        },
                        onclick: move |_| selected.set(Selection::Drive),
                        div { class: "flex items-center gap-2",
                            span { "Drive" }
                            Badge { variant: BadgeVariant::Outline, "loose files" }
                        }
                        div { class: "text-xs text-muted-foreground", "Outside any root" }
                    }
                }
                {match (rows.is_none(), active) {
                    (true, _) => rsx! {
                        task_ui_core::states::LoadingState { rows: 3 }
                    },
                    (false, Some(root)) => rsx! {
                        Explorer {
                            org: org(),
                            start: Location::Root { id: root.id, subpath: String::new() },
                            root: root.clone(),
                        }
                    },
                    (false, None) if missing_root => rsx! {
                        task_ui_core::states::EmptyState {
                            title: "That File Root is gone",
                            hint: "It is no longer in this org's roots — pick another, or browse Drive.",
                        }
                    },
                    (false, None) => rsx! {
                        DrivePane { org: org() }
                    },
                }}
            }
        }
    }
}

/// Drive browsing: loose files outside any root. The path is the user's
/// to type — the service confines it to the org's own files area, so an
/// out-of-bounds path comes back as an error rather than a listing.
#[component]
fn DrivePane(org: String) -> Element {
    let path = use_signal(String::new);
    let mut submitted = use_signal(String::new);
    rsx! {
        div { class: "flex flex-col gap-3",
            div { class: "flex items-center gap-2",
                Input { value: path, placeholder: "Path to browse" }
                Button {
                    on_click: move |_| submitted.set(path()),
                    "Browse"
                }
            }
            if submitted().is_empty() {
                Text {
                    variant: TextVariant::Muted,
                    "Give a path to browse loose files. Everything outside a File Root is unversioned — make it a root to start versioning it."
                }
            } else {
                Explorer {
                    org: org.clone(),
                    start: Location::Drive { path: submitted() },
                    floor: submitted(),
                }
            }
        }
    }
}

// ── The note-embedded widget ──────────────────────────────────────

/// The Files widget provider. Two ways a note embeds an explorer, both
/// scoped by the note's own `root:` / `slice:` frontmatter (glossary
/// "Root slice" — a reference to (root, subpath)):
///
/// - `type: file-root` — the note IS a File Root page.
/// - `experience: files` on any note — an explicit opt-in, so a project
///   note can carry the stems slice of its root inline.
///
/// The registry renders block widgets for note claims only
/// (`WidgetMatch::EmbedType` contributes decorations and href handling,
/// which are pure functions of the document and so can't carry live RPC
/// data) — hence the `experience:` opt-in rather than an
/// `![[Root note]]` embed claim.
///
/// Registered at the app root, like every other widget provider.
#[must_use]
pub fn widgets() -> Vec<WidgetSpec> {
    vec![
        WidgetSpec::new(
            "files.root",
            vec![
                // A `type: file-root` note IS its explorer…
                WidgetMatch::NoteType(NOTE_TYPE),
                // …and any other note opts in with `experience: files`
                // (a project note embedding the stems slice of its
                // root, say). Both read the same `root:` / `slice:`
                // frontmatter.
                WidgetMatch::NoteExperience("files"),
            ],
        )
        .render(|ctx| rsx! { FileRootNoteWidget { ctx } })
        .plugin("files"),
    ]
}

/// A File Root note's frontmatter scope: which root, and which slice of
/// it. `root:` accepts a root id or a root name; `slice:` is a
/// root-relative path (glossary "Slice" — a subtree of a root, the unit
/// selective sync and share links narrow to).
#[derive(Clone, Debug, PartialEq, Eq)]
struct NoteScope {
    root: String,
    slice: String,
}

fn scope_from_frontmatter(doc: &str) -> Option<NoteScope> {
    let scalar = |key: &str| {
        task_ui_core::frontmatter::frontmatter_value(doc, key)
            .map(|v| v.trim().trim_matches(['"', '\'']).trim().to_owned())
            .filter(|v| !v.is_empty())
    };
    Some(NoteScope {
        root: scalar("root")?,
        slice: scalar("slice")
            .unwrap_or_default()
            .trim_matches('/')
            .to_owned(),
    })
}

/// Resolve a note's `root:` — a root id, or a root name — against the
/// org's roots.
fn resolve_root(roots: &[FileRootInfo], reference: &str) -> Option<FileRootInfo> {
    if let Ok(id) = reference.parse::<Uuid>() {
        return roots.iter().find(|r| r.id == id).cloned();
    }
    roots
        .iter()
        .find(|r| r.name.eq_ignore_ascii_case(reference))
        .cloned()
}

/// The note-mounted explorer. The scope comes from the note's own
/// frontmatter (`root:` + optional `slice:`).
#[component]
fn FileRootNoteWidget(ctx: WidgetCtx) -> Element {
    let org = ctx.org.clone();
    let source = match &ctx.target {
        WidgetTarget::Note { .. } => Some((ctx.doc)()),
        // Defensive: the registry only mounts block widgets for note
        // claims today, but an embed claim would carry its target's
        // content (fetched lazily — a miss renders as "loading").
        WidgetTarget::Embed { target, .. } => (ctx.resolve)(target).and_then(|t| t.content),
        WidgetTarget::Fence { .. } => None,
    };
    let scope = source.as_deref().and_then(scope_from_frontmatter);

    let roots = {
        let org = org.clone();
        use_resource(move || {
            let org = org.clone();
            async move { fetch_roots(&org).await }
        })
    };

    let Some(scope) = scope else {
        return rsx! {
            div { class: "rounded-lg border border-border/40 bg-card/40 p-3 text-sm text-muted-foreground",
                "This File Root note names no root yet — add `root: <name or id>` (and an optional `slice:`) to its frontmatter."
            }
        };
    };

    let resolved = match &*roots.read_unchecked() {
        Some(Ok(list)) => Some(resolve_root(list, &scope.root)),
        Some(Err(_)) => Some(None),
        None => None,
    };

    match resolved {
        None => rsx! {
            task_ui_core::states::LoadingState { rows: 1 }
        },
        Some(None) => rsx! {
            task_ui_core::states::EmptyState {
                title: format!("No File Root named “{}”", scope.root),
                hint: "Point this note's `root:` at a File Root in this org — by name or by id.",
            }
        },
        Some(Some(root)) => rsx! {
            Explorer {
                org: org.clone(),
                start: Location::Root { id: root.id, subpath: scope.slice.clone() },
                floor: scope.slice.clone(),
                root: root.clone(),
                embedded: true,
            }
        },
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn entry(name: &str, is_dir: bool, size: Option<u64>) -> BrowseEntry {
        BrowseEntry {
            name: name.to_owned(),
            is_dir,
            size,
            stub: false,
            divergent: false,
        }
    }

    #[test]
    fn descending_builds_root_relative_and_absolute_paths() {
        let id = Uuid::new_v4();
        let root = Location::Root {
            id,
            subpath: String::new(),
        };
        assert_eq!(root.child("stems").path(), "stems");
        assert_eq!(
            root.child("stems").child("kick.wav").path(),
            "stems/kick.wav"
        );
        let drive = Location::Drive {
            path: "/srv/files/".to_owned(),
        };
        assert_eq!(drive.child("loose.wav").path(), "/srv/files/loose.wav");
    }

    #[test]
    fn breadcrumbs_never_rise_above_the_pinned_slice() {
        let id = Uuid::new_v4();
        let here = Location::Root {
            id,
            subpath: "stems/gtr/takes".to_owned(),
        };
        let labels = crumbs(&here, "stems");
        assert_eq!(
            labels,
            vec![
                ("gtr".to_owned(), "stems/gtr".to_owned()),
                ("takes".to_owned(), "stems/gtr/takes".to_owned()),
            ],
            "a slice-scoped embed can't navigate out of its slice"
        );
        // At the slice itself there is nothing above to show.
        let at_floor = Location::Root {
            id,
            subpath: "stems".to_owned(),
        };
        assert!(crumbs(&at_floor, "stems").is_empty());
    }

    #[test]
    fn stub_rows_show_no_live_tree_size() {
        assert_eq!(human_size(&entry("mix.wav", false, Some(2048))), "2.0 KB");
        assert_eq!(human_size(&entry("notes.txt", false, Some(12))), "12 B");
        assert_eq!(human_size(&entry("stems", true, None)), "—");
        let mut stub = entry("cut.mov", false, None);
        stub.stub = true;
        assert_eq!(human_size(&stub), "—");
    }

    fn project_version(number: u32, label: Option<&str>) -> ProjectVersion {
        ProjectVersion {
            id: Uuid::new_v4(),
            path: "Files/Project Versions/v2.md".to_owned(),
            root_id: Uuid::new_v4(),
            number,
            label: label.map(str::to_owned),
            change_id: "abc".to_owned(),
            commit_id: "def".to_owned(),
            started_at: chrono::Utc::now(),
        }
    }

    #[test]
    fn project_version_badge_reads_as_lineage() {
        assert_eq!(
            project_version_label(&project_version(2, Some("client cut"))),
            "Project Version 2 · client cut"
        );
        assert_eq!(
            project_version_label(&project_version(3, None)),
            "Project Version 3"
        );
    }

    #[test]
    fn note_scope_comes_from_frontmatter() {
        let doc = "---\ntype: file-root\nroot: \"El Artisa\"\nslice: stems/gtr/\n---\n# Notes\n";
        let scope = scope_from_frontmatter(doc).expect("scope");
        assert_eq!(scope.root, "El Artisa");
        assert_eq!(scope.slice, "stems/gtr", "slice is a clean relative path");

        // No slice = the whole root.
        let whole =
            scope_from_frontmatter("---\ntype: file-root\nroot: Mix\n---\n").expect("scope");
        assert_eq!(whole.slice, "");

        // A note with no `root:` claims nothing.
        assert!(scope_from_frontmatter("---\ntype: file-root\n---\n").is_none());
    }

    #[test]
    fn root_reference_resolves_by_id_or_name() {
        let id = Uuid::new_v4();
        let roots = vec![FileRootInfo {
            id,
            name: "El Artisa".to_owned(),
            path: "/srv/files/el-artisa".to_owned(),
            flavor: files_proto::RootFlavor::Media,
            created_at: chrono::Utc::now(),
            project_version: None,
        }];
        assert_eq!(
            resolve_root(&roots, &id.to_string()).map(|r| r.id),
            Some(id)
        );
        assert_eq!(resolve_root(&roots, "el artisa").map(|r| r.id), Some(id));
        assert!(resolve_root(&roots, "Nope").is_none());
    }
}
