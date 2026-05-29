//! `/projects` — the workspace's project command center.
//!
//! Fetches `Vec<ProjectInfo>` from the active org's `/org/<slug>/vox`
//! endpoint via the architect-generated `ProjectServiceClient`, then
//! renders a live stats band over a grid of rich project cards.
//!
//! Each card carries a data-driven accent (the project's own `color`,
//! or a stable pick from the chart palette), a real progress bar, and
//! priority / lead / due metadata. Subprojects nest as a compact,
//! status-dotted list inside their parent card.
//!
//! Loading paints skeleton cards; the empty and error states are
//! first-class. Everything is theme-token only (no hex in styling) so
//! it tracks light/dark automatically — dark is the default.

use dioxus::prelude::*;
use fts_ui::lucide_dioxus::{CalendarDays, Flag, FolderKanban, Layers, User};
use fts_ui::prelude::*;
use project::ProjectInfo;

use crate::routes::Route;

/// Page-scoped keyframes for the staggered card entrance. Injected once
/// per render as a plain `<style>` (idempotent — identical content).
const PAGE_CSS: &str = "@keyframes ftsFadeUp{from{opacity:0;transform:translateY(14px)}to{opacity:1;transform:translateY(0)}}";

/// How the project list is laid out. Persisted for the page's lifetime.
#[derive(Clone, Copy, PartialEq, Eq, Default)]
enum ViewMode {
    #[default]
    Cards,
    List,
}

#[component]
pub fn ProjectsView() -> Element {
    let selection = use_context::<Signal<crate::orgs::OrgSelection>>();
    let org_list = use_context::<Signal<Vec<crate::orgs::OrgMeta>>>();
    let projects = use_resource(move || async move {
        let slugs = crate::orgs::selected_slugs(&selection.read(), &org_list.read());
        crate::feeds::fetch_projects(&slugs).await
    });
    let view_mode = use_signal(ViewMode::default);

    // While the org list is still being discovered the fetch resolves
    // to an empty set — show loading rather than an empty grid.
    let view = if org_list.read().is_empty() {
        render_loading()
    } else {
        match &*projects.read() {
            Some(Ok(rows)) => render_loaded(rows, view_mode),
            Some(Err(e)) => render_error(e),
            None => render_loading(),
        }
    };

    rsx! {
        style { dangerous_inner_html: PAGE_CSS }
        div { class: "relative isolate",
            // Atmospheric wash behind the header — token-only via color-mix.
            div {
                class: "pointer-events-none absolute inset-x-0 top-0 -z-10 h-72",
                style: "background: radial-gradient(60% 120% at 50% -10%, color-mix(in oklch, var(--primary) 10%, transparent), transparent 70%);",
            }
            div { class: "mx-auto w-full max-w-6xl flex flex-col gap-8 p-4 sm:p-6 lg:p-10",
                {view}
            }
        }
    }
}

// ── states ──────────────────────────────────────────────────────────

fn render_loading() -> Element {
    rsx! {
        header { class: "flex flex-col gap-3",
            div { class: "h-3 w-28 animate-pulse rounded-full bg-muted" }
            div { class: "h-9 w-56 animate-pulse rounded-lg bg-muted" }
        }
        div { class: "grid grid-cols-2 gap-px overflow-hidden rounded-2xl border border-border/70 bg-border/70 sm:grid-cols-4",
            for _ in 0..4 {
                div { class: "flex flex-col gap-2 bg-card p-5",
                    div { class: "h-8 w-14 animate-pulse rounded-md bg-muted" }
                    div { class: "h-3 w-20 animate-pulse rounded-full bg-muted" }
                }
            }
        }
        div { class: "grid grid-cols-1 gap-4 md:grid-cols-2 xl:grid-cols-3",
            for _ in 0..6 {
                div { class: "flex flex-col gap-4 rounded-xl border border-border/70 bg-card p-6",
                    div { class: "flex items-start justify-between gap-3",
                        div { class: "h-5 w-32 animate-pulse rounded-md bg-muted" }
                        div { class: "h-6 w-16 animate-pulse rounded-full bg-muted" }
                    }
                    div { class: "h-1.5 w-full animate-pulse rounded-full bg-muted" }
                    div { class: "flex gap-2",
                        div { class: "h-3 w-16 animate-pulse rounded-full bg-muted" }
                        div { class: "h-3 w-20 animate-pulse rounded-full bg-muted" }
                    }
                }
            }
        }
    }
}

fn render_error(err: &str) -> Element {
    rsx! {
        page_header { count_line: None }
        div { class: "flex items-start gap-3 rounded-xl border border-destructive/40 bg-destructive/10 px-4 py-3",
            div { class: "mt-0.5 text-destructive", Flag { size: 18 } }
            div { class: "flex flex-col gap-0.5",
                span { class: "text-sm font-medium text-foreground", "Couldn't reach the project service" }
                span { class: "text-xs text-muted-foreground", "{err}" }
            }
        }
    }
}

fn render_loaded(rows: &[ProjectInfo], view_mode: Signal<ViewMode>) -> Element {
    // Archived projects stay out of the main grid.
    let live: Vec<&ProjectInfo> = rows.iter().filter(|p| !p.archived).collect();

    if live.is_empty() {
        return rsx! {
            page_header { count_line: None, view_mode: None }
            div { class: "flex flex-col items-center gap-4 rounded-2xl border border-dashed border-border/70 bg-card/40 px-6 py-16 text-center",
                div { class: "flex size-14 items-center justify-center rounded-2xl bg-muted text-muted-foreground",
                    FolderKanban { size: 26 }
                }
                div { class: "flex flex-col gap-1",
                    Heading { level: HeadingLevel::H3, "No projects yet" }
                    Text {
                        variant: TextVariant::Muted,
                        "Seed a `type: project` page under `vault/Projects/` and it'll appear here."
                    }
                }
            }
        };
    }

    let mut top: Vec<&ProjectInfo> = live.iter().filter(|p| p.parent_id.is_none()).copied().collect();
    // Stable, useful order: active first, then by priority, then title.
    top.sort_by(|a, b| {
        status_rank(&a.status)
            .cmp(&status_rank(&b.status))
            .then_with(|| priority_rank(&a.priority).cmp(&priority_rank(&b.priority)))
            .then_with(|| a.title.to_lowercase().cmp(&b.title.to_lowercase()))
    });
    let total = live.len();
    let active = live.iter().filter(|p| matches_status(&p.status, Bucket::Active)).count();
    let on_hold = live.iter().filter(|p| matches_status(&p.status, Bucket::Hold)).count();
    let tracked: Vec<i16> = live.iter().filter(|p| p.progress_percent >= 0).map(|p| p.progress_percent).collect();
    let avg = if tracked.is_empty() {
        None
    } else {
        Some(tracked.iter().map(|v| i32::from(*v)).sum::<i32>() / tracked.len() as i32)
    };
    let count_line = format!("{} top-level · {total} total", top.len());

    // Materialize each top-level project with its subprojects once, so
    // both layouts share the same data shaping.
    let groups: Vec<(ProjectInfo, Vec<ProjectInfo>)> = top
        .iter()
        .map(|parent| {
            let kids: Vec<ProjectInfo> = rows
                .iter()
                .filter(|p| !p.archived && p.parent_id == Some(parent.id))
                .cloned()
                .collect();
            ((*parent).clone(), kids)
        })
        .collect();

    rsx! {
        page_header { count_line: Some(count_line), view_mode: Some(view_mode) }

        // ── live stats band — hairline-divided tiles ───────────────
        div { class: "grid grid-cols-2 gap-px overflow-hidden rounded-2xl border border-border/70 bg-border/70 sm:grid-cols-4",
            Stat { value: "{total}", label: "Projects".to_string(), hint: None }
            Stat { value: "{active}", label: "Active".to_string(), hint: None }
            Stat { value: "{on_hold}", label: "On hold".to_string(), hint: None }
            Stat {
                value: match avg { Some(a) => format!("{a}%"), None => "—".to_string() },
                label: "Avg progress".to_string(),
                hint: None,
            }
        }

        match view_mode() {
            ViewMode::Cards => rsx! {
                // Equal-height cards: the grid stretches each cell to the
                // row's tallest, and the card fills it (`h-full`).
                div { class: "grid grid-cols-1 gap-4 md:grid-cols-2 xl:grid-cols-3",
                    for (i, (parent, kids)) in groups.iter().enumerate() {
                        ProjectCardView {
                            key: "{parent.id}",
                            p: parent.clone(),
                            subprojects: kids.clone(),
                            index: i,
                        }
                    }
                }
            },
            ViewMode::List => rsx! {
                div { class: "overflow-hidden rounded-xl border border-border/70",
                    for (i, (parent, kids)) in groups.iter().enumerate() {
                        ProjectRow {
                            key: "{parent.id}",
                            p: parent.clone(),
                            sub_count: kids.len(),
                            first: i == 0,
                        }
                    }
                }
            },
        }
    }
}

// ── header ──────────────────────────────────────────────────────────

#[derive(Props, Clone, PartialEq)]
struct PageHeaderProps {
    count_line: Option<String>,
    view_mode: Option<Signal<ViewMode>>,
}

#[component]
fn page_header(props: PageHeaderProps) -> Element {
    rsx! {
        header { class: "flex flex-col gap-2",
            span { class: "text-[0.7rem] font-semibold uppercase tracking-[0.18em] text-muted-foreground",
                "Workspace"
            }
            div { class: "flex flex-wrap items-end justify-between gap-3",
                Heading { level: HeadingLevel::H1, class: "tracking-tight", "Projects" }
                div { class: "flex items-center gap-2",
                    if let Some(line) = &props.count_line {
                        span { class: "rounded-full border border-border/70 bg-card/60 px-3 py-1 text-xs font-medium text-muted-foreground tabular-nums",
                            "{line}"
                        }
                    }
                    if let Some(vm) = props.view_mode {
                        ViewToggle { view: vm }
                    }
                }
            }
            Text {
                variant: TextVariant::Muted,
                class: "max-w-prose",
                "Everything in flight across the org, live from the project service."
            }
        }
    }
}

#[derive(Props, Clone, PartialEq)]
struct ViewToggleProps {
    view: Signal<ViewMode>,
}

#[component]
fn ViewToggle(props: ViewToggleProps) -> Element {
    let mut view = props.view;
    rsx! {
        div { class: "inline-flex items-center gap-0.5 rounded-lg bg-muted/40 p-0.5 text-xs",
            for (mode, label) in [(ViewMode::Cards, "Cards"), (ViewMode::List, "List")] {
                {
                    let active = view() == mode;
                    let cls = if active {
                        "rounded-md bg-background px-3 py-1 font-medium text-foreground shadow-sm transition-colors"
                    } else {
                        "rounded-md px-3 py-1 text-muted-foreground transition-colors hover:text-foreground"
                    };
                    rsx! {
                        button {
                            key: "{label}",
                            r#type: "button",
                            class: "{cls}",
                            onclick: move |_| view.set(mode),
                            "{label}"
                        }
                    }
                }
            }
        }
    }
}

// ── stat tile ───────────────────────────────────────────────────────

#[derive(Props, Clone, PartialEq)]
struct StatProps {
    value: String,
    label: String,
    hint: Option<String>,
}

#[component]
fn Stat(props: StatProps) -> Element {
    rsx! {
        div { class: "flex flex-col gap-1 bg-card p-5",
            div { class: "flex items-baseline gap-1.5",
                span { class: "text-3xl font-semibold tracking-tight tabular-nums text-foreground",
                    "{props.value}"
                }
                if let Some(h) = &props.hint {
                    span { class: "text-xs text-muted-foreground", "{h}" }
                }
            }
            span { class: "text-[0.7rem] font-medium uppercase tracking-[0.12em] text-muted-foreground",
                "{props.label}"
            }
        }
    }
}

// ── project card ────────────────────────────────────────────────────

#[derive(Props, Clone, PartialEq)]
struct ProjectCardProps {
    p: ProjectInfo,
    subprojects: Vec<ProjectInfo>,
    index: usize,
}

#[component]
fn ProjectCardView(props: ProjectCardProps) -> Element {
    let p = &props.p;
    let kids = props.subprojects.clone();
    let accent = accent(p);
    let pid = p.id.to_string();
    let progress = (p.progress_percent >= 0).then(|| p.progress_percent.clamp(0, 100));
    let summary = first_line(&p.details);
    let lead = clean_lead(&p.lead);
    let pri_label = priority_label(&p.priority);
    let pri_class = priority_class(&p.priority);
    let due = p.target_date.map(|d| d.format("%b %-d, %Y").to_string());
    let tags: Vec<String> = p.tags.0.clone();
    let shown_tags: Vec<String> = tags.iter().take(4).cloned().collect();
    let extra_tags = tags.len().saturating_sub(shown_tags.len());
    let sub_count = kids.len();
    let has_image = !p.image.trim().is_empty();
    // Staggered entrance — capped so a long list doesn't drag.
    let delay = (props.index.min(11) * 45) as u32;

    rsx! {
        div {
            class: "group relative h-full",
            style: "animation: ftsFadeUp 0.55s cubic-bezier(0.16,1,0.3,1) both; animation-delay: {delay}ms;",
            Card { class: "relative flex h-full flex-col overflow-hidden border-border/70 bg-card/70 backdrop-blur-sm transition-all duration-300 ease-out group-hover:-translate-y-1 group-hover:border-border group-hover:shadow-xl group-hover:shadow-foreground/5",
                // 16:9 cover — the project's image if set, else an
                // accent-gradient placeholder so the slot reads as
                // "ready for an image".
                div { class: "relative aspect-video w-full overflow-hidden bg-muted",
                    if has_image {
                        img {
                            src: "{p.image}",
                            alt: "{p.title}",
                            loading: "lazy",
                            class: "h-full w-full object-cover transition-transform duration-500 group-hover:scale-105",
                        }
                    } else {
                        div {
                            class: "h-full w-full",
                            style: "background: linear-gradient(135deg, {accent} 0%, color-mix(in oklch, {accent} 35%, var(--card)) 100%);",
                        }
                        // a faint icon watermark so the placeholder isn't flat
                        div { class: "absolute inset-0 flex items-center justify-center text-foreground/15",
                            FolderKanban { size: 40 }
                        }
                    }
                }
                CardHeader {
                    div { class: "flex items-start justify-between gap-3",
                        div { class: "flex min-w-0 flex-col gap-1",
                            Link {
                                to: Route::ProjectDetailRoute { id: pid.clone() },
                                class: "min-w-0",
                                CardTitle { class: "truncate transition-colors group-hover:text-foreground",
                                    "{p.title}"
                                }
                            }
                            if let Some(s) = summary {
                                CardDescription { class: "line-clamp-2", "{s}" }
                            }
                        }
                        StatusBadge { variant: status_variant(&p.status), label: p.status.clone() }
                    }
                }
                CardContent { class: "flex flex-1 flex-col",
                    div { class: "flex h-full flex-col gap-4",
                        // progress
                        if let Some(pct) = progress {
                            div { class: "flex flex-col gap-1.5",
                                div { class: "flex items-center justify-between text-xs",
                                    span { class: "text-muted-foreground", "Progress" }
                                    span { class: "font-medium tabular-nums text-foreground", "{pct}%" }
                                }
                                div { class: "h-1.5 w-full overflow-hidden rounded-full bg-muted",
                                    div {
                                        class: "h-full rounded-full transition-all duration-500",
                                        style: "width: {pct}%; background: {accent};",
                                    }
                                }
                            }
                        }
                        // metadata row
                        div { class: "flex flex-wrap items-center gap-x-4 gap-y-1.5 text-xs",
                            div { class: "flex items-center gap-1.5 {pri_class}",
                                Flag { size: 13 }
                                span { "{pri_label}" }
                            }
                            if !lead.is_empty() {
                                div { class: "flex items-center gap-1.5 text-muted-foreground",
                                    User { size: 13 }
                                    span { class: "max-w-[10rem] truncate", "{lead}" }
                                }
                            }
                            if let Some(due) = &due {
                                div { class: "flex items-center gap-1.5 text-muted-foreground",
                                    CalendarDays { size: 13 }
                                    span { "{due}" }
                                }
                            }
                            // Subproject count — a compact chip instead of the
                            // full list, so cards stay a uniform height.
                            if sub_count > 0 {
                                div { class: "flex items-center gap-1.5 text-muted-foreground",
                                    Layers { size: 13 }
                                    span {
                                        if sub_count == 1 { "1 subproject" } else { "{sub_count} subprojects" }
                                    }
                                }
                            }
                        }
                        // tags — pinned to the bottom so equal-height rows
                        // line their footers up.
                        if !shown_tags.is_empty() {
                            div { class: "mt-auto flex flex-wrap items-center gap-1.5 pt-1",
                                for tag in shown_tags.iter() {
                                    Badge { variant: BadgeVariant::Secondary, "{tag}" }
                                }
                                if extra_tags > 0 {
                                    span { class: "text-xs text-muted-foreground", "+{extra_tags}" }
                                }
                            }
                        }
                    }
                }
            }
        }
    }
}

// ── project row (list view) ─────────────────────────────────────────

#[derive(Props, Clone, PartialEq)]
struct ProjectRowProps {
    p: ProjectInfo,
    sub_count: usize,
    first: bool,
}

#[component]
fn ProjectRow(props: ProjectRowProps) -> Element {
    let p = &props.p;
    let accent = accent(p);
    let pid = p.id.to_string();
    let progress = (p.progress_percent >= 0).then(|| p.progress_percent.clamp(0, 100));
    let pri_label = priority_label(&p.priority);
    let pri_class = priority_class(&p.priority);
    let shown_tags: Vec<String> = p.tags.0.iter().take(3).cloned().collect();
    let row_cls = if props.first {
        "group flex items-center gap-3 bg-card/40 px-4 py-3 transition-colors hover:bg-accent/30"
    } else {
        "group flex items-center gap-3 border-t border-border/60 bg-card/40 px-4 py-3 transition-colors hover:bg-accent/30"
    };

    rsx! {
        Link { to: Route::ProjectDetailRoute { id: pid }, class: "{row_cls}",
            // accent rail
            span { class: "h-9 w-1 shrink-0 rounded-full", style: "background: {accent};" }
            // title + inline meta
            div { class: "flex min-w-0 flex-1 flex-col gap-0.5",
                span { class: "truncate text-sm font-medium text-foreground transition-colors group-hover:text-foreground",
                    "{p.title}"
                }
                div { class: "flex flex-wrap items-center gap-x-2.5 gap-y-0.5 text-xs text-muted-foreground",
                    span { class: "{pri_class}", "{pri_label}" }
                    if props.sub_count > 0 {
                        span { "· {props.sub_count} sub" }
                    }
                    for tag in shown_tags.iter() {
                        span { key: "{tag}", class: "hidden sm:inline", "· {tag}" }
                    }
                }
            }
            // progress (desktop)
            if let Some(pct) = progress {
                div { class: "hidden w-32 shrink-0 items-center gap-2 md:flex",
                    div { class: "h-1.5 flex-1 overflow-hidden rounded-full bg-muted",
                        div {
                            class: "h-full rounded-full",
                            style: "width: {pct}%; background: {accent};",
                        }
                    }
                    span { class: "w-8 shrink-0 text-right text-xs tabular-nums text-muted-foreground",
                        "{pct}%"
                    }
                }
            }
            StatusBadge { variant: status_variant(&p.status), label: p.status.clone() }
        }
    }
}

// ── helpers ─────────────────────────────────────────────────────────

/// Sort key: active projects first, paused next, in-progress/other,
/// then done, then cancelled.
fn status_rank(status: &str) -> u8 {
    match status {
        "active" | "open" | "in_progress" => 0,
        "on_hold" | "on-hold" | "paused" | "waiting" => 1,
        "done" | "completed" | "shipped" => 3,
        "cancelled" | "canceled" | "abandoned" => 4,
        _ => 2,
    }
}

/// Sort key: urgent → high → normal → low → lowest/unknown.
fn priority_rank(pr: &str) -> u8 {
    match pr {
        "p0" | "urgent" => 0,
        "p1" | "high" => 1,
        "" | "p2" | "normal" => 2,
        "p3" | "low" => 3,
        _ => 4,
    }
}

/// Accent for a project: its own stored `color` if set, else a stable
/// pick from the chart palette by title hash. Always a CSS value safe
/// for an inline `style` (a `var(--chart-N)` token, or user-set data).
fn accent(p: &ProjectInfo) -> String {
    if !p.color.trim().is_empty() {
        p.color.clone()
    } else {
        let n = 1 + (fnv1a(&p.title) % 5);
        format!("var(--chart-{n})")
    }
}

/// Tiny deterministic hash so a project's accent is stable across loads.
fn fnv1a(s: &str) -> u64 {
    let mut h: u64 = 0xcbf2_9ce4_8422_2325;
    for b in s.bytes() {
        h ^= u64::from(b);
        h = h.wrapping_mul(0x0000_0100_0000_01b3);
    }
    h
}

enum Bucket {
    Active,
    Hold,
}

fn matches_status(status: &str, bucket: Bucket) -> bool {
    match bucket {
        Bucket::Active => matches!(status, "active" | "open" | "in_progress"),
        Bucket::Hold => matches!(status, "on_hold" | "on-hold" | "paused" | "waiting"),
    }
}

fn status_variant(status: &str) -> StatusBadgeVariant {
    match status {
        "active" | "open" | "in_progress" => StatusBadgeVariant::Success,
        "on_hold" | "on-hold" | "paused" | "waiting" => StatusBadgeVariant::Warning,
        "cancelled" | "canceled" | "abandoned" | "blocked" => StatusBadgeVariant::Danger,
        _ => StatusBadgeVariant::Neutral,
    }
}


fn priority_label(pr: &str) -> String {
    match pr {
        "p0" | "urgent" => "Urgent".into(),
        "p1" | "high" => "High".into(),
        "" | "p2" | "normal" => "Normal".into(),
        "p3" | "low" => "Low".into(),
        "p4" | "lowest" => "Lowest".into(),
        other => {
            let mut c = other.chars();
            c.next()
                .map(|f| f.to_uppercase().collect::<String>() + c.as_str())
                .unwrap_or_default()
        }
    }
}

fn priority_class(pr: &str) -> &'static str {
    match pr {
        "p0" | "urgent" | "p1" | "high" => "text-destructive",
        "p3" | "low" | "p4" | "lowest" => "text-muted-foreground/60",
        _ => "text-muted-foreground",
    }
}

/// Strip a `[[wikilink]]` wrapper from a lead for display.
fn clean_lead(s: &str) -> String {
    s.trim()
        .trim_start_matches("[[")
        .trim_end_matches("]]")
        .trim()
        .to_string()
}

/// First meaningful line of the markdown body as a plain-text card
/// summary: skips front-matter rules and ATX headings, and strips
/// leading markup (`#`, `>`, list bullets) from the chosen line.
fn first_line(body: &str) -> Option<String> {
    let line = body
        .lines()
        .map(str::trim)
        .find(|l| !l.is_empty() && !l.starts_with("---") && !l.starts_with('#'))?;
    let cleaned = line.trim_start_matches(['#', '>', '-', '*', ' ']).trim();
    (!cleaned.is_empty()).then(|| cleaned.to_string())
}
