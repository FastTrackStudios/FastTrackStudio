//! `task plan …` + `task next` — the agent schedule-management
//! surface over the scheduling feature's `DayPlans` /
//! `DayTemplates` / `CalendarEvents` services (vault-backed at
//! `Records/dayplans/<YYYY-MM-DD>.md`).
//!
//! All verbs are read-modify-write over `upsert_day_plan`: load
//! the date's plan, mutate the `PlannedBlock` list locally
//! (overlap-checked), save it back. Block references accept a
//! 1-based index (as printed by `task plan show`) or a
//! case-insensitive label prefix.
//!
//! Lives in its own module so concurrent agents editing
//! `main.rs` only collide on the two-line dispatch arm.

use clap::{Args, Subcommand};
use scheduling_proto::day_plan::PlannedBlocks;
use scheduling_proto::{
    BlockAssignment, BlockCategory, CalEvent, DayPlan, PlannedBlock, TimeBlockId, TimeOfDay,
};

// ─────────────────────────────────────────────────────────────────────
// CLI surface
// ─────────────────────────────────────────────────────────────────────

#[derive(Subcommand)]
pub enum PlanCmd {
    /// Show a date's plan (blocks + assignments + calendar events).
    /// `week` reads today + the next 6 days.
    Show {
        /// `today` | `tomorrow` | `yesterday` | `YYYY-MM-DD` | `week`.
        #[arg(default_value = "today")]
        when: String,
        #[arg(long)]
        org: Option<String>,
        #[arg(long)]
        server: Option<String>,
        #[arg(long)]
        json: bool,
    },
    /// Block-level edits: add / move / resize / rm.
    #[command(subcommand)]
    Block(BlockCmd),
    /// Assign a task to a block (stores kind/title/ref_id).
    Assign {
        /// Block ref — 1-based index from `plan show`, or label prefix.
        block: String,
        /// Task id (uuid), id prefix, vault path, or title prefix.
        #[arg(long)]
        task: String,
        /// `today` | `tomorrow` | `yesterday` | `YYYY-MM-DD`.
        #[arg(long, default_value = "today")]
        date: String,
        #[arg(long)]
        org: Option<String>,
        #[arg(long)]
        server: Option<String>,
        #[arg(long)]
        json: bool,
    },
    /// Materialize a date's plan from a day template.
    FromTemplate {
        /// `today` | `tomorrow` | `YYYY-MM-DD`.
        date: String,
        /// Template name (case-insensitive prefix). Optional when
        /// exactly one template exists.
        #[arg(long)]
        template: Option<String>,
        /// Overwrite an existing saved plan for the date.
        #[arg(long)]
        force: bool,
        #[arg(long)]
        org: Option<String>,
        #[arg(long)]
        server: Option<String>,
        #[arg(long)]
        json: bool,
    },
    /// Plan vs actual — timer sessions overlaid on the blocks by
    /// time-overlap attribution.
    Diff {
        /// `today` | `yesterday` | `YYYY-MM-DD`.
        #[arg(default_value = "today")]
        date: String,
        #[arg(long)]
        org: Option<String>,
        #[arg(long)]
        server: Option<String>,
        #[arg(long)]
        json: bool,
    },
}

#[derive(Subcommand)]
pub enum BlockCmd {
    /// Add a block: `task plan block add "Deep work" 9:00-10:30`.
    Add {
        label: String,
        /// `<start>-<end>` — e.g. `9:00-10:30`, `9am-12pm`, `14:00-15:30`.
        range: String,
        /// reset | spiritual | meal | exercise | hygiene |
        /// allocatable | maintenance | winddown | sleep | other.
        #[arg(long)]
        category: Option<String>,
        /// `task:<id>` or `project:<id>` — validated against the
        /// service, stored as the block's assignment.
        #[arg(long)]
        assign: Option<String>,
        #[arg(long)]
        note: Option<String>,
        #[arg(long, default_value = "today")]
        date: String,
        /// Save even if the block overlaps an existing one.
        #[arg(long)]
        force: bool,
        #[arg(long)]
        org: Option<String>,
        #[arg(long)]
        server: Option<String>,
        #[arg(long)]
        json: bool,
    },
    /// Move a block (duration preserved).
    Move {
        /// Block ref — 1-based index or label prefix.
        block: String,
        /// New start time, e.g. `--to 14:00`.
        #[arg(long, conflicts_with = "by")]
        to: Option<String>,
        /// Relative shift, e.g. `--by +30m`, `--by -1h15m`.
        #[arg(long)]
        by: Option<String>,
        #[arg(long, default_value = "today")]
        date: String,
        #[arg(long)]
        force: bool,
        #[arg(long)]
        org: Option<String>,
        #[arg(long)]
        server: Option<String>,
        #[arg(long)]
        json: bool,
    },
    /// Change a block's end time.
    Resize {
        /// Block ref — 1-based index or label prefix.
        block: String,
        /// New end time, e.g. `--end 17:30`.
        #[arg(long)]
        end: String,
        #[arg(long, default_value = "today")]
        date: String,
        #[arg(long)]
        force: bool,
        #[arg(long)]
        org: Option<String>,
        #[arg(long)]
        server: Option<String>,
        #[arg(long)]
        json: bool,
    },
    /// Remove a block from the plan.
    Rm {
        /// Block ref — 1-based index or label prefix.
        block: String,
        #[arg(long, default_value = "today")]
        date: String,
        #[arg(long)]
        org: Option<String>,
        #[arg(long)]
        server: Option<String>,
        #[arg(long)]
        json: bool,
    },
}

/// `task next` — current block + remaining time + next block;
/// falls back to the next due task when no plan exists.
#[derive(Args)]
pub struct NextArgs {
    #[arg(long)]
    pub org: Option<String>,
    #[arg(long)]
    pub server: Option<String>,
    #[arg(long)]
    pub json: bool,
}

// ─────────────────────────────────────────────────────────────────────
// Pure helpers (unit-tested at the bottom)
// ─────────────────────────────────────────────────────────────────────

/// Parse a wall-clock time: `9:00`, `09:00`, `9am`, `9:30pm`,
/// `14:30`, bare `9`. `24:00` is allowed as an end bound.
pub(crate) fn parse_time_of_day(s: &str) -> Result<TimeOfDay, String> {
    let t = s.trim().to_ascii_lowercase();
    let (body, meridiem) = if let Some(b) = t.strip_suffix("am") {
        (b.trim(), Some(false))
    } else if let Some(b) = t.strip_suffix("pm") {
        (b.trim(), Some(true))
    } else {
        (t.as_str(), None)
    };
    let (h_str, m_str) = body.split_once(':').unwrap_or((body, "0"));
    let bad = || format!("can't parse time `{s}` — try forms like 9:00, 09:30, 9am, 14:30");
    let h: u16 = h_str.trim().parse().map_err(|_| bad())?;
    let m: u16 = m_str.trim().parse().map_err(|_| bad())?;
    if m > 59 {
        return Err(format!("`{s}`: minutes must be 00-59"));
    }
    let h24 = match meridiem {
        Some(pm) => {
            if h == 0 || h > 12 {
                return Err(format!("`{s}`: am/pm hours run 1-12"));
            }
            let base = if h == 12 { 0 } else { h };
            if pm { base + 12 } else { base }
        }
        None => h,
    };
    let minutes = h24 * 60 + m;
    if minutes > 24 * 60 {
        return Err(format!("`{s}` is past 24:00"));
    }
    Ok(TimeOfDay {
        minutes_since_midnight: minutes,
    })
}

/// Parse `<start>-<end>` (`9:00-10:30`, `9am-12pm`). Requires
/// `end > start`.
pub(crate) fn parse_time_range(s: &str) -> Result<(TimeOfDay, TimeOfDay), String> {
    let (a, b) = s
        .split_once('-')
        .ok_or_else(|| format!("expected `<start>-<end>` (e.g. 9:00-10:30), got `{s}`"))?;
    let start = parse_time_of_day(a)?;
    let end = parse_time_of_day(b)?;
    if end.minutes_since_midnight <= start.minutes_since_midnight {
        return Err(format!(
            "`{s}`: end ({}) must be after start ({})",
            fmt_tod(end),
            fmt_tod(start)
        ));
    }
    Ok((start, end))
}

/// Parse a signed duration for `block move --by`: `+30m`, `-1h15m`,
/// `45` (minutes), `2h`.
pub(crate) fn parse_delta_minutes(s: &str) -> Result<i32, String> {
    let t = s.trim();
    let (sign, body) = match t.strip_prefix('+') {
        Some(b) => (1i32, b),
        None => match t.strip_prefix('-') {
            Some(b) => (-1, b),
            None => (1, t),
        },
    };
    let bad = || format!("can't parse duration `{s}` — try +30m, -1h15m, 2h, 45");
    let mut mins: i32 = 0;
    if let Some((h, rest)) = body.split_once('h') {
        mins += h.trim().parse::<i32>().map_err(|_| bad())? * 60;
        let rest = rest.trim().trim_end_matches('m').trim();
        if !rest.is_empty() {
            mins += rest.parse::<i32>().map_err(|_| bad())?;
        }
    } else {
        let b = body.trim_end_matches('m').trim();
        mins += b.parse::<i32>().map_err(|_| bad())?;
    }
    if mins == 0 {
        return Err(format!("`{s}` is a zero-length shift"));
    }
    Ok(sign * mins)
}

/// First block in `blocks` that overlaps `[start, end)`, skipping
/// index `ignore` (the block being edited). Half-open semantics —
/// blocks that merely touch (10:30 end / 10:30 start) don't conflict.
pub(crate) fn find_overlap(
    blocks: &[PlannedBlock],
    start: TimeOfDay,
    end: TimeOfDay,
    ignore: Option<usize>,
) -> Option<usize> {
    blocks
        .iter()
        .enumerate()
        .position(|(i, b)| {
            Some(i) != ignore
                && start.minutes_since_midnight < b.end.minutes_since_midnight
                && b.start.minutes_since_midnight < end.minutes_since_midnight
        })
}

/// Resolve a block ref: a 1-based index as printed by `plan show`,
/// or a case-insensitive label prefix. Ambiguous prefixes list the
/// candidates.
pub(crate) fn resolve_block_ref(blocks: &[PlannedBlock], r: &str) -> Result<usize, String> {
    if let Ok(n) = r.parse::<usize>() {
        if (1..=blocks.len()).contains(&n) {
            return Ok(n - 1);
        }
        return Err(format!(
            "block index {n} is out of range — the plan has {} block(s); run `task plan show` to see them",
            blocks.len()
        ));
    }
    let needle = r.to_lowercase();
    let hits: Vec<usize> = blocks
        .iter()
        .enumerate()
        .filter(|(_, b)| b.label.to_lowercase().starts_with(&needle))
        .map(|(i, _)| i)
        .collect();
    match hits.as_slice() {
        [one] => Ok(*one),
        [] => Err(format!(
            "no block labelled `{r}…` — blocks: {}",
            blocks
                .iter()
                .enumerate()
                .map(|(i, b)| format!("{} \"{}\"", i + 1, b.label))
                .collect::<Vec<_>>()
                .join(", ")
        )),
        many => Err(format!(
            "`{r}` is ambiguous — matches: {}",
            many.iter()
                .map(|&i| format!("{} \"{}\" ({})", i + 1, blocks[i].label, fmt_tod(blocks[i].start)))
                .collect::<Vec<_>>()
                .join(", ")
        )),
    }
}

/// Attribute timer sessions (already projected to minutes-since-
/// local-midnight, clamped to the day) onto blocks by time overlap.
/// Returns per-block `(session index, overlap minutes)` lists plus
/// the indices of sessions that touched no block at all.
pub(crate) fn attribute_sessions(
    blocks: &[(u16, u16)],
    sessions: &[(u16, u16)],
) -> (Vec<Vec<(usize, u16)>>, Vec<usize>) {
    let mut per_block: Vec<Vec<(usize, u16)>> = vec![Vec::new(); blocks.len()];
    let mut unattributed = Vec::new();
    for (si, &(ss, se)) in sessions.iter().enumerate() {
        let mut hit = false;
        for (bi, &(bs, be)) in blocks.iter().enumerate() {
            let lo = ss.max(bs);
            let hi = se.min(be);
            if hi > lo {
                per_block[bi].push((si, hi - lo));
                hit = true;
            }
        }
        if !hit {
            unattributed.push(si);
        }
    }
    (per_block, unattributed)
}

pub(crate) fn fmt_tod(t: TimeOfDay) -> String {
    format!(
        "{:02}:{:02}",
        t.minutes_since_midnight / 60,
        t.minutes_since_midnight % 60
    )
}

pub(crate) fn fmt_min(mins: u16) -> String {
    let h = mins / 60;
    let m = mins % 60;
    if h > 0 && m > 0 {
        format!("{h}h{m:02}m")
    } else if h > 0 {
        format!("{h}h")
    } else {
        format!("{m}m")
    }
}

pub(crate) fn parse_category(s: &str) -> Result<BlockCategory, String> {
    Ok(match s.trim().to_lowercase().as_str() {
        "reset" => BlockCategory::Reset,
        "spiritual" => BlockCategory::Spiritual,
        "meal" => BlockCategory::Meal,
        "exercise" => BlockCategory::Exercise,
        "hygiene" => BlockCategory::Hygiene,
        "allocatable" | "work" => BlockCategory::Allocatable,
        "maintenance" => BlockCategory::Maintenance,
        "winddown" | "wind-down" => BlockCategory::WindDown,
        "sleep" => BlockCategory::Sleep,
        "other" => BlockCategory::Other,
        other => {
            return Err(format!(
                "unknown category `{other}` — one of: reset, spiritual, meal, exercise, \
                 hygiene, allocatable, maintenance, winddown, sleep, other"
            ));
        }
    })
}

pub(crate) fn category_name(c: BlockCategory) -> &'static str {
    match c {
        BlockCategory::Reset => "reset",
        BlockCategory::Spiritual => "spiritual",
        BlockCategory::Meal => "meal",
        BlockCategory::Exercise => "exercise",
        BlockCategory::Hygiene => "hygiene",
        BlockCategory::Allocatable => "allocatable",
        BlockCategory::Maintenance => "maintenance",
        BlockCategory::WindDown => "winddown",
        BlockCategory::Sleep => "sleep",
        BlockCategory::Other => "other",
    }
}

/// `today` / `tomorrow` / `yesterday` / `YYYY-MM-DD` → ISO date string.
pub(crate) fn parse_date_arg(s: &str) -> eyre::Result<String> {
    let today = chrono::Local::now().date_naive();
    let d = match s.trim().to_lowercase().as_str() {
        "today" => today,
        "tomorrow" => today.succ_opt().ok_or_else(|| eyre::eyre!("date overflow"))?,
        "yesterday" => today.pred_opt().ok_or_else(|| eyre::eyre!("date underflow"))?,
        other => chrono::NaiveDate::parse_from_str(other, "%Y-%m-%d").map_err(|_| {
            eyre::eyre!("can't parse date `{s}` — use today, tomorrow, yesterday, or YYYY-MM-DD")
        })?,
    };
    Ok(d.format("%Y-%m-%d").to_string())
}

fn sort_blocks(blocks: &mut [PlannedBlock]) {
    blocks.sort_by_key(|b| (b.start.minutes_since_midnight, b.end.minutes_since_midnight));
}

// ─────────────────────────────────────────────────────────────────────
// Service plumbing
// ─────────────────────────────────────────────────────────────────────

async fn day_plans_client(
    server: Option<String>,
    org: Option<String>,
) -> eyre::Result<(String, scheduling_proto::DayPlansClient)> {
    let slug = crate::resolve_active_org(org)?;
    let url = crate::resolve_org_vox_url(server, &slug);
    let client = crate::establish_for_url(&url).await?;
    Ok((slug, client))
}

async fn load_plan(
    client: &scheduling_proto::DayPlansClient,
    date: &str,
) -> eyre::Result<Option<DayPlan>> {
    client
        .get_day_plan(date.to_owned())
        .await
        .map_err(|e| eyre::eyre!("get_day_plan({date}): {e:?}"))
}

async fn save_plan(client: &scheduling_proto::DayPlansClient, plan: DayPlan) -> eyre::Result<()> {
    let date = plan.date.clone();
    client
        .upsert_day_plan(plan)
        .await
        .map_err(|e| eyre::eyre!("upsert_day_plan({date}): {e:?}"))
}

fn require_plan(plan: Option<DayPlan>, date: &str) -> eyre::Result<DayPlan> {
    plan.ok_or_else(|| {
        eyre::eyre!(
            "no saved plan for {date} — create one with `task plan from-template {date}` \
             or `task plan block add \"<label>\" <start>-<end> --date {date}`"
        )
    })
}

/// Resolve a task by uuid, id prefix, vault path, or title prefix.
/// Ambiguous refs list the candidates.
async fn resolve_task(
    client: &task::TaskServiceClient,
    target: &str,
) -> eyre::Result<task::TaskInfo> {
    if let Ok(id) = uuid::Uuid::parse_str(target) {
        return client
            .get(id)
            .await
            .map_err(|e| eyre::eyre!("get task {id}: {e:?}"));
    }
    let all = client
        .list()
        .await
        .map_err(|e| eyre::eyre!("list tasks: {e:?}"))?;
    let needle = target.to_lowercase();
    let hits: Vec<task::TaskInfo> = all
        .into_iter()
        .filter(|t| {
            t.id.to_string().starts_with(&needle)
                || t.path.to_lowercase() == needle
                || t.title.to_lowercase().starts_with(&needle)
        })
        .collect();
    match hits.len() {
        1 => Ok(hits.into_iter().next().expect("len 1")),
        0 => Err(eyre::eyre!(
            "no task matching `{target}` (by id prefix, path, or title prefix) — \
             run `task task list` to browse"
        )),
        _ => Err(eyre::eyre!(
            "`{target}` is ambiguous — matches:\n{}",
            hits.iter()
                .map(|t| format!("  {}  {}", t.id, t.title))
                .collect::<Vec<_>>()
                .join("\n")
        )),
    }
}

/// `--assign task:<id>|project:<id>` for `block add`. Validates the
/// referent exists and returns the stored assignment.
async fn resolve_assign_spec(
    spec: &str,
    server: Option<String>,
    slug: &str,
) -> eyre::Result<BlockAssignment> {
    let url = crate::resolve_org_vox_url(server, slug);
    if let Some(id) = spec.strip_prefix("task:") {
        let client: task::TaskServiceClient = crate::establish_for_url(&url).await?;
        let t = resolve_task(&client, id).await?;
        return Ok(BlockAssignment {
            kind: "task".into(),
            title: t.title,
            ref_id: Some(t.id.to_string()),
        });
    }
    if let Some(id) = spec.strip_prefix("project:") {
        let client: project::ProjectServiceClient = crate::establish_for_url(&url).await?;
        let p = if let Ok(pid) = uuid::Uuid::parse_str(id) {
            client
                .get(pid)
                .await
                .map_err(|e| eyre::eyre!("get project {pid}: {e:?}"))?
        } else {
            client
                .get_by_path(id.to_owned())
                .await
                .map_err(|e| eyre::eyre!("get project by path `{id}`: {e:?}"))?
        };
        return Ok(BlockAssignment {
            kind: "project".into(),
            title: p.title,
            ref_id: Some(p.id.to_string()),
        });
    }
    Err(eyre::eyre!(
        "can't parse --assign `{spec}` — use task:<id-or-prefix> or project:<id-or-path>"
    ))
}

// ─────────────────────────────────────────────────────────────────────
// Rendering
// ─────────────────────────────────────────────────────────────────────

pub(crate) fn render_block_line(idx: usize, b: &PlannedBlock) -> String {
    let mut line = format!(
        "{:>2}. {}–{}  {:<24} [{}]",
        idx + 1,
        fmt_tod(b.start),
        fmt_tod(b.end),
        b.label,
        category_name(b.category),
    );
    if let Some(a) = &b.assignment {
        line.push_str(&format!(
            "  → {}: {}{}",
            a.kind,
            a.title,
            a.ref_id
                .as_deref()
                .map(|r| format!(" ({})", &r[..r.len().min(8)]))
                .unwrap_or_default()
        ));
    }
    if let Some(n) = &b.note {
        line.push_str(&format!("  · {n}"));
    }
    line
}

pub(crate) fn render_plan(plan: &DayPlan) -> String {
    let mut out = format!("Plan for {}", plan.date);
    if let Some(t) = &plan.from_template {
        out.push_str(&format!("  (from template {})", t.0));
    }
    out.push('\n');
    if plan.blocks.is_empty() {
        out.push_str("  (no blocks)\n");
    }
    for (i, b) in plan.blocks.iter().enumerate() {
        out.push_str("  ");
        out.push_str(&render_block_line(i, b));
        out.push('\n');
    }
    out
}

fn print_json<T: serde::Serialize>(v: &T) -> eyre::Result<()> {
    println!(
        "{}",
        serde_json::to_string_pretty(v).map_err(|e| eyre::eyre!("json: {e}"))?
    );
    Ok(())
}

/// Does a calendar event (RFC-3339 start/end) land on the local date?
fn event_on_date(ev: &CalEvent, date: &str) -> bool {
    let day = chrono::NaiveDate::parse_from_str(date, "%Y-%m-%d").ok();
    match (
        chrono::DateTime::parse_from_rfc3339(&ev.start),
        chrono::DateTime::parse_from_rfc3339(&ev.end),
        day,
    ) {
        (Ok(s), Ok(e), Some(d)) => {
            let sl = s.with_timezone(&chrono::Local).date_naive();
            let el = e.with_timezone(&chrono::Local).date_naive();
            sl <= d && d <= el
        }
        _ => ev.start.get(..10) == Some(date),
    }
}

#[derive(serde::Serialize)]
struct DayView {
    date: String,
    plan: Option<DayPlan>,
    events: Vec<CalEvent>,
}

async fn fetch_day_view(
    plans: &scheduling_proto::DayPlansClient,
    events: &[CalEvent],
    date: &str,
) -> eyre::Result<DayView> {
    let plan = load_plan(plans, date).await?;
    let evs = events
        .iter()
        .filter(|e| event_on_date(e, date))
        .cloned()
        .collect();
    Ok(DayView {
        date: date.to_owned(),
        plan,
        events: evs,
    })
}

fn render_day_view(v: &DayView) -> String {
    let mut out = String::new();
    match &v.plan {
        Some(p) => out.push_str(&render_plan(p)),
        None => out.push_str(&format!(
            "No saved plan for {} — `task plan from-template {}` materializes one\n",
            v.date, v.date
        )),
    }
    if !v.events.is_empty() {
        out.push_str("  events:\n");
        for e in &v.events {
            let span = match (
                chrono::DateTime::parse_from_rfc3339(&e.start),
                chrono::DateTime::parse_from_rfc3339(&e.end),
            ) {
                (Ok(s), Ok(en)) => format!(
                    "{}–{}",
                    s.with_timezone(&chrono::Local).format("%H:%M"),
                    en.with_timezone(&chrono::Local).format("%H:%M")
                ),
                _ => format!("{} – {}", e.start, e.end),
            };
            out.push_str(&format!(
                "    {span}  {}{}\n",
                e.title,
                if e.all_day { " (all day)" } else { "" }
            ));
        }
    }
    out
}

// ─────────────────────────────────────────────────────────────────────
// Timer plumbing (shared with `task brief`)
// ─────────────────────────────────────────────────────────────────────

pub(crate) struct TimerCtx {
    pub store: timer::store::Store,
    pub user_id: uuid::Uuid,
}

/// Open the same local timer store `task timer …` uses (OrgRoot-
/// resolved sqlite db, deterministic local-owner user id).
pub(crate) async fn timer_ctx(org_override: Option<&str>) -> eyre::Result<TimerCtx> {
    use sea_orm::Database;
    use sea_orm_migration::MigratorTrait;
    use std::sync::Arc;
    use timer::store::{Store, VaultProjectDefaults};

    let ctx = crate::org_ctx::resolve_active(org_override)?;
    let db_url = std::env::var("TASK_TIMER_DB")
        .unwrap_or_else(|_| format!("sqlite://{}?mode=rwc", ctx.root.timer_db().display()));
    let vault_root = std::env::var("TASK_VAULT_ROOT")
        .map_or_else(|_| ctx.root.vault_dir(), std::path::PathBuf::from);
    let org_id = std::env::var("TASK_ORG_ID")
        .ok()
        .and_then(|s| s.parse::<uuid::Uuid>().ok())
        .or_else(|| ctx.root.manifest().ok().map(|m| m.id))
        .unwrap_or_else(|| uuid::Uuid::parse_str("00000000-0000-0000-0000-00000000000a").unwrap());
    let user_id = std::env::var("TASK_USER_ID")
        .ok()
        .and_then(|s| s.parse::<uuid::Uuid>().ok())
        .unwrap_or_else(|| crate::timer_owner_id(org_id));
    let conn = Database::connect(&db_url)
        .await
        .map_err(|e| eyre::eyre!("connect timer db `{db_url}`: {e}"))?;
    timer::Migrator::up(&conn, None)
        .await
        .map_err(|e| eyre::eyre!("timer migrations: {e}"))?;
    let store = Store::new(conn, Arc::new(VaultProjectDefaults { vault_root }));
    Ok(TimerCtx { store, user_id })
}

// ─────────────────────────────────────────────────────────────────────
// Verbs
// ─────────────────────────────────────────────────────────────────────

pub async fn run_plan(cmd: PlanCmd) -> eyre::Result<()> {
    match cmd {
        PlanCmd::Show {
            when,
            org,
            server,
            json,
        } => run_show(when, org, server, json).await,
        PlanCmd::Block(cmd) => run_block(cmd).await,
        PlanCmd::Assign {
            block,
            task,
            date,
            org,
            server,
            json,
        } => run_assign(block, task, date, org, server, json).await,
        PlanCmd::FromTemplate {
            date,
            template,
            force,
            org,
            server,
            json,
        } => run_from_template(date, template, force, org, server, json).await,
        PlanCmd::Diff {
            date,
            org,
            server,
            json,
        } => run_diff(date, org, server, json).await,
    }
}

async fn run_show(
    when: String,
    org: Option<String>,
    server: Option<String>,
    json: bool,
) -> eyre::Result<()> {
    let (slug, plans) = day_plans_client(server.clone(), org).await?;
    let url = crate::resolve_org_vox_url(server, &slug);
    let cal: scheduling_proto::CalendarEventsClient = crate::establish_for_url(&url).await?;
    let events = cal
        .list_events()
        .await
        .map_err(|e| eyre::eyre!("list_events: {e:?}"))?;

    if when.trim().eq_ignore_ascii_case("week") {
        let today = chrono::Local::now().date_naive();
        let mut days = Vec::with_capacity(7);
        for off in 0..7 {
            let d = (today + chrono::Days::new(off)).format("%Y-%m-%d").to_string();
            days.push(fetch_day_view(&plans, &events, &d).await?);
        }
        if json {
            return print_json(&days);
        }
        for v in &days {
            println!("{}", render_day_view(v));
        }
        return Ok(());
    }

    let date = parse_date_arg(&when)?;
    let view = fetch_day_view(&plans, &events, &date).await?;
    if json {
        return print_json(&view);
    }
    print!("{}", render_day_view(&view));
    Ok(())
}

/// Shared tail for every mutation: save, then print confirmation +
/// the updated plan (or `--json` the whole plan).
async fn finish_mutation(
    client: &scheduling_proto::DayPlansClient,
    plan: DayPlan,
    human_msg: &str,
    json: bool,
) -> eyre::Result<()> {
    save_plan(client, plan.clone()).await?;
    if json {
        return print_json(&DayView {
            date: plan.date.clone(),
            plan: Some(plan),
            events: Vec::new(),
        });
    }
    println!("{human_msg}");
    print!("{}", render_plan(&plan));
    Ok(())
}

fn overlap_bail(blocks: &[PlannedBlock], conflict: usize, start: TimeOfDay, end: TimeOfDay) -> eyre::Report {
    let c = &blocks[conflict];
    eyre::eyre!(
        "{}–{} overlaps block {} ({}–{} \"{}\") — pick a free range, move that block first, \
         or pass --force",
        fmt_tod(start),
        fmt_tod(end),
        conflict + 1,
        fmt_tod(c.start),
        fmt_tod(c.end),
        c.label,
    )
}

async fn run_block(cmd: BlockCmd) -> eyre::Result<()> {
    match cmd {
        BlockCmd::Add {
            label,
            range,
            category,
            assign,
            note,
            date,
            force,
            org,
            server,
            json,
        } => {
            let date = parse_date_arg(&date)?;
            let (start, end) = parse_time_range(&range).map_err(|e| eyre::eyre!(e))?;
            let category = category
                .as_deref()
                .map(parse_category)
                .transpose()
                .map_err(|e| eyre::eyre!(e))?
                .unwrap_or_default();
            let (slug, client) = day_plans_client(server.clone(), org).await?;
            let assignment = match &assign {
                Some(spec) => Some(resolve_assign_spec(spec, server, &slug).await?),
                None => None,
            };
            let mut plan = load_plan(&client, &date).await?.unwrap_or(DayPlan {
                date: date.clone(),
                from_template: None,
                blocks: PlannedBlocks::default(),
            });
            if !force {
                if let Some(i) = find_overlap(&plan.blocks, start, end, None) {
                    return Err(overlap_bail(&plan.blocks, i, start, end));
                }
            }
            plan.blocks.push(PlannedBlock {
                id: TimeBlockId(uuid::Uuid::new_v4().to_string()),
                start,
                end,
                label: label.clone(),
                category,
                note,
                assignment,
            });
            sort_blocks(&mut plan.blocks);
            let msg = format!("added \"{label}\" {}–{} on {date}", fmt_tod(start), fmt_tod(end));
            finish_mutation(&client, plan, &msg, json).await
        }
        BlockCmd::Move {
            block,
            to,
            by,
            date,
            force,
            org,
            server,
            json,
        } => {
            let date = parse_date_arg(&date)?;
            let (_slug, client) = day_plans_client(server, org).await?;
            let mut plan = require_plan(load_plan(&client, &date).await?, &date)?;
            let idx = resolve_block_ref(&plan.blocks, &block).map_err(|e| eyre::eyre!(e))?;
            let old = plan.blocks[idx].clone();
            let dur = i32::from(old.end.minutes_since_midnight) - i32::from(old.start.minutes_since_midnight);
            let new_start: i32 = match (&to, &by) {
                (Some(t), None) => i32::from(
                    parse_time_of_day(t)
                        .map_err(|e| eyre::eyre!(e))?
                        .minutes_since_midnight,
                ),
                (None, Some(d)) => {
                    i32::from(old.start.minutes_since_midnight)
                        + parse_delta_minutes(d).map_err(|e| eyre::eyre!(e))?
                }
                _ => eyre::bail!("pass exactly one of --to <time> or --by <±delta>"),
            };
            let new_end = new_start + dur;
            if new_start < 0 || new_end > 24 * 60 {
                eyre::bail!(
                    "move puts \"{}\" outside the day ({}..{}) — it must stay within 00:00-24:00",
                    old.label,
                    new_start,
                    new_end
                );
            }
            let start = TimeOfDay {
                minutes_since_midnight: new_start as u16,
            };
            let end = TimeOfDay {
                minutes_since_midnight: new_end as u16,
            };
            if !force {
                if let Some(i) = find_overlap(&plan.blocks, start, end, Some(idx)) {
                    return Err(overlap_bail(&plan.blocks, i, start, end));
                }
            }
            plan.blocks[idx].start = start;
            plan.blocks[idx].end = end;
            sort_blocks(&mut plan.blocks);
            let msg = format!(
                "moved \"{}\" {}–{} → {}–{} on {date}",
                old.label,
                fmt_tod(old.start),
                fmt_tod(old.end),
                fmt_tod(start),
                fmt_tod(end)
            );
            finish_mutation(&client, plan, &msg, json).await
        }
        BlockCmd::Resize {
            block,
            end,
            date,
            force,
            org,
            server,
            json,
        } => {
            let date = parse_date_arg(&date)?;
            let (_slug, client) = day_plans_client(server, org).await?;
            let mut plan = require_plan(load_plan(&client, &date).await?, &date)?;
            let idx = resolve_block_ref(&plan.blocks, &block).map_err(|e| eyre::eyre!(e))?;
            let new_end = parse_time_of_day(&end).map_err(|e| eyre::eyre!(e))?;
            let b = plan.blocks[idx].clone();
            if new_end.minutes_since_midnight <= b.start.minutes_since_midnight {
                eyre::bail!(
                    "new end {} must be after \"{}\"'s start {}",
                    fmt_tod(new_end),
                    b.label,
                    fmt_tod(b.start)
                );
            }
            if !force {
                if let Some(i) = find_overlap(&plan.blocks, b.start, new_end, Some(idx)) {
                    return Err(overlap_bail(&plan.blocks, i, b.start, new_end));
                }
            }
            plan.blocks[idx].end = new_end;
            sort_blocks(&mut plan.blocks);
            let msg = format!(
                "resized \"{}\" {}–{} → {}–{} on {date}",
                b.label,
                fmt_tod(b.start),
                fmt_tod(b.end),
                fmt_tod(b.start),
                fmt_tod(new_end)
            );
            finish_mutation(&client, plan, &msg, json).await
        }
        BlockCmd::Rm {
            block,
            date,
            org,
            server,
            json,
        } => {
            let date = parse_date_arg(&date)?;
            let (_slug, client) = day_plans_client(server, org).await?;
            let mut plan = require_plan(load_plan(&client, &date).await?, &date)?;
            let idx = resolve_block_ref(&plan.blocks, &block).map_err(|e| eyre::eyre!(e))?;
            let removed = plan.blocks.remove(idx);
            let msg = format!(
                "removed \"{}\" {}–{} from {date}",
                removed.label,
                fmt_tod(removed.start),
                fmt_tod(removed.end)
            );
            finish_mutation(&client, plan, &msg, json).await
        }
    }
}

async fn run_assign(
    block: String,
    task_ref: String,
    date: String,
    org: Option<String>,
    server: Option<String>,
    json: bool,
) -> eyre::Result<()> {
    let date = parse_date_arg(&date)?;
    let (slug, client) = day_plans_client(server.clone(), org).await?;
    let mut plan = require_plan(load_plan(&client, &date).await?, &date)?;
    let idx = resolve_block_ref(&plan.blocks, &block).map_err(|e| eyre::eyre!(e))?;
    let url = crate::resolve_org_vox_url(server, &slug);
    let tasks: task::TaskServiceClient = crate::establish_for_url(&url).await?;
    let t = resolve_task(&tasks, &task_ref).await?;
    let label = plan.blocks[idx].label.clone();
    plan.blocks[idx].assignment = Some(BlockAssignment {
        kind: "task".into(),
        title: t.title.clone(),
        ref_id: Some(t.id.to_string()),
    });
    let msg = format!("assigned task \"{}\" ({}) to block \"{label}\" on {date}", t.title, t.id);
    finish_mutation(&client, plan, &msg, json).await
}

async fn run_from_template(
    date: String,
    template: Option<String>,
    force: bool,
    org: Option<String>,
    server: Option<String>,
    json: bool,
) -> eyre::Result<()> {
    let date = parse_date_arg(&date)?;
    let (slug, client) = day_plans_client(server.clone(), org).await?;
    let url = crate::resolve_org_vox_url(server, &slug);
    let templates_client: scheduling_proto::DayTemplatesClient =
        crate::establish_for_url(&url).await?;
    let templates = templates_client
        .list_day_templates()
        .await
        .map_err(|e| eyre::eyre!("list_day_templates: {e:?}"))?;
    if templates.is_empty() {
        eyre::bail!(
            "no day templates exist for org `{slug}` — create one in the calendar UI \
             (or seed `Projects/Scheduling/templates/`) first"
        );
    }
    let names = || {
        templates
            .iter()
            .map(|t| t.name.as_str())
            .collect::<Vec<_>>()
            .join(", ")
    };
    let tpl = if let Some(want) = &template {
        let needle = want.to_lowercase();
        let mut hits: Vec<_> = templates
            .iter()
            .filter(|t| t.name.to_lowercase() == needle)
            .collect();
        if hits.is_empty() {
            hits = templates
                .iter()
                .filter(|t| t.name.to_lowercase().starts_with(&needle))
                .collect();
        }
        match hits.as_slice() {
            [one] => (*one).clone(),
            [] => eyre::bail!("no template named `{want}` — available: {}", names()),
            _ => eyre::bail!(
                "`{want}` is ambiguous — matches: {}",
                hits.iter().map(|t| t.name.as_str()).collect::<Vec<_>>().join(", ")
            ),
        }
    } else {
        if templates.len() > 1 {
            eyre::bail!("multiple templates ({}) — pass --template <name>", names());
        }
        templates[0].clone()
    };
    if !force {
        if let Some(existing) = load_plan(&client, &date).await? {
            eyre::bail!(
                "a plan already exists for {date} ({} block(s)) — pass --force to overwrite it",
                existing.blocks.len()
            );
        }
    }
    let mut blocks: Vec<PlannedBlock> = tpl
        .blocks
        .iter()
        .map(|tb| PlannedBlock {
            id: tb.id.clone(),
            start: tb.start,
            end: tb.end,
            label: tb.label.clone(),
            category: tb.category,
            note: tb.note.clone(),
            assignment: None,
        })
        .collect();
    sort_blocks(&mut blocks);
    let plan = DayPlan {
        date: date.clone(),
        from_template: Some(tpl.id.clone()),
        blocks: blocks.into(),
    };
    let msg = format!(
        "materialized {date} from template \"{}\" ({} blocks)",
        tpl.name,
        plan.blocks.len()
    );
    finish_mutation(&client, plan, &msg, json).await
}

// ── task next ────────────────────────────────────────────────────────

#[derive(serde::Serialize)]
struct NextView {
    now: String,
    date: String,
    current: Option<CurrentBlock>,
    next: Option<PlannedBlock>,
    /// Populated only when no plan exists for today.
    fallback_task: Option<task::TaskInfo>,
}

#[derive(serde::Serialize)]
struct CurrentBlock {
    #[serde(flatten)]
    block: PlannedBlock,
    remaining_min: u16,
}

pub async fn run_next(args: NextArgs) -> eyre::Result<()> {
    let NextArgs { org, server, json } = args;
    let (slug, plans) = day_plans_client(server.clone(), org).await?;
    let now_local = chrono::Local::now();
    let date = now_local.date_naive().format("%Y-%m-%d").to_string();
    let now_min = u16::try_from(
        chrono::Timelike::hour(&now_local) * 60 + chrono::Timelike::minute(&now_local),
    )
    .unwrap_or(0);
    let plan = load_plan(&plans, &date).await?;

    let mut view = NextView {
        now: format!("{:02}:{:02}", now_min / 60, now_min % 60),
        date: date.clone(),
        current: None,
        next: None,
        fallback_task: None,
    };

    if let Some(plan) = &plan {
        let current = plan.blocks.iter().enumerate().find(|(_, b)| {
            b.start.minutes_since_midnight <= now_min && now_min < b.end.minutes_since_midnight
        });
        view.current = current.map(|(_, b)| CurrentBlock {
            block: b.clone(),
            remaining_min: b.end.minutes_since_midnight - now_min,
        });
        view.next = plan
            .blocks
            .iter()
            .filter(|b| b.start.minutes_since_midnight > now_min)
            .min_by_key(|b| b.start.minutes_since_midnight)
            .cloned();
    } else {
        // No plan — fall back to the next due (or scheduled) task.
        let url = crate::resolve_org_vox_url(server, &slug);
        let tasks: task::TaskServiceClient = crate::establish_for_url(&url).await?;
        let all = tasks
            .list()
            .await
            .map_err(|e| eyre::eyre!("list tasks: {e:?}"))?;
        view.fallback_task = next_due_task(all);
    }

    if json {
        return print_json(&view);
    }
    match (&view.current, &view.next) {
        (Some(c), _) => {
            println!(
                "Now {} — in {} (ends {}, {} left)",
                view.now,
                c.block.label,
                fmt_tod(c.block.end),
                fmt_min(c.remaining_min)
            );
            if let Some(a) = &c.block.assignment {
                println!("  assignment: {}: {}", a.kind, a.title);
            }
            if let Some(n) = &view.next {
                println!("Next: {}", render_block_line(0, n).trim_start_matches(" 1. "));
            }
        }
        (None, Some(n)) => {
            println!(
                "Now {} — between blocks; next up at {}: {} [{}]",
                view.now,
                fmt_tod(n.start),
                n.label,
                category_name(n.category)
            );
            if let Some(a) = &n.assignment {
                println!("  assignment: {}: {}", a.kind, a.title);
            }
        }
        (None, None) => match &view.fallback_task {
            Some(t) => {
                println!("No plan for {date} — next due task:");
                println!(
                    "  {}  {}  (due {})",
                    t.id,
                    t.title,
                    t.due.as_deref().or(t.scheduled.as_deref()).unwrap_or("?")
                );
            }
            None if plan.is_some() => {
                println!("Now {} — the day's blocks are done. Nothing left on the plan.", view.now);
            }
            None => {
                println!(
                    "No plan for {date} and nothing due — `task plan from-template {date}` \
                     or `task plan block add` to shape the day"
                );
            }
        },
    }
    Ok(())
}

/// Earliest-due open task (due, falling back to scheduled).
pub(crate) fn next_due_task(tasks: Vec<task::TaskInfo>) -> Option<task::TaskInfo> {
    tasks
        .into_iter()
        .filter(|t| !matches!(t.status.as_str(), "done" | "cancelled" | "archived"))
        .filter_map(|t| {
            t.due
                .clone()
                .or_else(|| t.scheduled.clone())
                .map(|d| (d, t))
        })
        .min_by(|a, b| a.0.cmp(&b.0))
        .map(|(_, t)| t)
}

// ── task plan diff ───────────────────────────────────────────────────

#[derive(serde::Serialize)]
struct DiffView {
    date: String,
    blocks: Vec<DiffBlock>,
    unattributed: Vec<DiffSession>,
}

#[derive(serde::Serialize)]
struct DiffBlock {
    index: usize,
    label: String,
    start: String,
    end: String,
    category: String,
    planned_min: u16,
    actual_min: u16,
    sessions: Vec<DiffSession>,
}

#[derive(serde::Serialize)]
struct DiffSession {
    id: uuid::Uuid,
    description: String,
    start: String,
    end: Option<String>,
    overlap_min: u16,
}

async fn run_diff(
    date: String,
    org: Option<String>,
    server: Option<String>,
    json: bool,
) -> eyre::Result<()> {
    use timer_proto::service::TimerService;

    let date = parse_date_arg(&date)?;
    let day = chrono::NaiveDate::parse_from_str(&date, "%Y-%m-%d")
        .map_err(|e| eyre::eyre!("date `{date}`: {e}"))?;
    let (_slug, plans) = day_plans_client(server, org.clone()).await?;
    let plan = require_plan(load_plan(&plans, &date).await?, &date)?;

    // Local-midnight bounds → UTC for the session filter. `since`
    // widened a day so an overnight session still attributes.
    let day_start = day
        .and_hms_opt(0, 0, 0)
        .and_then(|dt| dt.and_local_timezone(chrono::Local).single())
        .ok_or_else(|| eyre::eyre!("can't resolve local midnight for {date}"))?;
    let day_end = day_start + chrono::Duration::days(1);
    let tctx = timer_ctx(org.as_deref()).await?;
    let sessions = tctx
        .store
        .list_sessions(timer_proto::WorkSessionFilter {
            user_id: Some(tctx.user_id),
            project_id: None,
            since: Some((day_start - chrono::Duration::days(1)).with_timezone(&chrono::Utc)),
            until: Some(day_end.with_timezone(&chrono::Utc)),
            billable: None,
            open: None,
        })
        .await
        .map_err(|e| eyre::eyre!("list_sessions: {e}"))?;

    // Project each session onto minutes-since-local-midnight,
    // clamped to the day; drop sessions that miss the day entirely.
    let now = chrono::Local::now();
    let mut spans: Vec<(u16, u16)> = Vec::new();
    let mut kept: Vec<&timer_proto::WorkSession> = Vec::new();
    for s in &sessions {
        let st = s.start_time.with_timezone(&chrono::Local);
        let en = s
            .end_time
            .map_or(now, |e| e.with_timezone(&chrono::Local));
        if en <= day_start || st >= day_end {
            continue;
        }
        let lo = (st.max(day_start) - day_start).num_minutes().clamp(0, 1440) as u16;
        let hi = (en.min(day_end) - day_start).num_minutes().clamp(0, 1440) as u16;
        if hi > lo {
            spans.push((lo, hi));
            kept.push(s);
        }
    }

    let block_spans: Vec<(u16, u16)> = plan
        .blocks
        .iter()
        .map(|b| (b.start.minutes_since_midnight, b.end.minutes_since_midnight))
        .collect();
    let (per_block, unattributed) = attribute_sessions(&block_spans, &spans);

    let to_diff_session = |si: usize, overlap: u16| {
        let s = kept[si];
        DiffSession {
            id: s.id,
            description: s.description.clone(),
            start: s.start_time.with_timezone(&chrono::Local).to_rfc3339(),
            end: s
                .end_time
                .map(|e| e.with_timezone(&chrono::Local).to_rfc3339()),
            overlap_min: overlap,
        }
    };
    let view = DiffView {
        date: date.clone(),
        blocks: plan
            .blocks
            .iter()
            .enumerate()
            .map(|(i, b)| DiffBlock {
                index: i + 1,
                label: b.label.clone(),
                start: fmt_tod(b.start),
                end: fmt_tod(b.end),
                category: category_name(b.category).to_owned(),
                planned_min: b.end.minutes_since_midnight - b.start.minutes_since_midnight,
                actual_min: per_block[i].iter().map(|&(_, o)| o).sum(),
                sessions: per_block[i]
                    .iter()
                    .map(|&(si, o)| to_diff_session(si, o))
                    .collect(),
            })
            .collect(),
        unattributed: unattributed
            .iter()
            .map(|&si| to_diff_session(si, spans[si].1 - spans[si].0))
            .collect(),
    };

    if json {
        return print_json(&view);
    }
    println!("Plan vs actual — {date}");
    for b in &view.blocks {
        let pct = if b.planned_min == 0 {
            0
        } else {
            u32::from(b.actual_min) * 100 / u32::from(b.planned_min)
        };
        println!(
            "{:>2}. {}–{}  {:<24} planned {:<6} actual {:<6} ({pct}%)",
            b.index,
            b.start,
            b.end,
            b.label,
            fmt_min(b.planned_min),
            fmt_min(b.actual_min),
        );
        for s in &b.sessions {
            println!(
                "      {}  {}",
                fmt_min(s.overlap_min),
                if s.description.is_empty() { "(no description)" } else { &s.description }
            );
        }
    }
    if !view.unattributed.is_empty() {
        println!("  unattributed:");
        for s in &view.unattributed {
            println!(
                "      {}  {}",
                fmt_min(s.overlap_min),
                if s.description.is_empty() { "(no description)" } else { &s.description }
            );
        }
    }
    Ok(())
}

// ─────────────────────────────────────────────────────────────────────
// Tests
// ─────────────────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;

    fn tod(h: u16, m: u16) -> TimeOfDay {
        TimeOfDay {
            minutes_since_midnight: h * 60 + m,
        }
    }

    fn block(label: &str, s: (u16, u16), e: (u16, u16)) -> PlannedBlock {
        PlannedBlock {
            id: TimeBlockId(label.to_owned()),
            start: tod(s.0, s.1),
            end: tod(e.0, e.1),
            label: label.to_owned(),
            category: BlockCategory::Other,
            note: None,
            assignment: None,
        }
    }

    // ── time parsing ────────────────────────────────────────────

    #[test]
    fn parses_24h_and_meridiem_times() {
        assert_eq!(parse_time_of_day("9:00").unwrap(), tod(9, 0));
        assert_eq!(parse_time_of_day("09:00").unwrap(), tod(9, 0));
        assert_eq!(parse_time_of_day("14:30").unwrap(), tod(14, 30));
        assert_eq!(parse_time_of_day("9am").unwrap(), tod(9, 0));
        assert_eq!(parse_time_of_day("9:30pm").unwrap(), tod(21, 30));
        assert_eq!(parse_time_of_day("12am").unwrap(), tod(0, 0));
        assert_eq!(parse_time_of_day("12pm").unwrap(), tod(12, 0));
        assert_eq!(parse_time_of_day("24:00").unwrap(), tod(24, 0));
        assert_eq!(parse_time_of_day("9").unwrap(), tod(9, 0));
    }

    #[test]
    fn rejects_bad_times() {
        assert!(parse_time_of_day("25:00").is_err());
        assert!(parse_time_of_day("9:75").is_err());
        assert!(parse_time_of_day("13pm").is_err());
        assert!(parse_time_of_day("lunch").is_err());
    }

    #[test]
    fn parses_ranges_and_rejects_inverted() {
        let (s, e) = parse_time_range("9:00-10:30").unwrap();
        assert_eq!((s, e), (tod(9, 0), tod(10, 30)));
        let (s, e) = parse_time_range("9am-12pm").unwrap();
        assert_eq!((s, e), (tod(9, 0), tod(12, 0)));
        assert!(parse_time_range("10:30-9:00").is_err());
        assert!(parse_time_range("9:00").is_err());
    }

    #[test]
    fn parses_signed_deltas() {
        assert_eq!(parse_delta_minutes("+30m").unwrap(), 30);
        assert_eq!(parse_delta_minutes("-45m").unwrap(), -45);
        assert_eq!(parse_delta_minutes("1h15m").unwrap(), 75);
        assert_eq!(parse_delta_minutes("-1h").unwrap(), -60);
        assert_eq!(parse_delta_minutes("90").unwrap(), 90);
        assert!(parse_delta_minutes("0m").is_err());
        assert!(parse_delta_minutes("soon").is_err());
    }

    // ── overlap checker ─────────────────────────────────────────

    #[test]
    fn overlap_detects_intersections() {
        let blocks = vec![
            block("morning", (9, 0), (10, 30)),
            block("midday", (12, 0), (13, 0)),
        ];
        // Straddles the first block.
        assert_eq!(find_overlap(&blocks, tod(10, 0), tod(11, 0), None), Some(0));
        // Fully inside the second.
        assert_eq!(find_overlap(&blocks, tod(12, 15), tod(12, 45), None), Some(1));
        // Engulfs the second.
        assert_eq!(find_overlap(&blocks, tod(11, 0), tod(14, 0), None), Some(1));
    }

    #[test]
    fn overlap_allows_touching_and_free_ranges() {
        let blocks = vec![block("morning", (9, 0), (10, 30))];
        // Touching boundaries are fine (half-open ranges).
        assert_eq!(find_overlap(&blocks, tod(10, 30), tod(11, 0), None), None);
        assert_eq!(find_overlap(&blocks, tod(8, 0), tod(9, 0), None), None);
        assert_eq!(find_overlap(&blocks, tod(15, 0), tod(16, 0), None), None);
    }

    #[test]
    fn overlap_ignores_the_block_being_edited() {
        let blocks = vec![
            block("a", (9, 0), (10, 0)),
            block("b", (10, 0), (11, 0)),
        ];
        // Growing block 0 into its own old range is fine, but hitting b isn't.
        assert_eq!(find_overlap(&blocks, tod(9, 0), tod(10, 0), Some(0)), None);
        assert_eq!(find_overlap(&blocks, tod(9, 0), tod(10, 30), Some(0)), Some(1));
    }

    // ── block-ref resolution ────────────────────────────────────

    #[test]
    fn block_ref_by_one_based_index() {
        let blocks = vec![block("a", (9, 0), (10, 0)), block("b", (10, 0), (11, 0))];
        assert_eq!(resolve_block_ref(&blocks, "1").unwrap(), 0);
        assert_eq!(resolve_block_ref(&blocks, "2").unwrap(), 1);
        assert!(resolve_block_ref(&blocks, "0").is_err());
        assert!(resolve_block_ref(&blocks, "3").is_err());
    }

    #[test]
    fn block_ref_by_case_insensitive_label_prefix() {
        let blocks = vec![
            block("Deep work", (9, 0), (10, 0)),
            block("Lunch", (12, 0), (13, 0)),
        ];
        assert_eq!(resolve_block_ref(&blocks, "deep").unwrap(), 0);
        assert_eq!(resolve_block_ref(&blocks, "LUN").unwrap(), 1);
        assert!(resolve_block_ref(&blocks, "dinner").is_err());
    }

    #[test]
    fn block_ref_ambiguous_prefix_lists_matches() {
        let blocks = vec![
            block("Block 1", (9, 0), (10, 0)),
            block("Block 2", (10, 0), (11, 0)),
        ];
        let err = resolve_block_ref(&blocks, "block").unwrap_err();
        assert!(err.contains("ambiguous"), "got: {err}");
        assert!(err.contains("Block 1") && err.contains("Block 2"), "got: {err}");
    }

    // ── diff attribution ────────────────────────────────────────

    #[test]
    fn attribution_splits_a_session_across_blocks() {
        // Blocks 9-10 and 10-11; session 9:30-10:30 → 30m each.
        let blocks = vec![(540, 600), (600, 660)];
        let sessions = vec![(570, 630)];
        let (per_block, un) = attribute_sessions(&blocks, &sessions);
        assert_eq!(per_block[0], vec![(0, 30)]);
        assert_eq!(per_block[1], vec![(0, 30)]);
        assert!(un.is_empty());
    }

    #[test]
    fn attribution_marks_misses_unattributed() {
        let blocks = vec![(540, 600)];
        let sessions = vec![(720, 780), (540, 600)];
        let (per_block, un) = attribute_sessions(&blocks, &sessions);
        assert_eq!(per_block[0], vec![(1, 60)]);
        assert_eq!(un, vec![0]);
    }

    #[test]
    fn attribution_handles_zero_overlap_touch() {
        // Session that exactly touches a block boundary is no overlap.
        let blocks = vec![(540, 600)];
        let sessions = vec![(600, 660)];
        let (per_block, un) = attribute_sessions(&blocks, &sessions);
        assert!(per_block[0].is_empty());
        assert_eq!(un, vec![0]);
    }

    // ── category + misc ─────────────────────────────────────────

    #[test]
    fn category_roundtrip() {
        for name in [
            "reset",
            "spiritual",
            "meal",
            "exercise",
            "hygiene",
            "allocatable",
            "maintenance",
            "winddown",
            "sleep",
            "other",
        ] {
            let c = parse_category(name).unwrap();
            assert_eq!(category_name(c), name);
        }
        assert!(parse_category("naps").is_err());
        assert_eq!(parse_category("WORK").unwrap(), BlockCategory::Allocatable);
    }

    #[test]
    fn next_due_prefers_earliest_due_then_scheduled() {
        let mk = |title: &str, status: &str, due: Option<&str>, sched: Option<&str>| {
            task::TaskInfo {
                id: uuid::Uuid::nil(),
                path: String::new(),
                title: title.to_owned(),
                status: status.to_owned(),
                priority: "normal".into(),
                due: due.map(str::to_owned),
                scheduled: sched.map(str::to_owned),
                tags: task::model::StringList::default(),
                contexts: task::model::StringList::default(),
                projects: task::model::StringList::default(),
                project_id: None,
                milestone_id: None,
                time_estimate: None,
                time_entries: task::model::TimeEntries::default(),
                recurrence: None,
                recurrence_anchor: None,
                complete_instances: task::model::StringList::default(),
                completed_date: None,
                agent_profile: String::new(),
                dispatched_agent_tasks: task::model::StringList::default(),
                date_created: None,
                date_modified: None,
                details: String::new(),
                workflow: None,
            }
        };
        let tasks = vec![
            mk("done early", "done", Some("2026-01-01"), None),
            mk("later", "open", Some("2026-06-20"), None),
            mk("soonest", "open", None, Some("2026-06-12")),
        ];
        assert_eq!(next_due_task(tasks).unwrap().title, "soonest");
    }
}
