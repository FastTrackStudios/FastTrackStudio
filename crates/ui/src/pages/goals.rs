//! `/goals` — goal hierarchy + cycle anchoring.
//!
//! Renders Cody's lifetime / yearly / cycle goal tree as a
//! grouped list. Each goal carries:
//!
//! - status badge (aspiration / active / paused / achieved / abandoned)
//! - `kind` chip (lifetime / yearly / quarterly / cycle / weekly)
//! - cycle pill (when `cycle_id` resolves — `2026 Q3 C1` etc.)
//! - parent breadcrumb so the decomposition is visible
//!
//! Cycle resolution uses the `cycle` crate's pure-function
//! generator — same deterministic UUIDv5s as the seeded
//! goal markdown. No data fetch yet; replace the in-page
//! fixture with a vox-backed loader when the goal RPC
//! lands.

use chrono::Weekday;
use cycle::FirstWeekRule;
use dioxus::prelude::*;
use fts_ui::prelude::*;

#[derive(Clone, PartialEq)]
struct GoalCard {
    title: &'static str,
    kind: &'static str,
    status: &'static str,
    parent: Option<&'static str>,
    /// Pre-computed `(year, quarter, ordinal)` triple when the
    /// goal anchors to a cycle. `None` for non-cycle goals.
    cycle: Option<(i32, u8, u8)>,
    target_date: Option<&'static str>,
    summary: &'static str,
}

const GOALS: &[GoalCard] = &[
    // ── BUY A CAR ────────────────────────────────────────
    GoalCard {
        title: "Buy a Car",
        kind: "yearly",
        status: "aspiration",
        parent: None,
        cycle: None,
        target_date: Some("2027-01-01"),
        summary: "Replace the current vehicle by start of 2027.",
    },
    GoalCard {
        title: "Buy a Car — 2026",
        kind: "yearly",
        status: "active",
        parent: Some("Buy a Car"),
        cycle: None,
        target_date: Some("2026-12-31"),
        summary: "2026 track. Decomposes into down payment + research + close.",
    },
    GoalCard {
        title: "Down payment saved",
        kind: "yearly",
        status: "active",
        parent: Some("Buy a Car — 2026"),
        cycle: None,
        target_date: Some("2026-09-30"),
        summary: "Cash in place by end of Q3 so Q4 is negotiation, not scrambling.",
    },
    GoalCard {
        title: "Research + shortlist models",
        kind: "cycle",
        status: "aspiration",
        parent: Some("Buy a Car — 2026"),
        cycle: Some((2026, 3, 1)),
        target_date: Some("2026-07-26"),
        summary: "Test-drive, read reviews, narrow to 3 contenders.",
    },
    GoalCard {
        title: "Negotiate + close",
        kind: "cycle",
        status: "aspiration",
        parent: Some("Buy a Car — 2026"),
        cycle: Some((2026, 4, 2)),
        target_date: Some("2026-11-22"),
        summary: "Pick the dealer, negotiate, sign.",
    },
    // ── BUY A HOUSE ──────────────────────────────────────
    GoalCard {
        title: "Buy a House",
        kind: "lifetime",
        status: "aspiration",
        parent: None,
        cycle: None,
        target_date: None,
        summary: "Own a place rather than rent. Multi-year savings horizon.",
    },
    GoalCard {
        title: "Buy a House — 2026",
        kind: "yearly",
        status: "active",
        parent: Some("Buy a House"),
        cycle: None,
        target_date: Some("2026-12-31"),
        summary: "Foundation year — credit + savings + market scan. No purchase yet.",
    },
    GoalCard {
        title: "Credit readiness check",
        kind: "cycle",
        status: "aspiration",
        parent: Some("Buy a House — 2026"),
        cycle: Some((2026, 3, 2)),
        target_date: Some("2026-08-23"),
        summary: "Pull score, clean up, set up monitoring. Floor ≥ 720.",
    },
    GoalCard {
        title: "10% of down payment saved",
        kind: "yearly",
        status: "active",
        parent: Some("Buy a House — 2026"),
        cycle: None,
        target_date: Some("2026-12-31"),
        summary: "~0.83% per cycle. Reviewed monthly via finance review.",
    },
    GoalCard {
        title: "Market scan — first pass",
        kind: "cycle",
        status: "aspiration",
        parent: Some("Buy a House — 2026"),
        cycle: Some((2026, 4, 1)),
        target_date: Some("2026-10-25"),
        summary: "3 target neighborhoods, comps, price-floor/ceiling model.",
    },
    // ── GET MARRIED ──────────────────────────────────────
    GoalCard {
        title: "Get Married",
        kind: "lifetime",
        status: "aspiration",
        parent: None,
        cycle: None,
        target_date: None,
        summary: "Marry the right person. Shape daily decisions, no deadline.",
    },
    GoalCard {
        title: "Marriage readiness — 2026",
        kind: "yearly",
        status: "active",
        parent: Some("Get Married"),
        cycle: None,
        target_date: Some("2026-12-31"),
        summary: "Character + faith + financial readiness for 2026.",
    },
    GoalCard {
        title: "Character + faith — ongoing",
        kind: "lifetime",
        status: "active",
        parent: Some("Marriage readiness — 2026"),
        cycle: None,
        target_date: None,
        summary: "Daily spiritual time block + Bible Study project compound over years.",
    },
    GoalCard {
        title: "Financial readiness",
        kind: "yearly",
        status: "active",
        parent: Some("Marriage readiness — 2026"),
        cycle: None,
        target_date: Some("2026-12-31"),
        summary: "6-month emergency fund, no consumer debt, joint-finances-ready.",
    },
];

#[component]
pub fn GoalsView() -> Element {
    let lifetime_roots: Vec<&GoalCard> = GOALS.iter().filter(|g| g.parent.is_none()).collect();
    let total = GOALS.len();
    let cycle_anchored = GOALS.iter().filter(|g| g.cycle.is_some()).count();

    // Current cycle highlight — drives the "you're here" pill.
    let today = chrono::Local::now().date_naive();
    let now_cycle =
        cycle::cycle_for_date(today, Weekday::Mon, FirstWeekRule::AtLeastFourDaysInYear);

    rsx! {
        div { class: "mx-auto w-full max-w-5xl flex flex-col gap-6 p-6 lg:p-10",
            div { class: "flex items-start justify-between gap-4 flex-wrap",
                div { class: "flex flex-col gap-1",
                    Heading { level: HeadingLevel::H1, "Goals" }
                    Text {
                        variant: TextVariant::Muted,
                        "{total} goals · {cycle_anchored} anchored to a 2026 cycle. Lifetime aspirations decompose into yearly tracks and cycle-scoped milestones."
                    }
                }
                if let Some(c) = now_cycle.clone() {
                    div { class: "rounded-lg border border-border/40 bg-muted/20 px-3 py-2 text-xs",
                        Text { variant: TextVariant::Muted, class: "uppercase tracking-wider text-[10px]", "Now" }
                        div { class: "font-medium text-sm tabular-nums",
                            "{c.year} Q{c.quarter} C{c.ordinal}"
                        }
                        div { class: "text-muted-foreground tabular-nums",
                            "{c.start_date} → {c.end_date}"
                        }
                    }
                }
            }
            for root in lifetime_roots.iter() {
                {
                    let root = *root;
                    rsx! {
                        GoalBranch {
                            key: "{root.title}",
                            g: root.clone(),
                            now_cycle: now_cycle.clone(),
                        }
                    }
                }
            }
        }
    }
}

#[derive(Props, Clone, PartialEq)]
struct GoalBranchProps {
    g: GoalCard,
    now_cycle: Option<cycle::Cycle>,
}

#[component]
fn GoalBranch(props: GoalBranchProps) -> Element {
    let g = props.g.clone();
    // Walk all descendants of `g` (one level for the
    // existing data; recursive logic ready for deeper trees).
    let descendants: Vec<GoalCard> = collect_descendants(g.title);
    let now = props.now_cycle.clone();
    rsx! {
        Card {
            CardHeader {
                div { class: "flex items-start justify-between gap-2 flex-wrap",
                    CardTitle { "{g.title}" }
                    div { class: "flex items-center gap-1.5",
                        KindChip { kind: g.kind }
                        StatusBadge {
                            variant: status_variant(g.status),
                            label: g.status.to_string(),
                        }
                    }
                }
                CardDescription { "{g.summary}" }
            }
            CardContent {
                if let Some(td) = g.target_date {
                    Text { variant: TextVariant::Muted, class: "text-xs",
                        "Target: {td}"
                    }
                }
                if !descendants.is_empty() {
                    div { class: "mt-3 flex flex-col gap-1.5",
                        for d in descendants.iter() {
                            GoalRow {
                                key: "{d.title}",
                                g: d.clone(),
                                now_cycle: now.clone(),
                            }
                        }
                    }
                }
            }
        }
    }
}

#[derive(Props, Clone, PartialEq)]
struct GoalRowProps {
    g: GoalCard,
    now_cycle: Option<cycle::Cycle>,
}

#[component]
fn GoalRow(props: GoalRowProps) -> Element {
    let g = props.g.clone();
    // Indent based on depth — count parent chain length.
    let depth = depth_of(g.title);
    let indent_px = (depth as u32) * 16;
    let is_current_cycle = matches!(
        (g.cycle, &props.now_cycle),
        (Some((y, q, o)), Some(c)) if y == c.year && q == c.quarter && o == c.ordinal
    );
    let row_cls = if is_current_cycle {
        "flex items-center justify-between gap-2 rounded-md border border-primary/40 bg-primary/[0.04] px-3 py-2 text-sm"
    } else {
        "flex items-center justify-between gap-2 rounded-md bg-muted/30 px-3 py-2 text-sm"
    };
    rsx! {
        div {
            class: "{row_cls}",
            style: "margin-left: {indent_px}px;",
            div { class: "flex flex-col min-w-0 gap-0.5",
                span { class: "font-medium truncate", "{g.title}" }
                if let Some((y, q, o)) = g.cycle {
                    span {
                        class: if is_current_cycle {
                            "text-xs text-primary font-medium tabular-nums"
                        } else {
                            "text-xs text-muted-foreground tabular-nums"
                        },
                        "{y} Q{q} C{o}"
                        if is_current_cycle { " · current cycle" }
                    }
                } else if let Some(td) = g.target_date {
                    span { class: "text-xs text-muted-foreground tabular-nums",
                        "target {td}"
                    }
                }
            }
            div { class: "flex items-center gap-1.5 shrink-0",
                KindChip { kind: g.kind }
                StatusBadge {
                    variant: status_variant(g.status),
                    label: g.status.to_string(),
                }
            }
        }
    }
}

#[derive(Props, Clone, PartialEq)]
struct KindChipProps {
    kind: &'static str,
}

#[component]
fn KindChip(props: KindChipProps) -> Element {
    let cls = match props.kind {
        "lifetime" => "rounded-full px-2 py-0.5 bg-accent/30 text-accent-foreground text-[11px]",
        "yearly" => "rounded-full px-2 py-0.5 bg-primary/10 text-primary text-[11px]",
        "quarterly" => "rounded-full px-2 py-0.5 bg-muted/60 text-muted-foreground text-[11px]",
        "cycle" => "rounded-full px-2 py-0.5 bg-success/15 text-success text-[11px]",
        _ => "rounded-full px-2 py-0.5 bg-muted/40 text-muted-foreground text-[11px]",
    };
    let label = props.kind;
    rsx! {
        span { class: "{cls}", "{label}" }
    }
}

fn collect_descendants(parent_title: &str) -> Vec<GoalCard> {
    let mut out: Vec<GoalCard> = Vec::new();
    let direct: Vec<&GoalCard> = GOALS
        .iter()
        .filter(|g| g.parent == Some(parent_title))
        .collect();
    for d in direct {
        out.push(d.clone());
        out.extend(collect_descendants(d.title));
    }
    out
}

fn depth_of(title: &str) -> usize {
    let mut depth = 0usize;
    let mut current = title;
    loop {
        let Some(g) = GOALS.iter().find(|g| g.title == current) else {
            break;
        };
        match g.parent {
            Some(p) => {
                depth += 1;
                current = p;
            }
            None => break,
        }
    }
    depth.saturating_sub(1)
}

fn status_variant(status: &str) -> StatusBadgeVariant {
    match status {
        "active" | "achieved" => StatusBadgeVariant::Success,
        "paused" | "on-hold" => StatusBadgeVariant::Warning,
        "abandoned" | "cancelled" => StatusBadgeVariant::Danger,
        _ => StatusBadgeVariant::Neutral,
    }
}
