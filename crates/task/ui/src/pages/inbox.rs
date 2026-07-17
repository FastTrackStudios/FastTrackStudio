//! `/inbox` — the FLAP capture queue + daily review.
//!
//! Capture anything with near-zero friction (the quick-add box), then
//! honour the "temporal contract": read the open items again later and
//! either process them into something durable, snooze them to resurface
//! on a date, or let them go. The list shows `open` items oldest-first;
//! snoozed items stay hidden until their date (toggle "Show all" to see
//! everything, including processed + archived).
//!
//! State is the shared optimistic store ([`crate::stores`]): every
//! mutation — capture, status flips, snoozes, deletes, and the focused
//! ProcessReview decisions — patches the store instantly and reconciles
//! against the server (rollback + tray notification on failure), so
//! leaving review mode needs no refetch: the store already reflects
//! every decision.

use architect::Id;
use chrono::Utc;
use dioxus::prelude::*;
use fts_ui::prelude::*;
use inbox_proto::{review, InboxItem, ReviewResponse};

use crate::orgs::{OrgMeta, OrgSelection};
use crate::shell::mobile::MobileActionBar;
use crate::stores;

const INPUT_CLS: &str = "rounded-lg border border-input bg-input/30 px-3 py-2 text-sm transition-colors \
     focus-visible:border-ring focus-visible:outline-none focus-visible:ring-[3px] \
     focus-visible:ring-ring/50 placeholder:text-muted-foreground";

#[component]
pub fn InboxView() -> Element {
    let selection = use_context::<Signal<OrgSelection>>();
    let org_list = use_context::<Signal<Vec<OrgMeta>>>();

    // The org we capture into / review (first selected, or home).
    let slug = use_memo(move || {
        crate::orgs::selected_slugs(&selection.read(), &org_list.read())
            .into_iter()
            .next()
    });

    let mut draft = use_signal(String::new);
    let mut show_all = use_signal(|| false);
    // Focused daily-review ("process") mode + its frozen work queue.
    let mut processing = use_signal(|| false);
    let mut queue = use_signal(Vec::<InboxItem>::new);

    // The shared store: one AtomResult for the list; every mutation is
    // optimistic, so exiting process mode needs no reload.
    let result = stores::use_inbox_list();
    let store = stores::use_inbox_store();
    let muts = stores::use_inbox_mutations();

    // Capture the current draft as a fresh fleeting note: it appears
    // instantly as a typed temp row, then persists.
    let mut capture = move || {
        let text = draft.read().trim().to_string();
        if text.is_empty() {
            return;
        }
        let Some(s) = slug() else { return };
        draft.set(String::new());
        let id = uuid::Uuid::new_v4().to_string();
        let created = Utc::now().to_rfc3339();
        muts.capture(s, InboxItem::capture(id, text, "ui", created));
    };

    let today = Utc::now().date_naive().to_string();

    let all_rows: Vec<(Id<String>, InboxItem)> = result.value().cloned().unwrap_or_default();
    let load_err = result.error().cloned();
    let first_load = result.is_waiting() && result.value().is_none();

    let show_all_now = show_all();
    let rows: Vec<(Id<String>, InboxItem)> = all_rows
        .iter()
        .filter(|(_, it)| {
            show_all_now
                || (it.is_open()
                    && it
                        .resurface_on
                        .as_deref()
                        .is_none_or(|d| d <= today.as_str()))
        })
        .cloned()
        .collect();

    let open_count = all_rows.iter().filter(|(_, it)| it.is_open()).count();

    // The daily-review work set: open items whose snooze has elapsed,
    // oldest first (the fetch already sorts). Frozen into `queue` when
    // the user enters process mode so mutations don't reshuffle it.
    let due_open: Vec<InboxItem> = all_rows
        .iter()
        .filter(|(_, it)| {
            it.is_open()
                && it
                    .resurface_on
                    .as_deref()
                    .is_none_or(|d| d <= today.as_str())
        })
        .map(|(_, it)| it.clone())
        .collect();

    // Agent-proposed captures awaiting one-tap accept/dismiss.
    let suggested: Vec<(Id<String>, InboxItem)> = all_rows
        .iter()
        .filter(|(_, it)| it.status == InboxItem::STATUS_SUGGESTED)
        .cloned()
        .collect();

    // Focused review mode takes over the whole page. Decisions mutate
    // the shared store optimistically, so exiting just exits.
    if processing() {
        return rsx! {
            ProcessReview {
                items: queue(),
                slug,
                on_exit: move |()| processing.set(false),
            }
        };
    }

    rsx! {
        div { class: "mx-auto flex max-w-3xl flex-col gap-5 p-4 pb-14 sm:p-6 md:pb-6 lg:p-10",
            div { class: "flex items-center justify-between gap-3",
                Heading { level: HeadingLevel::H1, "Inbox" }
                if !due_open.is_empty() {
                    // Desktop CTA — on phones the sticky bottom action
                    // bar (thumb reach) carries the same action.
                    div { class: "hidden md:block",
                        Button {
                            variant: ButtonVariant::Primary,
                            size: ButtonSize::Small,
                            on_click: {
                                let q = due_open.clone();
                                move |_| {
                                    queue.set(q.clone());
                                    processing.set(true);
                                }
                            },
                            "Process {due_open.len()} →"
                        }
                    }
                    span { class: "text-sm text-muted-foreground md:hidden", "{open_count} open" }
                } else {
                    Text { variant: TextVariant::Muted, class: "text-sm", "{open_count} open" }
                }
            }
            Text {
                variant: TextVariant::Muted,
                class: "text-sm -mt-2",
                "Capture anything. Read it again tomorrow — process it, snooze it, or let it go.",
            }

            // ── Quick-add ──────────────────────────────────────────
            div { class: "flex gap-2",
                input {
                    class: "{INPUT_CLS} flex-1",
                    placeholder: "Capture a thought…",
                    value: "{draft}",
                    oninput: move |e| draft.set(e.value()),
                    onkeydown: move |e| {
                        if e.key() == Key::Enter {
                            capture();
                        }
                    },
                }
                Button {
                    variant: ButtonVariant::Primary,
                    on_click: move |_| capture(),
                    "Capture"
                }
            }

            // ── Filter toggle ──────────────────────────────────────
            div { class: "flex items-center gap-3 text-xs text-muted-foreground",
                button {
                    class: "underline-offset-2 hover:underline",
                    onclick: move |_| show_all.toggle(),
                    if show_all() {
                        "Showing all — show open only"
                    } else {
                        "Show all (processed + archived)"
                    }
                }
            }

            // ── Suggested (agent-proposed) — one-tap accept/dismiss ──
            if !suggested.is_empty() {
                div { class: "flex flex-col gap-2 rounded-xl border border-primary/30 bg-primary/5 p-3",
                    div { class: "flex items-baseline gap-2",
                        span { class: "text-sm font-medium text-foreground", "Suggested for you" }
                        span { class: "text-xs text-muted-foreground",
                            "{suggested.len()} from your sources — accept to add to the queue"
                        }
                    }
                    for (id, item) in suggested {
                        SuggestedRow { key: "{id}", pending: id.is_temp(), item, slug }
                    }
                }
            }

            // ── The queue ──────────────────────────────────────────
            if first_load {
                crate::states::LoadingState {}
            } else if rows.is_empty() {
                if let Some(err) = load_err {
                    crate::states::ErrorState {
                        title: "Couldn't load inbox",
                        message: err,
                        on_retry: move |()| store.reload(),
                    }
                } else {
                    crate::states::EmptyState {
                        title: "Inbox empty",
                        hint: "Capture a thought above — nothing to review right now.",
                    }
                }
            } else {
                div { class: "flex flex-col gap-2",
                    for (id, item) in rows {
                        InboxRow { key: "{id}", pending: id.is_temp(), item, slug }
                    }
                }
            }
        }
        // ── Mobile: sticky Process CTA above the tab bar ───────────
        if !due_open.is_empty() {
            MobileActionBar {
                button {
                    r#type: "button",
                    class: "flex min-h-11 flex-1 items-center justify-center gap-2 rounded-lg bg-primary px-3 py-2 text-sm font-medium text-primary-foreground active:bg-primary/85",
                    onclick: {
                        let q = due_open.clone();
                        move |_| {
                            queue.set(q.clone());
                            processing.set(true);
                        }
                    },
                    "Process {due_open.len()} →"
                }
            }
        }
    }
}

/// One row in the review queue. Its own component so each row's action
/// closures capture just that item by value.
#[component]
fn InboxRow(item: InboxItem, slug: Memo<Option<String>>, pending: bool) -> Element {
    let muts = stores::use_inbox_mutations();
    // Hold the item in a Copy `Signal` so each action closure captures
    // only Copy handles and stays cheap to clone into the `on_click`s.
    let item = use_signal(|| item);

    let snap = item.read();
    let open = snap.is_open();
    let body = snap.body.clone();
    let kind = snap.kind.clone();
    let status = snap.status.clone();
    let created = snap.created.clone();
    let date = created.get(..10).unwrap_or(&created).to_string();
    let resurface = snap.resurface_on.clone();
    drop(snap);

    // Flip this item's status (process / archive / reopen): optimistic
    // store patch + write-through.
    let set_status = move |status: &'static str| {
        let Some(s) = slug() else { return };
        let mut next = item();
        next.status = status.to_string();
        muts.save(s, next);
    };

    // Snooze a week out — resurfaces in the daily queue then.
    let snooze = move || {
        let Some(s) = slug() else { return };
        let mut next = item();
        let until = (Utc::now().date_naive() + chrono::Duration::days(7)).to_string();
        next.resurface_on = Some(until);
        muts.save(s, next);
    };

    let delete = move || {
        let Some(s) = slug() else { return };
        muts.delete(s, item().id);
    };

    // Closed items already read as muted; layer pending on top. A
    // failed write rolls back and reports to the notification tray.
    let state_cls = if pending || !open {
        "border-border bg-card/40 opacity-60"
    } else {
        "border-border bg-card/40"
    };

    rsx! {
        div { class: "flex flex-col gap-2 rounded-lg border px-3 py-2.5 sm:flex-row sm:items-start sm:gap-3 sm:py-2 {state_cls}",
            div { class: "flex min-w-0 flex-1 flex-col gap-1",
                Text { class: "whitespace-pre-wrap break-words text-sm", "{body}" }
                div { class: "flex flex-wrap items-center gap-2 text-[11px] text-muted-foreground",
                    span { class: "rounded bg-muted px-1.5 py-px", "{kind}" }
                    span { "{date}" }
                    if !open {
                        span { class: "rounded bg-muted px-1.5 py-px", "{status}" }
                    }
                    if let Some(r) = resurface.as_ref() {
                        span { class: "rounded bg-muted px-1.5 py-px", "💤 {r}" }
                    }
                }
            }
            div { class: "flex shrink-0 items-center gap-1 self-end sm:self-auto",
                if open {
                    Button {
                        variant: ButtonVariant::Secondary,
                        size: ButtonSize::Small,
                        on_click: move |_| set_status(InboxItem::STATUS_PROCESSED),
                        "Process"
                    }
                    Button {
                        variant: ButtonVariant::Ghost,
                        size: ButtonSize::Small,
                        on_click: move |_| snooze(),
                        "Snooze 1w"
                    }
                    Button {
                        variant: ButtonVariant::Ghost,
                        size: ButtonSize::Small,
                        on_click: move |_| set_status(InboxItem::STATUS_ARCHIVED),
                        "Archive"
                    }
                } else {
                    Button {
                        variant: ButtonVariant::Ghost,
                        size: ButtonSize::Small,
                        on_click: move |_| set_status(InboxItem::STATUS_OPEN),
                        "Reopen"
                    }
                    Button {
                        variant: ButtonVariant::Destructive,
                        size: ButtonSize::Small,
                        on_click: move |_| delete(),
                        "Delete"
                    }
                }
            }
        }
    }
}

/// One agent-suggested capture: the proposed text + a one-tap Accept
/// (→ enters the open review queue) or Dismiss (→ deleted).
#[component]
fn SuggestedRow(item: InboxItem, slug: Memo<Option<String>>, pending: bool) -> Element {
    let muts = stores::use_inbox_mutations();
    let item = use_signal(|| item);
    let snap = item.read();
    let body = snap.body.clone();
    let source = snap.source.clone();
    drop(snap);

    let accept = move |_| {
        let Some(s) = slug() else { return };
        let mut next = item();
        next.status = InboxItem::STATUS_OPEN.to_string();
        muts.save(s, next);
    };
    let dismiss = move |_| {
        let Some(s) = slug() else { return };
        muts.delete(s, item().id);
    };

    let state_cls = if pending {
        "border-border bg-card/60 opacity-60"
    } else {
        "border-border bg-card/60"
    };

    rsx! {
        div { class: "flex flex-col gap-2 rounded-lg border px-3 py-2.5 sm:flex-row sm:items-start sm:gap-3 sm:py-2 {state_cls}",
            div { class: "flex min-w-0 flex-1 flex-col gap-0.5",
                Text { class: "whitespace-pre-wrap break-words text-sm", "{body}" }
                span { class: "text-[11px] text-muted-foreground", "via {source}" }
            }
            div { class: "flex shrink-0 items-center gap-1 self-end sm:self-auto",
                Button {
                    variant: ButtonVariant::Primary,
                    size: ButtonSize::Small,
                    on_click: accept,
                    "Accept"
                }
                Button {
                    variant: ButtonVariant::Ghost,
                    size: ButtonSize::Small,
                    on_click: dismiss,
                    "Dismiss"
                }
            }
        }
    }
}

/// Focused daily-review ("process") mode: walk a frozen queue of open
/// items one at a time and decide what each becomes — a Task, an atomic
/// note, a snooze, done, or gone. Mirrors the FLAP processing ritual.
/// Every decision is an optimistic store mutation (rollback + tray
/// notification on failure) and the cursor advances immediately; the
/// queue is a snapshot so it never reshuffles under you.
#[component]
fn ProcessReview(
    items: Vec<InboxItem>,
    slug: Memo<Option<String>>,
    on_exit: EventHandler<()>,
) -> Element {
    let muts = stores::use_inbox_mutations();
    let mut cursor = use_signal(|| 0usize);
    let total = items.len();
    let idx = cursor();

    // One dispatcher for every decision — a Copy `Callback`, so the
    // buttons AND the keyboard both fire it. The item is passed in each
    // call (never a stale capture). Every decision advances the cursor.
    let act = use_callback(move |(item, action): (InboxItem, Act)| {
        let Some(s) = slug() else {
            cursor += 1;
            return;
        };
        match action {
            Act::Task => {
                let (title, details) = split_title_body(&item.body);
                muts.promote_to_task(s, item, title, details);
            }
            Act::Note => {
                let (title, _) = split_title_body(&item.body);
                let path = format!(
                    "Wiki/Atomic/{}-{}.md",
                    slugify(&title),
                    item.id.get(..6).unwrap_or("note")
                );
                let md = atomic_markdown(&title, &item.body, &Utc::now().to_rfc3339());
                muts.promote_to_note(s, item, path, md);
            }
            Act::Done => {
                let mut done = item;
                done.status = InboxItem::STATUS_PROCESSED.to_string();
                muts.save(s, done);
            }
            Act::Delete => muts.delete(s, item.id),
            Act::Skip => {}
            Act::Rate(resp) => {
                // Spaced-repetition reschedule (obsidian-SR SM-2): urgency
                // escalates the interval; the item stays open and resurfaces
                // on its new due date.
                let today = Utc::now().date_naive();
                let (interval, ease, resurface_on, reviews) = review(&item, resp, today);
                let mut next = item;
                next.interval = interval;
                next.ease = ease;
                next.resurface_on = Some(resurface_on);
                next.reviews = reviews;
                muts.save(s, next);
            }
        }
        cursor += 1;
    });

    if idx >= total {
        return rsx! {
            div { class: "mx-auto flex max-w-2xl flex-col items-center gap-4 p-6 pt-[12vh] text-center lg:p-10",
                div { class: "text-5xl", "🎉" }
                Heading { level: HeadingLevel::H2, "Inbox clear" }
                Text { variant: TextVariant::Muted, "You've processed everything in the queue." }
                Button {
                    variant: ButtonVariant::Primary,
                    on_click: move |_| on_exit.call(()),
                    "Back to inbox"
                }
            }
        };
    }

    let item = items[idx].clone();
    let body = item.body.clone();
    let kind = item.kind.clone();
    let source = item.source.clone();
    let date = item.created.get(..10).unwrap_or(&item.created).to_string();
    let reviews = item.reviews;
    let interval = item.interval;
    let pct = ((idx as f32) / (total.max(1) as f32) * 100.0).round() as i32;

    // Preview where each urgency rating would push the item.
    let today = Utc::now().date_naive();
    let iv_urgent = review(&item, ReviewResponse::Hard, today).0;
    let iv_maybe = review(&item, ReviewResponse::Good, today).0;
    let iv_someday = review(&item, ReviewResponse::Easy, today).0;

    rsx! {
        div {
            class: "mx-auto flex max-w-2xl flex-col gap-4 p-4 outline-none sm:p-6 lg:p-10",
            tabindex: "0",
            autofocus: true,
            onkeydown: {
                let item = item.clone();
                move |e: KeyboardEvent| {
                    let a = if let Key::Character(c) = e.key() {
                        match c.as_str() {
                            "1" => Some(Act::Rate(ReviewResponse::Hard)),
                            "2" => Some(Act::Rate(ReviewResponse::Good)),
                            "3" => Some(Act::Rate(ReviewResponse::Easy)),
                            "t" => Some(Act::Task),
                            "n" => Some(Act::Note),
                            "d" => Some(Act::Done),
                            "x" => Some(Act::Delete),
                            " " => Some(Act::Skip),
                            _ => None,
                        }
                    } else {
                        None
                    };
                    if let Some(a) = a {
                        e.prevent_default();
                        act.call((item.clone(), a));
                    }
                }
            },
            // Progress + exit.
            div { class: "flex items-center justify-between",
                Text { variant: TextVariant::Muted, class: "text-sm", "Processing {idx + 1} of {total}" }
                Button {
                    variant: ButtonVariant::Ghost,
                    size: ButtonSize::Small,
                    on_click: move |_| on_exit.call(()),
                    "Exit"
                }
            }
            div { class: "h-1 w-full overflow-hidden rounded-full bg-muted",
                div { class: "h-full rounded-full bg-primary transition-all", style: "width: {pct}%" }
            }

            // The capture, verbatim + its review schedule.
            div { class: "flex flex-col gap-2 rounded-xl border border-border bg-card/40 p-5",
                div { class: "flex flex-wrap items-center gap-2 text-[11px] text-muted-foreground",
                    span { class: "rounded bg-muted px-1.5 py-px", "{kind}" }
                    span { "{date}" }
                    if source != "ui" && source != "cli" {
                        span { class: "rounded bg-muted px-1.5 py-px", "via {source}" }
                    }
                    if reviews > 0 {
                        span { class: "ml-auto", "seen {reviews}\u{d7} · last {fmt_interval(interval)}" }
                    }
                }
                Text { class: "whitespace-pre-wrap break-words text-base", "{body}" }
            }

            // Spaced-repetition triage — urgency escalates the schedule so
            // only the consistently-urgent items keep resurfacing.
            Text { variant: TextVariant::Muted, class: "text-xs", "How urgent is this?  (1 · 2 · 3)" }
            div { class: "flex flex-wrap gap-2",
                {rate_button(act, item.clone(), ReviewResponse::Hard, "Urgent", iv_urgent, ButtonVariant::Primary)}
                {rate_button(act, item.clone(), ReviewResponse::Good, "Maybe", iv_maybe, ButtonVariant::Secondary)}
                {rate_button(act, item.clone(), ReviewResponse::Easy, "Someday", iv_someday, ButtonVariant::Outline)}
            }

            // …or act on it now.
            Text { variant: TextVariant::Muted, class: "text-xs", "…or handle it now" }
            div { class: "flex flex-wrap items-center gap-2",
                Button {
                    variant: ButtonVariant::Ghost,
                    size: ButtonSize::Small,
                    class: "min-h-11 sm:min-h-0",
                    on_click: {
                        let item = item.clone();
                        move |_| act.call((item.clone(), Act::Task))
                    },
                    "→ Task (t)"
                }
                Button {
                    variant: ButtonVariant::Ghost,
                    size: ButtonSize::Small,
                    class: "min-h-11 sm:min-h-0",
                    on_click: {
                        let item = item.clone();
                        move |_| act.call((item.clone(), Act::Note))
                    },
                    "→ Note (n)"
                }
                Button {
                    variant: ButtonVariant::Ghost,
                    size: ButtonSize::Small,
                    class: "min-h-11 sm:min-h-0",
                    on_click: {
                        let item = item.clone();
                        move |_| act.call((item.clone(), Act::Done))
                    },
                    "Done (d)"
                }
                div { class: "flex-1" }
                Button {
                    variant: ButtonVariant::Ghost,
                    size: ButtonSize::Small,
                    class: "min-h-11 sm:min-h-0",
                    on_click: move |_| cursor += 1,
                    "Skip"
                }
                Button {
                    variant: ButtonVariant::Destructive,
                    size: ButtonSize::Small,
                    class: "min-h-11 sm:min-h-0",
                    on_click: {
                        let item = item.clone();
                        move |_| act.call((item.clone(), Act::Delete))
                    },
                    "Delete (x)"
                }
            }
        }
    }
}

/// A decision the review dispatcher can carry out.
#[derive(Clone)]
enum Act {
    Task,
    Note,
    Done,
    Delete,
    Skip,
    Rate(ReviewResponse),
}

/// One urgency-rating button: label over the interval it would schedule.
fn rate_button(
    act: Callback<(InboxItem, Act)>,
    item: InboxItem,
    resp: ReviewResponse,
    label: &str,
    interval: i64,
    variant: ButtonVariant,
) -> Element {
    let label = label.to_string();
    rsx! {
        Button {
            variant,
            class: "min-h-12 flex-1 flex-col gap-0 sm:flex-none",
            on_click: move |_| act.call((item.clone(), Act::Rate(resp))),
            span { "{label}" }
            span { class: "text-[10px] font-normal opacity-70", "{fmt_interval(interval)}" }
        }
    }
}

/// Human-friendly interval label ("today", "1d", "3w", "2mo").
fn fmt_interval(days: i64) -> String {
    if days <= 0 {
        "today".to_string()
    } else if days == 1 {
        "1d".to_string()
    } else if days < 7 {
        format!("{days}d")
    } else if days < 30 {
        format!("{}w", (days as f64 / 7.0).round() as i64)
    } else if days < 365 {
        format!("{}mo", (days as f64 / 30.0).round() as i64)
    } else {
        format!("{:.1}y", days as f64 / 365.0)
    }
}

/// First non-empty line (capped) as the title; the remainder as the
/// body. Used to seed a promoted Task's title + details.
fn split_title_body(body: &str) -> (String, String) {
    let trimmed = body.trim();
    let (first, rest) = trimmed.split_once('\n').unwrap_or((trimmed, ""));
    let title: String = first.trim().chars().take(120).collect();
    let title = if title.is_empty() {
        "Untitled".to_string()
    } else {
        title
    };
    (title, rest.trim().to_string())
}

/// Kebab-case a title into a vault-safe filename stem.
fn slugify(s: &str) -> String {
    let mut out = String::new();
    let mut prev_dash = false;
    for c in s.chars() {
        if c.is_ascii_alphanumeric() {
            out.push(c.to_ascii_lowercase());
            prev_dash = false;
        } else if !prev_dash && !out.is_empty() {
            out.push('-');
            prev_dash = true;
        }
    }
    let capped: String = out.trim_matches('-').chars().take(60).collect();
    let capped = capped.trim_matches('-').to_string();
    if capped.is_empty() {
        "note".to_string()
    } else {
        capped
    }
}

/// Markdown for a promoted atomic note: frontmatter (title / `atomic`
/// type + tag / created) over the verbatim capture as the body.
fn atomic_markdown(title: &str, body: &str, created: &str) -> String {
    let esc = title.replace('"', "'");
    format!(
        "---\ntitle: \"{esc}\"\ntype: atomic\ntags:\n  - atomic\ncreated: {created}\n---\n\n{body}\n"
    )
}
