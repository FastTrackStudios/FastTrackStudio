//! `/inbox` — the FLAP capture queue + daily review.
//!
//! Capture anything with near-zero friction (the quick-add box), then
//! honour the "temporal contract": read the open items again later and
//! either process them into something durable, snooze them to resurface
//! on a date, or let them go. The list shows `open` items oldest-first;
//! snoozed items stay hidden until their date (toggle "Show all" to see
//! everything, including processed + archived).
//!
//! Processing into a task / atomic wiki note is the next slice; for now
//! "Process" retires the item (marks it `processed`) and records nothing
//! downstream.

use chrono::Utc;
use dioxus::prelude::*;
use fts_ui::prelude::*;
use inbox_proto::InboxItem;

use crate::orgs::{OrgMeta, OrgSelection};

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
    // Bumped after every mutation to re-run the fetch.
    let mut refresh = use_signal(|| 0u32);
    // Focused daily-review ("process") mode + its frozen work queue.
    let mut processing = use_signal(|| false);
    let mut queue = use_signal(Vec::<InboxItem>::new);

    let items = use_resource(move || {
        let _ = refresh(); // subscribe so mutations re-fetch
        async move {
            match slug() {
                Some(s) => crate::feeds::fetch_inbox(&s).await,
                None => Ok(Vec::new()),
            }
        }
    });

    // Capture the current draft as a fresh fleeting note.
    let mut capture = move || {
        let text = draft.read().trim().to_string();
        if text.is_empty() {
            return;
        }
        let Some(s) = slug() else { return };
        draft.set(String::new());
        spawn(async move {
            let id = uuid::Uuid::new_v4().to_string();
            let created = Utc::now().to_rfc3339();
            let item = InboxItem::capture(id, text, "ui", created);
            let _ = crate::feeds::upsert_inbox_item(&s, item).await;
            refresh += 1;
        });
    };

    let today = Utc::now().date_naive().to_string();

    let (rows, load_err): (Vec<InboxItem>, Option<String>) = match &*items.read() {
        Some(Ok(all)) => {
            let show_all = show_all();
            let today = today.clone();
            let visible = all
                .iter()
                .filter(|it| {
                    show_all
                        || (it.is_open()
                            && it
                                .resurface_on
                                .as_deref()
                                .is_none_or(|d| d <= today.as_str()))
                })
                .cloned()
                .collect();
            (visible, None)
        }
        Some(Err(e)) => (Vec::new(), Some(e.clone())),
        None => (Vec::new(), None),
    };

    let open_count = match &*items.read() {
        Some(Ok(all)) => all.iter().filter(|it| it.is_open()).count(),
        _ => 0,
    };

    // The daily-review work set: open items whose snooze has elapsed,
    // oldest first (the fetch already sorts). Frozen into `queue` when
    // the user enters process mode so mutations don't reshuffle it.
    let due_open: Vec<InboxItem> = match &*items.read() {
        Some(Ok(all)) => all
            .iter()
            .filter(|it| {
                it.is_open()
                    && it
                        .resurface_on
                        .as_deref()
                        .is_none_or(|d| d <= today.as_str())
            })
            .cloned()
            .collect(),
        _ => Vec::new(),
    };

    // Focused review mode takes over the whole page.
    if processing() {
        return rsx! {
            ProcessReview {
                items: queue(),
                slug,
                on_exit: move |()| {
                    processing.set(false);
                    refresh += 1;
                },
            }
        };
    }

    rsx! {
        div { class: "mx-auto flex max-w-3xl flex-col gap-5 p-6 lg:p-10",
            div { class: "flex items-center justify-between gap-3",
                Heading { level: HeadingLevel::H1, "Inbox" }
                if !due_open.is_empty() {
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

            if let Some(err) = load_err {
                div { class: "rounded-lg border border-destructive/40 bg-destructive/10 px-3 py-2 text-sm text-destructive",
                    "Couldn't load the inbox: {err}"
                }
            }

            // ── The queue ──────────────────────────────────────────
            if rows.is_empty() {
                div { class: "rounded-lg border border-dashed border-border px-4 py-10 text-center",
                    Text { variant: TextVariant::Muted, "Inbox empty — nothing to review. 🎉" }
                }
            } else {
                div { class: "flex flex-col gap-2",
                    for item in rows {
                        InboxRow { key: "{item.id}", item, slug, refresh }
                    }
                }
            }
        }
    }
}

/// One row in the review queue. Its own component so each row's action
/// closures capture just that item by value.
#[component]
fn InboxRow(item: InboxItem, slug: Memo<Option<String>>, mut refresh: Signal<u32>) -> Element {
    // Hold the item in a Copy `Signal` so each action closure captures
    // only Copy handles (Signal / Memo) and stays cheap to clone into
    // the multiple `on_click`s.
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

    // Mutate this item's status (process / archive / reopen), then refetch.
    let set_status = move |status: &'static str| {
        let Some(s) = slug() else { return };
        let mut next = item();
        next.status = status.to_string();
        spawn(async move {
            let _ = crate::feeds::upsert_inbox_item(&s, next).await;
            refresh += 1;
        });
    };

    // Snooze a week out — resurfaces in the daily queue then.
    let snooze = move || {
        let Some(s) = slug() else { return };
        let mut next = item();
        let until = (Utc::now().date_naive() + chrono::Duration::days(7)).to_string();
        next.resurface_on = Some(until);
        spawn(async move {
            let _ = crate::feeds::upsert_inbox_item(&s, next).await;
            refresh += 1;
        });
    };

    let delete = move || {
        let Some(s) = slug() else { return };
        let id = item().id;
        spawn(async move {
            let _ = crate::feeds::delete_inbox_item(&s, &id).await;
            refresh += 1;
        });
    };

    let dim = if open { "" } else { "opacity-60" };

    rsx! {
        div { class: "flex items-start gap-3 rounded-lg border border-border bg-card/40 px-3 py-2 {dim}",
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
            div { class: "flex shrink-0 items-center gap-1",
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

/// Focused daily-review ("process") mode: walk a frozen queue of open
/// items one at a time and decide what each becomes — a Task, an atomic
/// note, a snooze, done, or gone. Mirrors the FLAP processing ritual.
/// Mutations fire optimistically and the cursor advances immediately;
/// the queue is a snapshot so it never reshuffles under you.
#[component]
fn ProcessReview(
    items: Vec<InboxItem>,
    slug: Memo<Option<String>>,
    on_exit: EventHandler<()>,
) -> Element {
    let mut cursor = use_signal(|| 0usize);
    let total = items.len();
    let idx = cursor();

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
    let (title, details) = split_title_body(&body);
    let kind = item.kind.clone();
    let source = item.source.clone();
    let date = item.created.get(..10).unwrap_or(&item.created).to_string();
    let pct = ((idx as f32) / (total.max(1) as f32) * 100.0).round() as i32;

    // ── action closures (each fires its mutation, then advances) ──
    let to_task = {
        let item = item.clone();
        let title = title.clone();
        let details = details.clone();
        move |_| {
            if let Some(s) = slug() {
                let (item, title, details) = (item.clone(), title.clone(), details.clone());
                spawn(async move {
                    if let Ok(t) = crate::feeds::create_task(&s, &title, &details).await {
                        let mut done = item;
                        done.status = InboxItem::STATUS_PROCESSED.to_string();
                        done.processed_into = Some(t.path);
                        let _ = crate::feeds::upsert_inbox_item(&s, done).await;
                    }
                });
            }
            cursor += 1;
        }
    };

    let to_note = {
        let item = item.clone();
        let title = title.clone();
        let body = body.clone();
        move |_| {
            if let Some(s) = slug() {
                let (item, title, body) = (item.clone(), title.clone(), body.clone());
                let path = format!(
                    "Wiki/Atomic/{}-{}.md",
                    slugify(&title),
                    item.id.get(..6).unwrap_or("note")
                );
                let md = atomic_markdown(&title, &body, &Utc::now().to_rfc3339());
                spawn(async move {
                    if crate::feeds::create_wiki_note(&s, &path, &md).await.is_ok() {
                        let mut done = item;
                        done.status = InboxItem::STATUS_PROCESSED.to_string();
                        done.processed_into = Some(path);
                        let _ = crate::feeds::upsert_inbox_item(&s, done).await;
                    }
                });
            }
            cursor += 1;
        }
    };

    let mark_done = {
        let item = item.clone();
        move |_| {
            if let Some(s) = slug() {
                let mut done = item.clone();
                spawn(async move {
                    done.status = InboxItem::STATUS_PROCESSED.to_string();
                    let _ = crate::feeds::upsert_inbox_item(&s, done).await;
                });
            }
            cursor += 1;
        }
    };

    let delete = {
        let id = item.id.clone();
        move |_| {
            if let Some(s) = slug() {
                let id = id.clone();
                spawn(async move {
                    let _ = crate::feeds::delete_inbox_item(&s, &id).await;
                });
            }
            cursor += 1;
        }
    };

    // Snooze the current item `days` out, then advance. Inlined per
    // button (each needs its own clone of the item).
    let snooze_btn =
        |item: InboxItem, slug: Memo<Option<String>>, mut cursor: Signal<usize>, days: i64| {
            if let Some(s) = slug() {
                let mut next = item;
                let until = (Utc::now().date_naive() + chrono::Duration::days(days)).to_string();
                spawn(async move {
                    next.resurface_on = Some(until);
                    let _ = crate::feeds::upsert_inbox_item(&s, next).await;
                });
            }
            cursor += 1;
        };

    rsx! {
        div { class: "mx-auto flex max-w-2xl flex-col gap-4 p-6 lg:p-10",
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

            // The capture, verbatim.
            div { class: "flex flex-col gap-2 rounded-xl border border-border bg-card/40 p-5",
                div { class: "flex flex-wrap items-center gap-2 text-[11px] text-muted-foreground",
                    span { class: "rounded bg-muted px-1.5 py-px", "{kind}" }
                    span { "{date}" }
                    if source != "ui" && source != "cli" {
                        span { class: "rounded bg-muted px-1.5 py-px", "via {source}" }
                    }
                }
                Text { class: "whitespace-pre-wrap break-words text-base", "{body}" }
            }

            // Decisions.
            Text { variant: TextVariant::Muted, class: "text-xs", "What should this become?" }
            div { class: "flex flex-wrap gap-2",
                Button { variant: ButtonVariant::Primary, on_click: to_task, "→ Task" }
                Button { variant: ButtonVariant::Secondary, on_click: to_note, "→ Note" }
                Button { variant: ButtonVariant::Outline, on_click: mark_done, "Done" }
            }
            div { class: "flex flex-wrap items-center gap-2",
                Text { variant: TextVariant::Muted, class: "text-xs", "Snooze:" }
                Button {
                    variant: ButtonVariant::Ghost,
                    size: ButtonSize::Small,
                    on_click: {
                        let item = item.clone();
                        move |_| snooze_btn(item.clone(), slug, cursor, 1)
                    },
                    "Tomorrow"
                }
                Button {
                    variant: ButtonVariant::Ghost,
                    size: ButtonSize::Small,
                    on_click: {
                        let item = item.clone();
                        move |_| snooze_btn(item.clone(), slug, cursor, 3)
                    },
                    "3 days"
                }
                Button {
                    variant: ButtonVariant::Ghost,
                    size: ButtonSize::Small,
                    on_click: {
                        let item = item.clone();
                        move |_| snooze_btn(item.clone(), slug, cursor, 7)
                    },
                    "1 week"
                }
                div { class: "flex-1" }
                Button {
                    variant: ButtonVariant::Ghost,
                    size: ButtonSize::Small,
                    on_click: move |_| cursor += 1,
                    "Skip"
                }
                Button {
                    variant: ButtonVariant::Destructive,
                    size: ButtonSize::Small,
                    on_click: delete,
                    "Delete"
                }
            }
        }
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
