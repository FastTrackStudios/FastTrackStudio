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

    rsx! {
        div { class: "mx-auto flex max-w-3xl flex-col gap-5 p-6 lg:p-10",
            div { class: "flex items-baseline justify-between gap-3",
                Heading { level: HeadingLevel::H1, "Inbox" }
                Text { variant: TextVariant::Muted, class: "text-sm", "{open_count} to review" }
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
