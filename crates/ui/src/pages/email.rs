//! `/email` — synced-mail reader over the org's `EmailSync` service.
//!
//! First slice: list the mail accounts the org's backend serves, let the
//! user pick one, and show that account's recent `INBOX` envelopes
//! (from / subject / date). Read-only — compose / reply / flag come
//! later. Org-scoped: it reads the first selected org's `EmailSync`
//! backend.
//!
//! The backend is a Maildir-backed `EmailSync` impl; an org with no
//! configured mailbox returns an empty account list, which renders as an
//! empty state rather than an error. Per-account fetch errors surface
//! inline so a misconfigured mailbox doesn't blank the page.

use dioxus::prelude::*;
use email_proto::{Account, Envelope};
use fts_ui::prelude::*;

use crate::orgs::{OrgMeta, OrgSelection};

#[component]
pub fn EmailView() -> Element {
    let selection = use_context::<Signal<OrgSelection>>();
    let org_list = use_context::<Signal<Vec<OrgMeta>>>();

    // The org we read mail from (first selected, or home).
    let slug = use_memo(move || {
        crate::orgs::selected_slugs(&selection.read(), &org_list.read())
            .into_iter()
            .next()
    });

    // Which account is selected (its id). `None` until accounts load;
    // we default to the first account once they arrive.
    let mut selected_account = use_signal(|| None::<String>);

    let accounts = use_resource(move || async move {
        match slug() {
            Some(s) => crate::feeds::fetch_email_accounts(&s).await,
            None => Ok(Vec::new()),
        }
    });

    // Settle on a default account once the list loads (first one).
    let account_list: Vec<Account> = match &*accounts.read() {
        Some(Ok(list)) => list.clone(),
        _ => Vec::new(),
    };
    use_effect(move || {
        if selected_account.peek().is_none() {
            if let Some(Ok(list)) = &*accounts.read() {
                if let Some(first) = list.first() {
                    selected_account.set(Some(first.id.0.clone()));
                }
            }
        }
    });

    let envelopes = use_resource(move || async move {
        match (slug(), selected_account()) {
            (Some(s), Some(acct)) => crate::feeds::fetch_email_envelopes(&s, &acct, 50).await,
            _ => Ok(Vec::new()),
        }
    });

    let accounts_err = match &*accounts.read() {
        Some(Err(e)) => Some(e.clone()),
        _ => None,
    };
    let current = selected_account();
    let (rows, rows_err): (Vec<Envelope>, Option<String>) = match &*envelopes.read() {
        Some(Ok(list)) => (list.clone(), None),
        Some(Err(e)) => (Vec::new(), Some(e.clone())),
        None => (Vec::new(), None),
    };
    let loading = envelopes.read().is_none() && current.is_some();

    rsx! {
        div { class: "mx-auto flex max-w-3xl flex-col gap-5 p-6 lg:p-10",
            Heading { level: HeadingLevel::H1, "Email" }
            Text {
                variant: TextVariant::Muted,
                class: "text-sm -mt-2",
                "Synced mail for the selected org. Pick an account to read its recent inbox.",
            }

            if let Some(err) = accounts_err {
                div { class: "rounded-lg border border-destructive/40 bg-destructive/10 px-3 py-2 text-sm text-destructive",
                    "Couldn't load accounts: {err}"
                }
            }

            // ── Account picker ─────────────────────────────────────
            if account_list.is_empty() {
                div { class: "rounded-lg border border-dashed border-border px-4 py-10 text-center",
                    Text {
                        variant: TextVariant::Muted,
                        "No mail accounts configured for this org yet.",
                    }
                }
            } else {
                div { class: "flex flex-wrap gap-2",
                    for acct in account_list.iter().cloned() {
                        AccountChip {
                            key: "{acct.id.0}",
                            id: acct.id.0.clone(),
                            label: if acct.name.is_empty() { acct.address.clone() } else { acct.name.clone() },
                            selected: current.as_deref() == Some(acct.id.0.as_str()),
                            on_select: move |id: String| selected_account.set(Some(id)),
                        }
                    }
                }
            }

            // ── Recent messages ────────────────────────────────────
            if let Some(err) = rows_err {
                div { class: "rounded-lg border border-destructive/40 bg-destructive/10 px-3 py-2 text-sm text-destructive",
                    "Couldn't load messages: {err}"
                }
            } else if loading {
                Text { variant: TextVariant::Muted, class: "text-sm", "Loading messages…" }
            } else if !account_list.is_empty() && rows.is_empty() {
                div { class: "rounded-lg border border-dashed border-border px-4 py-10 text-center",
                    Text { variant: TextVariant::Muted, "Inbox empty — no recent messages." }
                }
            } else if !rows.is_empty() {
                div { class: "flex flex-col gap-1.5",
                    for env in rows {
                        EnvelopeRow {
                            key: "{env.message_id}",
                            from: sender_label(&env),
                            subject: if env.subject.is_empty() { "(no subject)".to_owned() } else { env.subject.clone() },
                            snippet: env.snippet.clone().filter(|s| !s.is_empty()),
                            date: format_date(env.date_ms),
                            unread: !env.flags.iter().any(|f| f == "\\Seen" || f == "Seen"),
                        }
                    }
                }
            }
        }
    }
}

/// One selectable account chip. Takes primitive props (the proto
/// `Account` doesn't impl `PartialEq`, which Dioxus props require).
#[component]
fn AccountChip(
    id: String,
    label: String,
    selected: bool,
    on_select: EventHandler<String>,
) -> Element {
    let cls = if selected {
        "rounded-full border border-primary bg-primary/10 px-3 py-1 text-sm text-foreground"
    } else {
        "rounded-full border border-border bg-card/40 px-3 py-1 text-sm text-muted-foreground hover:text-foreground"
    };
    rsx! {
        button {
            class: "{cls}",
            onclick: move |_| on_select.call(id.clone()),
            "{label}"
        }
    }
}

/// One message summary row: sender, subject, date. Primitive props for
/// the same reason as [`AccountChip`].
#[component]
fn EnvelopeRow(
    from: String,
    subject: String,
    snippet: Option<String>,
    date: String,
    unread: bool,
) -> Element {
    let weight = if unread { "font-medium" } else { "" };

    rsx! {
        div { class: "flex items-baseline gap-3 rounded-lg border border-border bg-card/40 px-3 py-2",
            span { class: "w-40 shrink-0 truncate text-sm {weight} text-foreground", "{from}" }
            div { class: "flex min-w-0 flex-1 flex-col",
                span { class: "truncate text-sm {weight} text-foreground", "{subject}" }
                if let Some(snippet) = snippet.as_ref().filter(|s| !s.is_empty()) {
                    span { class: "truncate text-xs text-muted-foreground", "{snippet}" }
                }
            }
            span { class: "shrink-0 text-xs text-muted-foreground", "{date}" }
        }
    }
}

/// Display name for an envelope's first sender: their name if present,
/// else their email, else a placeholder.
fn sender_label(env: &Envelope) -> String {
    env.from.first().map_or_else(
        || "(unknown sender)".to_owned(),
        |a| {
            a.name
                .clone()
                .filter(|n| !n.is_empty())
                .unwrap_or_else(|| a.email.clone())
        },
    )
}

/// Format a unix-ms timestamp as a short local date. Falls back to an
/// empty string for the zero/sentinel value so undated envelopes don't
/// render "1970".
fn format_date(date_ms: i64) -> String {
    if date_ms <= 0 {
        return String::new();
    }
    use chrono::TimeZone;
    chrono::Local
        .timestamp_millis_opt(date_ms)
        .single()
        .map(|dt| dt.format("%b %-d").to_string())
        .unwrap_or_default()
}
