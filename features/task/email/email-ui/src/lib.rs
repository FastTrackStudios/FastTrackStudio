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

use task_ui_core::feeds;
use task_ui_core::orgs::{OrgMeta, OrgSelection};

#[component]
pub fn EmailView() -> Element {
    let selection = use_context::<Signal<OrgSelection>>();
    let org_list = use_context::<Signal<Vec<OrgMeta>>>();

    // The org we read mail from (first selected, or home).
    let slug = use_memo(move || {
        task_ui_core::orgs::selected_slugs(&selection.read(), &org_list.read())
            .into_iter()
            .next()
    });

    // Which account is selected (its id). `None` until accounts load;
    // we default to the first account once they arrive.
    let mut selected_account = use_signal(|| None::<String>);

    let accounts = use_resource(move || async move {
        match slug() {
            Some(s) => fetch_email_accounts(&s).await,
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
            (Some(s), Some(acct)) => fetch_email_envelopes(&s, &acct, 50).await,
            _ => Ok(Vec::new()),
        }
    });

    // ── Live mailbox changes ──────────────────────────────────
    // `EmailSync`'s `#[subscribe]` stream. Mail is the archetypal
    // push surface — new mail should land in the list, not wait for
    // a revisit. Events name what changed, not the new value (an
    // `Envelope` is a mailbox read, and IMAP's IDLE only reports
    // *that* something changed), so a hit for the selected account
    // re-lists.
    //
    // How live this actually is depends on the mounted backend:
    // `email-imap` publishes from its IDLE loop; the maildir
    // backend the server mounts today has no watcher and no
    // supported mutations, so its stream is silent until one lands.
    architect::use_stream(
        move |tx| {
            let slug = slug();
            async move {
                let Some(slug) = slug else {
                    return false;
                };
                let Ok(client) = task_ui_core::vox_clients::establish_for::<
                    email_proto::EmailSyncStreamClient,
                >(&slug)
                .await
                else {
                    return false;
                };
                client.changes(tx).await.is_ok()
            }
        },
        move |change: email_proto::EmailChange| {
            let mut envelopes = envelopes;
            // One stream carries every account the backend serves —
            // keep the mailbox on screen.
            if selected_account.peek().as_deref() != Some(change.account.as_str()) {
                return;
            }
            envelopes.restart();
        },
    );

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
        div { class: "mx-auto flex max-w-3xl flex-col gap-5 p-4 sm:p-6 lg:p-10",
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

// ── data ────────────────────────────────────────────────────────────
//
// This slice's RPCs live with the page that calls them, not in the
// shell's `feeds` module — that is the point of the split. `feeds!` and
// the fan-out helpers come from `task-ui-core`; see its `feeds` module
// for the shape.

feeds! {
    email_proto::EmailSyncClient {
        /// Every mail account the org's `EmailSync` backend serves. An org
        /// with no configured mailbox returns an empty list (operational but
        /// unconfigured) — the `/email` page renders that as an empty state
        /// rather than an error.
        fetch_email_accounts() -> Vec<email_proto::Account>
            = accounts() as "list accounts";
    }
}

/// Recent envelopes (header summaries) for one account's `INBOX`,
/// newest first. `count` caps the slice. Returns an empty list for an
/// empty mailbox; surfaces backend errors verbatim so the page can show
/// them inline.
pub async fn fetch_email_envelopes(
    slug: &str,
    account: &str,
    count: u32,
) -> Result<Vec<email_proto::Envelope>, String> {
    let client = task_ui_core::vox_clients::establish_for::<email_proto::EmailSyncClient>(slug).await?;
    let mut envelopes = client
        .fetch_envelopes(
            account.to_owned(),
            "INBOX".to_owned(),
            email_proto::SeqRange::Recent(count),
        )
        .await
        .map_err(|e| format!("{slug}: fetch envelopes: {e:?}"))?;
    // Newest first — the backend's `Recent` ordering isn't guaranteed
    // across implementations, so sort defensively on the date.
    envelopes.sort_by(|a, b| b.date_ms.cmp(&a.date_ms));
    Ok(envelopes)
}
