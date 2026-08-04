//! `/email` — mail over the org's `EmailSync` + `EmailProduct`
//! services.
//!
//! v1 product surface: account chips, the account's recent
//! `INBOX` envelopes, a minimal compose (new message + reply)
//! that *stages* drafts into the outbox, and the outbox panel
//! where staged mail is approved or cancelled — the
//! human-in-the-loop gate. Everything re-reads on the one
//! `EmailChange` stream (changes-only contract): mailbox events
//! re-list envelopes, `OutboxChanged` re-lists the outbox.
//!
//! The backend is a Maildir-backed `EmailSync` impl; an org with
//! no configured mailbox returns an empty account list, which
//! renders as an empty state rather than an error. Sending
//! requires the account's `account.json` to configure an SMTP
//! `submit` endpoint — a staged entry on an account without one
//! surfaces the delivery error in the outbox panel.

use dioxus::prelude::*;
use email_proto::{Account, Addr, Draft, Envelope, OutboxEntry, OutboxStatus};
use fts_ui::prelude::*;

use task_ui_core::feeds;
use task_ui_core::orgs::{OrgMeta, OrgSelection};

/// What the compose form opens with. `None` reply fields = a
/// fresh message.
#[derive(Clone, PartialEq)]
struct ComposeSeed {
    to: String,
    subject: String,
    in_reply_to: Option<String>,
}

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
    // Open compose form, if any.
    let mut composing = use_signal(|| None::<ComposeSeed>);

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

    // Outbox entries for the selected account, newest first.
    let mut outbox = use_resource(move || async move {
        match (slug(), selected_account()) {
            (Some(s), Some(acct)) => fetch_email_outbox(&s, &acct).await,
            _ => Ok(Vec::new()),
        }
    });

    // Triage derivations (urgency / tags) for the listed
    // envelopes. Reads the envelopes resource, so it re-fetches
    // whenever the list does; `DerivationsUpdated` restarts it
    // directly.
    let mut derivs = use_resource(move || async move {
        let ids: Vec<String> = match &*envelopes.read() {
            Some(Ok(list)) => list.iter().map(|e| e.message_id.clone()).collect(),
            _ => Vec::new(),
        };
        match (slug(), selected_account()) {
            (Some(s), Some(acct)) if !ids.is_empty() => {
                fetch_email_derivations(&s, &acct, ids).await
            }
            _ => Ok(Vec::new()),
        }
    });

    // ── Live changes ──────────────────────────────────────────
    // One `EmailChange` stream carries mailbox AND outbox events
    // (shared hub server-side). Events name what changed, not the
    // new value — a hit for the selected account re-reads the
    // touched list.
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
            let mut outbox = outbox;
            let mut derivs = derivs;
            if selected_account.peek().as_deref() != Some(change.account.as_str()) {
                return;
            }
            match change.event {
                email_proto::EmailEvent::OutboxChanged { .. } => outbox.restart(),
                email_proto::EmailEvent::DerivationsUpdated { .. } => derivs.restart(),
                _ => envelopes.restart(),
            }
        },
    );

    let accounts_err = match &*accounts.read() {
        Some(Err(e)) => Some(e.clone()),
        _ => None,
    };
    let current = selected_account();
    let current_address = account_list
        .iter()
        .find(|a| Some(a.id.0.as_str()) == current.as_deref())
        .map(|a| a.address.clone());
    let (rows, rows_err): (Vec<Envelope>, Option<String>) = match &*envelopes.read() {
        Some(Ok(list)) => (list.clone(), None),
        Some(Err(e)) => (Vec::new(), Some(e.clone())),
        None => (Vec::new(), None),
    };
    let outbox_rows: Vec<OutboxEntry> = match &*outbox.read() {
        Some(Ok(list)) => list.clone(),
        _ => Vec::new(),
    };
    // message_id → (urgency, tags) from the derivation cache.
    let deriv_map: std::collections::HashMap<String, (Option<u8>, Vec<String>)> = {
        let mut map = std::collections::HashMap::new();
        if let Some(Ok(rows)) = &*derivs.read() {
            for d in rows {
                let entry = map
                    .entry(d.message_id.clone())
                    .or_insert((None, Vec::new()));
                match d.kind {
                    email_proto::DerivationKind::Urgency => entry.0 = d.urgency(),
                    email_proto::DerivationKind::Tags => {
                        entry.1 = d.tags().into_iter().map(str::to_string).collect();
                    }
                }
            }
        }
        map
    };
    let loading = envelopes.read().is_none() && current.is_some();

    rsx! {
        div { class: "mx-auto flex max-w-3xl flex-col gap-5 p-4 sm:p-6 lg:p-10",
            div { class: "flex items-baseline justify-between gap-3",
                Heading { level: HeadingLevel::H1, "Email" }
                if current.is_some() {
                    Button {
                        size: ButtonSize::Small,
                        on_click: move |_| {
                            composing
                                .set(
                                    Some(ComposeSeed {
                                        to: String::new(),
                                        subject: String::new(),
                                        in_reply_to: None,
                                    }),
                                )
                        },
                        "New message"
                    }
                }
            }
            Text {
                variant: TextVariant::Muted,
                class: "text-sm -mt-2",
                "Synced mail for the selected org. Compose stages into the outbox; approval sends.",
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

            // ── Compose ────────────────────────────────────────────
            if let (Some(seed), Some(slug_now), Some(acct), Some(from)) = (
                composing(),
                slug(),
                current.clone(),
                current_address.clone(),
            ) {
                ComposeForm {
                    // Keyed on the seed so switching between "new"
                    // and a specific reply remounts the form (its
                    // field signals initialize from the seed).
                    key: "{seed.in_reply_to:?}|{seed.to}",
                    slug: slug_now,
                    account: acct,
                    from,
                    seed_to: seed.to.clone(),
                    seed_subject: seed.subject.clone(),
                    in_reply_to: seed.in_reply_to.clone(),
                    on_done: move |_| composing.set(None),
                }
            }

            // ── Outbox ─────────────────────────────────────────────
            if !outbox_rows.is_empty() {
                OutboxPanel {
                    slug: slug().unwrap_or_default(),
                    account: current.clone().unwrap_or_default(),
                    entries: outbox_rows,
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
                            urgency: deriv_map.get(&env.message_id).and_then(|(u, _)| *u),
                            tags: deriv_map.get(&env.message_id).map(|(_, t)| t.clone()).unwrap_or_default(),
                            on_reply: {
                                let sender = env.from.first().map(|a| a.email.clone()).unwrap_or_default();
                                let subject = reply_subject(&env.subject);
                                let message_id = env.message_id.clone();
                                move |_| {
                                    composing
                                        .set(
                                            Some(ComposeSeed {
                                                to: sender.clone(),
                                                subject: subject.clone(),
                                                in_reply_to: Some(message_id.clone()),
                                            }),
                                        )
                                }
                            },
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

/// One message summary row: sender, subject, date, triage chips,
/// reply action. Primitive props for the same reason as
/// [`AccountChip`].
#[component]
#[allow(clippy::too_many_arguments)]
fn EnvelopeRow(
    from: String,
    subject: String,
    snippet: Option<String>,
    date: String,
    unread: bool,
    urgency: Option<u8>,
    tags: Vec<String>,
    on_reply: EventHandler<()>,
) -> Element {
    let weight = if unread { "font-medium" } else { "" };
    // Chips stay quiet for the boring cases: urgency 0 and the
    // `other` tag render nothing.
    let urgency_chip = urgency.filter(|u| *u > 0).map(|u| {
        let cls = match u {
            1 => "border-border text-muted-foreground",
            2 => "border-amber-500/50 text-amber-600 dark:text-amber-400",
            _ => "border-destructive/60 text-destructive",
        };
        (format!("!{u}"), cls)
    });
    let shown_tags: Vec<String> = tags.into_iter().filter(|t| t != "other").collect();

    rsx! {
        div { class: "group flex items-baseline gap-3 rounded-lg border border-border bg-card/40 px-3 py-2",
            span { class: "w-40 shrink-0 truncate text-sm {weight} text-foreground", "{from}" }
            div { class: "flex min-w-0 flex-1 flex-col",
                div { class: "flex min-w-0 items-baseline gap-1.5",
                    if let Some((label, cls)) = urgency_chip {
                        span { class: "shrink-0 rounded-full border px-1.5 text-[10px] font-semibold {cls}",
                            "{label}"
                        }
                    }
                    span { class: "truncate text-sm {weight} text-foreground", "{subject}" }
                    for tag in shown_tags {
                        span {
                            key: "{tag}",
                            class: "shrink-0 rounded-full border border-border px-1.5 text-[10px] text-muted-foreground",
                            "{tag}"
                        }
                    }
                }
                if let Some(snippet) = snippet.as_ref().filter(|s| !s.is_empty()) {
                    span { class: "truncate text-xs text-muted-foreground", "{snippet}" }
                }
            }
            button {
                class: "shrink-0 text-xs text-muted-foreground opacity-0 transition-opacity hover:text-foreground group-hover:opacity-100",
                onclick: move |_| on_reply.call(()),
                "Reply"
            }
            span { class: "shrink-0 text-xs text-muted-foreground", "{date}" }
        }
    }
}

/// Minimal compose: to / subject / body. "Send" stages the draft
/// AND approves it in one go (the poller delivers moments later);
/// "Stage" leaves it pending in the outbox for review — the shape
/// agent-drafted mail always takes.
#[component]
fn ComposeForm(
    slug: String,
    account: String,
    from: String,
    seed_to: String,
    seed_subject: String,
    in_reply_to: Option<String>,
    on_done: EventHandler<()>,
) -> Element {
    let to = use_signal(|| seed_to.clone());
    let subject = use_signal(|| seed_subject.clone());
    let body = use_signal(String::new);
    let mut busy = use_signal(|| false);
    let mut error = use_signal(|| None::<String>);

    let title = if in_reply_to.is_some() { "Reply" } else { "New message" };

    let submit = move |approve_now: bool| {
        let slug = slug.clone();
        let account = account.clone();
        let from = from.clone();
        let in_reply_to = in_reply_to.clone();
        spawn(async move {
            let recipients = parse_addr_list(&to.peek());
            if recipients.is_empty() {
                error.set(Some("Add at least one recipient.".into()));
                return;
            }
            busy.set(true);
            error.set(None);
            let draft = Draft {
                from: Addr {
                    name: None,
                    email: from,
                },
                to: recipients,
                cc: vec![],
                bcc: vec![],
                subject: subject.peek().clone(),
                body_text: body.peek().clone(),
                body_html: None,
                in_reply_to: in_reply_to.clone(),
                references: in_reply_to.clone().into_iter().collect(),
                attachments: vec![],
            };
            let result = stage_email_draft(&slug, &account, draft, approve_now).await;
            busy.set(false);
            match result {
                Ok(()) => on_done.call(()),
                Err(e) => error.set(Some(e)),
            }
        });
    };

    rsx! {
        div { class: "flex flex-col gap-2 rounded-lg border border-border bg-card/60 p-3",
            div { class: "flex items-center justify-between",
                Text { class: "text-sm font-medium", "{title}" }
                button {
                    class: "text-xs text-muted-foreground hover:text-foreground",
                    onclick: move |_| on_done.call(()),
                    "Close"
                }
            }
            Input { value: to, placeholder: "To (comma-separated)" }
            Input { value: subject, placeholder: "Subject" }
            Textarea { value: body, placeholder: "Write…", rows: 6 }
            if let Some(err) = error() {
                div { class: "rounded-md border border-destructive/40 bg-destructive/10 px-2 py-1 text-xs text-destructive",
                    "{err}"
                }
            }
            div { class: "flex items-center justify-end gap-2",
                Button {
                    variant: ButtonVariant::Outline,
                    size: ButtonSize::Small,
                    disabled: busy(),
                    on_click: {
                        let mut submit = submit.clone();
                        move |_| submit(false)
                    },
                    "Stage for approval"
                }
                Button {
                    size: ButtonSize::Small,
                    disabled: busy(),
                    on_click: {
                        let mut submit = submit.clone();
                        move |_| submit(true)
                    },
                    "Send"
                }
            }
        }
    }
}

/// The outbox: staged sends with their status and the
/// approve / cancel gates. Rendered whenever the account has
/// entries (terminal ones included, so outcomes stay visible).
#[component]
fn OutboxPanel(slug: String, account: String, entries: Vec<OutboxEntry>) -> Element {
    rsx! {
        div { class: "flex flex-col gap-1.5",
            SectionHeader { label: "Outbox".to_string() }
            for entry in entries {
                OutboxRow {
                    key: "{entry.id}",
                    slug: slug.clone(),
                    account: account.clone(),
                    id: entry.id,
                    status: entry.status,
                    subject: if entry.draft.subject.is_empty() { "(no subject)".to_owned() } else { entry.draft.subject.clone() },
                    to: entry.draft.to.iter().map(|a| a.email.clone()).collect::<Vec<_>>().join(", "),
                    origin: entry.origin.clone(),
                    error: entry.last_error.clone(),
                    retries: entry.retries,
                }
            }
        }
    }
}

/// One outbox entry. The approve / cancel buttons show only in
/// the states where the transition is legal; the stream's
/// `OutboxChanged` events keep the row fresh.
#[component]
#[allow(clippy::too_many_arguments)]
fn OutboxRow(
    slug: String,
    account: String,
    id: u64,
    status: OutboxStatus,
    subject: String,
    to: String,
    origin: String,
    error: Option<String>,
    retries: u32,
) -> Element {
    let mut busy = use_signal(|| false);
    let (badge, badge_variant) = status_badge(status);
    let approvable = matches!(
        status,
        OutboxStatus::PendingApproval | OutboxStatus::Failed
    );
    let cancellable = matches!(
        status,
        OutboxStatus::Draft
            | OutboxStatus::PendingApproval
            | OutboxStatus::Approved
            | OutboxStatus::Failed
    );
    let from_agent = origin != "user";

    let act = move |approve: bool| {
        let slug = slug.clone();
        let account = account.clone();
        spawn(async move {
            busy.set(true);
            // Errors surface via the refreshed list (the row's
            // status simply won't change); keep the panel dumb.
            let _ = outbox_action(&slug, &account, id, approve).await;
            busy.set(false);
        });
    };

    rsx! {
        div { class: "flex items-baseline gap-3 rounded-lg border border-border bg-card/40 px-3 py-2",
            Badge { variant: badge_variant, "{badge}" }
            div { class: "flex min-w-0 flex-1 flex-col",
                span { class: "truncate text-sm text-foreground", "{subject}" }
                span { class: "truncate text-xs text-muted-foreground",
                    "to {to}"
                    if from_agent {
                        " · staged by {origin}"
                    }
                    if retries > 0 {
                        " · {retries} attempts"
                    }
                }
                if let Some(err) = error.as_ref() {
                    span { class: "truncate text-xs text-destructive", "{err}" }
                }
            }
            if approvable {
                Button {
                    size: ButtonSize::Small,
                    disabled: busy(),
                    on_click: {
                        let mut act = act.clone();
                        move |_| act(true)
                    },
                    if status == OutboxStatus::Failed { "Retry" } else { "Approve" }
                }
            }
            if cancellable {
                Button {
                    variant: ButtonVariant::Ghost,
                    size: ButtonSize::Small,
                    disabled: busy(),
                    on_click: {
                        let mut act = act.clone();
                        move |_| act(false)
                    },
                    "Cancel"
                }
            }
        }
    }
}

fn status_badge(status: OutboxStatus) -> (&'static str, BadgeVariant) {
    match status {
        OutboxStatus::Draft => ("draft", BadgeVariant::Outline),
        OutboxStatus::PendingApproval => ("pending", BadgeVariant::Secondary),
        OutboxStatus::Approved => ("approved", BadgeVariant::Default),
        OutboxStatus::Sending => ("sending", BadgeVariant::Default),
        OutboxStatus::Sent => ("sent", BadgeVariant::Outline),
        OutboxStatus::Failed => ("failed", BadgeVariant::Destructive),
        OutboxStatus::Cancelled => ("cancelled", BadgeVariant::Outline),
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

/// `Re:`-prefix a subject exactly once.
fn reply_subject(subject: &str) -> String {
    if subject.trim_start().to_ascii_lowercase().starts_with("re:") {
        subject.to_string()
    } else {
        format!("Re: {subject}")
    }
}

/// Comma/space-separated address list → `Addr`s (bare emails,
/// display names come later).
fn parse_addr_list(raw: &str) -> Vec<Addr> {
    raw.split([',', ';'])
        .map(str::trim)
        .filter(|s| !s.is_empty())
        .map(|email| Addr {
            name: None,
            email: email.to_string(),
        })
        .collect()
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

/// Cached triage derivations (urgency / tags) for the given
/// message-ids. Messages the background pass hasn't reached yet
/// simply have no rows.
pub async fn fetch_email_derivations(
    slug: &str,
    account: &str,
    ids: Vec<String>,
) -> Result<Vec<email_proto::Derivation>, String> {
    let client =
        task_ui_core::vox_clients::establish_for::<email_proto::EmailProductClient>(slug).await?;
    client
        .derivations(account.to_owned(), ids)
        .await
        .map_err(|e| format!("{slug}: derivations: {e:?}"))
}

/// The account's outbox, newest first (terminal entries included).
pub async fn fetch_email_outbox(slug: &str, account: &str) -> Result<Vec<OutboxEntry>, String> {
    let client =
        task_ui_core::vox_clients::establish_for::<email_proto::EmailProductClient>(slug).await?;
    client
        .list_outbox(account.to_owned())
        .await
        .map_err(|e| format!("{slug}: list outbox: {e:?}"))
}

/// Stage a draft into the outbox; when `approve_now`, immediately
/// approve it too (the user pressing "Send" is the approval).
pub async fn stage_email_draft(
    slug: &str,
    account: &str,
    draft: Draft,
    approve_now: bool,
) -> Result<(), String> {
    let client =
        task_ui_core::vox_clients::establish_for::<email_proto::EmailProductClient>(slug).await?;
    let entry = client
        .submit_draft(account.to_owned(), draft, "user".to_owned())
        .await
        .map_err(|e| format!("{slug}: stage draft: {e:?}"))?;
    if approve_now {
        client
            .approve(account.to_owned(), entry.id)
            .await
            .map_err(|e| format!("{slug}: approve: {e:?}"))?;
    }
    Ok(())
}

/// Approve (`true`) or cancel (`false`) one outbox entry.
pub async fn outbox_action(
    slug: &str,
    account: &str,
    id: u64,
    approve: bool,
) -> Result<(), String> {
    let client =
        task_ui_core::vox_clients::establish_for::<email_proto::EmailProductClient>(slug).await?;
    if approve {
        client
            .approve(account.to_owned(), id)
            .await
            .map_err(|e| format!("{slug}: approve: {e:?}"))?;
    } else {
        client
            .cancel(account.to_owned(), id)
            .await
            .map_err(|e| format!("{slug}: cancel: {e:?}"))?;
    }
    Ok(())
}
