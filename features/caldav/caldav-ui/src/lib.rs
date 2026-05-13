//! CalDAV feature UI. Dumb components — accounts list, add-account
//! form. The sync trigger lives in the parent route component in
//! `task-ui` since it needs the `CalDavService` client.

use caldav_proto::{CalDavAccount, CalDavAccountCreate};
use chrono::Utc;
use dioxus::prelude::*;
use fts_ui::lucide_dioxus::{Cloud, CloudOff, Plus, RefreshCw, ShieldAlert, ShieldCheck, Trash2};
use fts_ui::prelude::*;
use uuid::Uuid;

/// Purpose-built CalDAV sync dashboard — connection health stats,
/// add-account form, list of accounts with per-row sync trigger.
#[component]
pub fn CalDavAccountDashboard(
    items: Vec<CalDavAccount>,
    status: String,
    on_create: EventHandler<CalDavAccountCreate>,
    on_delete: EventHandler<Uuid>,
    on_sync: EventHandler<Uuid>,
) -> Element {
    let total = items.len();
    let with_errors = items.iter().filter(|a| a.last_error.is_some()).count();
    let healthy = total.saturating_sub(with_errors);
    let now = Utc::now();
    let synced_today = items
        .iter()
        .filter(|a| {
            a.last_sync_at
                .map(|t| (now - t).num_hours() < 24)
                .unwrap_or(false)
        })
        .count();
    let never_synced = items.iter().filter(|a| a.last_sync_at.is_none()).count();

    rsx! {
        VStack { class: "gap-6",
            SectionHeader {
                label: "CalDAV sync".to_string(),
                trailing: rsx! {
                    StatusBadge {
                        variant: StatusBadgeVariant::Neutral,
                        label: status.clone(),
                    }
                },
            }
            HStack { class: "gap-2 items-center",
                Cloud { size: 22 }
                Heading { level: HeadingLevel::H2, "Connected calendars" }
            }
            Text { variant: TextVariant::Muted, "Two-way sync with Nextcloud, iCloud, Fastmail, and any standards-compliant CalDAV server." }

            div { class: "grid grid-cols-2 md:grid-cols-4 gap-3",
                Card { class: "border-l-4 border-l-cyan-500",
                    CardHeader {
                        CardDescription { "Accounts" }
                        CardTitle { class: "text-2xl", "{total}" }
                    }
                    CardContent { class: "flex items-center gap-2 text-cyan-500",
                        Cloud { size: 16 }
                        Text { variant: TextVariant::Muted, "connected" }
                    }
                }
                Card { class: "border-l-4 border-l-emerald-500",
                    CardHeader {
                        CardDescription { "Healthy" }
                        CardTitle { class: "text-2xl", "{healthy}" }
                    }
                    CardContent { class: "flex items-center gap-2 text-emerald-500",
                        ShieldCheck { size: 16 }
                        Text { variant: TextVariant::Muted, "no errors" }
                    }
                }
                Card { class: "border-l-4 border-l-sky-500",
                    CardHeader {
                        CardDescription { "Synced 24h" }
                        CardTitle { class: "text-2xl", "{synced_today}" }
                    }
                    CardContent { class: "flex items-center gap-2 text-sky-500",
                        RefreshCw { size: 16 }
                        Text { variant: TextVariant::Muted, "{never_synced} never" }
                    }
                }
                Card { class: "border-l-4 border-l-rose-500",
                    CardHeader {
                        CardDescription { "Errors" }
                        CardTitle { class: "text-2xl", "{with_errors}" }
                    }
                    CardContent { class: "flex items-center gap-2 text-rose-500",
                        ShieldAlert { size: 16 }
                        Text { variant: TextVariant::Muted, "need attention" }
                    }
                }
            }

            Divider {}

            CalDavAccountCreateForm { on_submit: move |p| on_create.call(p) }

            Heading { level: HeadingLevel::H3, "Accounts" }
            if items.is_empty() {
                EmptyState {
                    message: "No CalDAV accounts yet. Connect to Nextcloud, iCloud, or another CalDAV server above.",
                    icon: rsx! { CloudOff { size: 32 } },
                }
            } else {
                div { class: "flex flex-col gap-2",
                    for account in items.iter().cloned() {
                        CalDavAccountSyncRow {
                            key: "{account.id}",
                            account: account.clone(),
                            on_delete: move |id| on_delete.call(id),
                            on_sync: move |id| on_sync.call(id),
                        }
                    }
                }
            }
        }
    }
}

#[component]
fn CalDavAccountSyncRow(
    account: CalDavAccount,
    on_delete: EventHandler<Uuid>,
    on_sync: EventHandler<Uuid>,
) -> Element {
    let id = account.id;
    let kind = account.kind.clone().unwrap_or_else(|| "caldav".into());
    let last_sync = account
        .last_sync_at
        .map(|t| t.format("%Y-%m-%d %H:%M").to_string())
        .unwrap_or_else(|| "never".into());
    let meta = format!("{kind} · {} · last sync: {last_sync}", account.username);
    let has_error = account.last_error.is_some();
    let err_text = account.last_error.clone().unwrap_or_default();
    rsx! {
        Item {
            ItemContent {
                ItemTitle { "{account.display_name}" }
                ItemDescription { "{meta}" }
                if has_error {
                    ItemDescription { class: "text-destructive", "⚠ {err_text}" }
                }
            }
            ItemActions { class: "gap-2",
                if has_error {
                    StatusBadge { variant: StatusBadgeVariant::Danger, label: "Error" }
                } else {
                    Badge { variant: BadgeVariant::Secondary, "{kind}" }
                }
                Button {
                    variant: ButtonVariant::Outline,
                    size: ButtonSize::Small,
                    on_click: move |_| on_sync.call(id),
                    RefreshCw { size: 14 }
                    " Sync"
                }
                Button {
                    variant: ButtonVariant::Ghost,
                    size: ButtonSize::Small,
                    on_click: move |_| on_delete.call(id),
                    Trash2 { size: 14 }
                }
            }
        }
    }
}

#[component]
pub fn CalDavAccountList(items: Vec<CalDavAccount>, on_delete: EventHandler<Uuid>) -> Element {
    if items.is_empty() {
        return rsx! {
            EmptyState {
                message: "No CalDAV accounts yet. Add one above to connect to Nextcloud, iCloud, or another CalDAV server.",
                icon: rsx! { CloudOff { size: 32 } },
            }
        };
    }
    rsx! {
        div { class: "flex flex-col gap-2",
            for account in items.iter().cloned() {
                CalDavAccountRow {
                    key: "{account.id}",
                    account: account.clone(),
                    on_delete: move |id| on_delete.call(id),
                }
            }
        }
    }
}

#[component]
pub fn CalDavAccountRow(account: CalDavAccount, on_delete: EventHandler<Uuid>) -> Element {
    let id = account.id;
    let kind = account.kind.clone().unwrap_or_else(|| "caldav".into());
    let last_sync = account
        .last_sync_at
        .map(|t| t.format("%Y-%m-%d %H:%M").to_string())
        .unwrap_or_else(|| "never".into());
    let meta = format!("{kind} · {} · last sync: {last_sync}", account.username);
    let has_error = account.last_error.is_some();
    let err_text = account.last_error.clone().unwrap_or_default();
    rsx! {
        Item {
            ItemContent {
                ItemTitle { "{account.display_name}" }
                ItemDescription { "{meta}" }
                if has_error {
                    ItemDescription { class: "text-destructive", "⚠ {err_text}" }
                }
            }
            ItemActions { class: "gap-2",
                if has_error {
                    StatusBadge { variant: StatusBadgeVariant::Danger, label: "Error" }
                } else {
                    Badge { variant: BadgeVariant::Secondary, "{kind}" }
                }
                Button {
                    variant: ButtonVariant::Ghost,
                    size: ButtonSize::Small,
                    on_click: move |_| on_delete.call(id),
                    Trash2 { size: 14 }
                }
            }
        }
    }
}

#[component]
pub fn CalDavAccountCreateForm(on_submit: EventHandler<CalDavAccountCreate>) -> Element {
    let mut display_name = use_signal(String::new);
    let mut base_url = use_signal(String::new);
    let mut username = use_signal(String::new);
    let mut kind = use_signal(|| "nextcloud".to_string());
    rsx! {
        Card {
            CardHeader {
                CardTitle { "New CalDAV account" }
            }
            CardContent { class: "flex flex-col gap-3",
                div { class: "flex flex-wrap gap-2",
                    Input {
                        value: display_name,
                        placeholder: "Display name",
                        class: "flex-1 min-w-40",
                    }
                    Input {
                        value: username,
                        placeholder: "Username",
                        class: "flex-1 min-w-40",
                    }
                }
                Input {
                    value: base_url,
                    placeholder: "https://cloud.example.com/remote.php/dav/principals/users/alice/",
                    class: "w-full",
                }
                div { class: "flex items-center gap-3",
                    select {
                        class: "rounded-md border border-input bg-background px-3 py-2 text-sm",
                        value: "{kind}",
                        onchange: move |evt| kind.set(evt.value()),
                        option { value: "nextcloud", "Nextcloud" }
                        option { value: "icloud", "iCloud" }
                        option { value: "owncloud", "Owncloud" }
                        option { value: "other", "Other" }
                    }
                    div { class: "flex-1" }
                    Button {
                        on_click: move |_| {
                            let dn = display_name.read().clone();
                            let url = base_url.read().clone();
                            let user = username.read().clone();
                            if dn.trim().is_empty() || url.trim().is_empty() || user.trim().is_empty() {
                                return;
                            }
                            let payload = CalDavAccountCreate {
                                display_name: dn,
                                base_url: url,
                                username: user,
                                kind: trim_to_option(kind.read().clone()),
                                last_sync_at: None,
                                last_error: None,
                                tags: Vec::new(),
                            };
                            on_submit.call(payload);
                            display_name.set(String::new());
                            base_url.set(String::new());
                            username.set(String::new());
                        },
                        Plus { size: 14 }
                        " Add account"
                    }
                }
            }
        }
    }
}

fn trim_to_option(s: String) -> Option<String> {
    let t = s.trim();
    if t.is_empty() {
        None
    } else {
        Some(t.to_string())
    }
}
