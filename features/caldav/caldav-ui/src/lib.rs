//! CalDAV feature UI. Dumb components — accounts list, add-account
//! form. The sync trigger lives in the parent route component in
//! `task-ui` since it needs the `CalDavService` client.

use caldav_proto::{CalDavAccount, CalDavAccountCreate};
use dioxus::prelude::*;
use uuid::Uuid;

#[component]
pub fn CalDavAccountList(items: Vec<CalDavAccount>, on_delete: EventHandler<Uuid>) -> Element {
    if items.is_empty() {
        return rsx! {
            div { class: "text-sm text-slate-500",
                "No CalDAV accounts yet. Add one above to connect to Nextcloud, iCloud, or another CalDAV server."
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
    rsx! {
        div { class: "flex items-center justify-between rounded-md border border-slate-800 bg-slate-900 px-4 py-3",
            div { class: "flex flex-col",
                span { class: "text-sm font-medium text-slate-100", "{account.display_name}" }
                span { class: "text-xs text-slate-500",
                    "{kind} · {account.username} · last sync: {last_sync}"
                }
                if let Some(err) = &account.last_error {
                    span { class: "text-xs text-rose-400", "⚠ {err}" }
                }
            }
            button {
                class: "text-xs text-slate-500 hover:text-rose-400",
                onclick: move |_| on_delete.call(id),
                "Delete"
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
        form {
            class: "flex flex-wrap gap-2",
            onsubmit: move |evt| {
                evt.prevent_default();
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
            input {
                class: "flex-1 min-w-40 rounded-md border border-slate-700 bg-slate-900 px-3 py-2 text-sm text-slate-100 placeholder:text-slate-500",
                placeholder: "Display name",
                value: "{display_name}",
                oninput: move |evt| display_name.set(evt.value()),
            }
            input {
                class: "flex-[2] min-w-64 rounded-md border border-slate-700 bg-slate-900 px-3 py-2 text-sm text-slate-100 placeholder:text-slate-500",
                placeholder: "https://cloud.example.com/remote.php/dav/principals/users/alice/",
                value: "{base_url}",
                oninput: move |evt| base_url.set(evt.value()),
            }
            input {
                class: "flex-1 min-w-40 rounded-md border border-slate-700 bg-slate-900 px-3 py-2 text-sm text-slate-100 placeholder:text-slate-500",
                placeholder: "Username",
                value: "{username}",
                oninput: move |evt| username.set(evt.value()),
            }
            select {
                class: "rounded-md border border-slate-700 bg-slate-900 px-3 py-2 text-sm text-slate-100",
                value: "{kind}",
                onchange: move |evt| kind.set(evt.value()),
                option { value: "nextcloud", "Nextcloud" }
                option { value: "icloud", "iCloud" }
                option { value: "owncloud", "Owncloud" }
                option { value: "other", "Other" }
            }
            button {
                r#type: "submit",
                class: "rounded-md bg-cyan-500 px-4 py-2 text-sm font-semibold text-slate-950 hover:bg-cyan-400",
                "Add account"
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
