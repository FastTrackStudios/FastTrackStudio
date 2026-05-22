//! Phase 8 — `/servers` route. Add / remove / sign in to remote
//! task-servers. Persisted via [`crate::server_registry`].

use dioxus::prelude::*;
use fts_ui::prelude::*;
use uuid::Uuid;

use crate::server_registry::{ServerEntry, ServerRegistry};

#[component]
pub fn ServersView() -> Element {
    // One registry per app instance — held in context so other
    // routes (federated view) can read the same entries.
    let mut registry: ServerRegistry = use_context_provider(ServerRegistry::new);
    let entries = registry.list();

    let mut label = use_signal(String::new);
    let mut server_url = use_signal(String::new);
    let mut email = use_signal(String::new);
    let mut password = use_signal(String::new);
    let mut last_error: Signal<Option<String>> = use_signal(|| None);

    let on_add = move |_| {
        let lbl = label.read().trim().to_string();
        let url = server_url.read().trim().to_string();
        let em = email.read().trim().to_string();
        let pw = password.read().clone();
        if lbl.is_empty() || url.is_empty() {
            last_error.set(Some("label and server URL are required".into()));
            return;
        }
        spawn(async move {
            let entry = ServerEntry::new(lbl, url.clone());
            // Sign in if email + password supplied. Empty
            // credentials → entry stored without a token (works
            // for dev servers that don't enforce capabilities).
            let signed = if !em.is_empty() && !pw.is_empty() {
                sign_in(&url, &em, &pw).await
            } else {
                Ok(None)
            };
            match signed {
                Ok(token_pair) => {
                    let mut e = entry;
                    if let Some((token, user_id)) = token_pair {
                        e.session_token = Some(token);
                        e.my_user_id = Some(user_id);
                        e.my_email = Some(em);
                    }
                    registry.upsert(e);
                    label.set(String::new());
                    server_url.set(String::new());
                    email.set(String::new());
                    password.set(String::new());
                    last_error.set(None);
                }
                Err(e) => last_error.set(Some(e)),
            }
        });
    };

    rsx! {
        div {
            id: "servers-route",
            class: "mx-auto flex max-w-3xl flex-col gap-4 p-6 lg:p-10",
            Heading { level: HeadingLevel::H1, "Servers" }
            Text { variant: TextVariant::Muted,
                "Each row is a remote task-server this app federates with."
            }
            if let Some(err) = last_error.read().as_ref() {
                div { "data-testid": "servers-error",
                    class: "rounded-md border border-border bg-card p-3 text-sm",
                    "{err}"
                }
            }
            // ─ Existing entries ─
            section {
                "data-testid": "servers-list",
                class: "rounded-md border border-border bg-card p-3 flex flex-col gap-2",
                if entries.is_empty() {
                    Text { variant: TextVariant::Muted, "No servers yet." }
                }
                for entry in entries.iter() {
                    ServerRow {
                        key: "{entry.id}",
                        entry: entry.clone(),
                        on_remove: {
                            let id = entry.id;
                            let mut registry = registry;
                            EventHandler::new(move |()| registry.remove(id))
                        },
                    }
                }
            }
            // ─ Add form ─
            section {
                "data-testid": "servers-add-form",
                class: "rounded-md border border-border bg-card p-4 flex flex-col gap-2",
                Heading { level: HeadingLevel::H3, "Add a server" }
                LabeledInput { label: "Label", value: label, testid: "servers-input-label" }
                LabeledInput {
                    label: "Server URL (ws://… or http://…)",
                    value: server_url,
                    testid: "servers-input-url",
                }
                LabeledInput {
                    label: "Email (optional)",
                    value: email,
                    testid: "servers-input-email",
                }
                LabeledInput {
                    label: "Password (optional)",
                    value: password,
                    testid: "servers-input-password",
                }
                span { "data-testid": "servers-add-button",
                    Button { on_click: on_add, "Add server" }
                }
            }
        }
    }
}

#[component]
fn ServerRow(entry: ServerEntry, on_remove: EventHandler<()>) -> Element {
    let id = entry.id;
    let row_testid = format!("server-row-{id}");
    let signed_in = entry.session_token.is_some();
    rsx! {
        div {
            "data-testid": row_testid,
            "data-signed-in": if signed_in { "true" } else { "false" },
            class: "flex items-center justify-between gap-3 px-2 py-1",
            div { class: "flex flex-col",
                span { class: "text-sm font-medium", "{entry.label}" }
                span { class: "text-xs text-muted-foreground", "{entry.server_url}" }
                if let Some(email) = entry.my_email.as_ref() {
                    span { class: "text-xs text-muted-foreground",
                        "signed in as {email}"
                    }
                }
            }
            span { "data-testid": format!("servers-remove-{id}"),
                Button { on_click: move |_| on_remove.call(()), "Remove" }
            }
        }
    }
}

#[component]
fn LabeledInput(label: String, value: Signal<String>, testid: String) -> Element {
    let v = value.read().clone();
    rsx! {
        label { class: "flex flex-col gap-1 text-sm",
            span { class: "text-muted-foreground", "{label}" }
            input {
                "data-testid": testid,
                r#type: "text",
                class: "rounded border border-border bg-background px-2 py-1",
                value: "{v}",
                oninput: move |e| value.set(e.value()),
            }
        }
    }
}

/// Sign in to `server_url`. Phase 8 stub: not wired through to
/// architect-auth in the wasm bundle because `architect-auth`
/// transitively pulls in tokio TCP / sea-orm and inflates the
/// wasm binary. A subsequent commit will either move the auth
/// vox client into its own wasm-clean crate or stand up a small
/// fetch-based HTTP shim. For Phase 8 MVP, entries store URL +
/// label only; the federated view still reads public seeded data.
async fn sign_in(
    _server_url: &str,
    _email: &str,
    _password: &str,
) -> Result<Option<(String, Uuid)>, String> {
    // Returning Ok(None) means "no token captured"; entry is
    // stored without a session, federated view will still hit
    // the server (works against dev servers with no capability
    // enforcement).
    Ok(None)
}
