//! `IntegrationSettings` — page-level settings surface for the
//! `/settings/integrations` route. Renders one card per known
//! integration plus a GitHub-repositories table.

use agent_proto::GitRepoConnection;
use dioxus::prelude::*;
use fts_ui::lucide_dioxus::{Copy as CopyIcon, Plus, Trash2};
use fts_ui::prelude::*;
use uuid::Uuid;

#[derive(Clone, PartialEq)]
pub enum IntegrationConfigStatus {
    Configured,
    NotConfigured,
    Error(String),
}

#[derive(Clone, PartialEq)]
pub struct ConfiguredIntegration {
    pub name: String,
    pub label: String,
    pub status: IntegrationConfigStatus,
    pub base_url: Option<String>,
    pub default_profile: Option<String>,
}

#[derive(Clone, Debug, PartialEq, Default)]
pub struct HermesConfigDraft {
    pub base_url: String,
    pub session_token: String,
    pub default_profile: String,
    pub webhook_secret: Option<String>,
}

#[derive(Clone, Debug, PartialEq, Default)]
pub struct GitRepoDraft {
    pub provider: String,
    pub owner: String,
    pub repo: String,
    pub default_branch: String,
    pub project_id: Option<Uuid>,
    pub webhook_secret: String,
}

#[component]
pub fn IntegrationSettings(
    configured: Vec<ConfiguredIntegration>,
    git_connections: Vec<GitRepoConnection>,
    on_save_hermes: EventHandler<HermesConfigDraft>,
    on_test_hermes: EventHandler<()>,
    on_add_repo: EventHandler<GitRepoDraft>,
    on_delete_repo: EventHandler<Uuid>,
) -> Element {
    let hermes = configured
        .iter()
        .find(|c| c.name == "hermes")
        .cloned()
        .unwrap_or(ConfiguredIntegration {
            name: "hermes".into(),
            label: "Hermes".into(),
            status: IntegrationConfigStatus::NotConfigured,
            base_url: None,
            default_profile: None,
        });

    let base_url = use_signal(|| hermes.base_url.clone().unwrap_or_default());
    let session_token = use_signal(String::new);
    let default_profile = use_signal(|| hermes.default_profile.clone().unwrap_or_default());
    let webhook_secret = use_signal(String::new);

    let (status_variant, status_text) = match &hermes.status {
        IntegrationConfigStatus::Configured => (StatusBadgeVariant::Success, "Configured".into()),
        IntegrationConfigStatus::NotConfigured => {
            (StatusBadgeVariant::Neutral, "Not configured".into())
        }
        IntegrationConfigStatus::Error(e) => (StatusBadgeVariant::Danger, format!("Error: {e}")),
    };

    // Add-repo dialog state
    let mut repo_dialog_open = use_signal(|| false);
    let repo_provider = use_signal(|| "github".to_string());
    let repo_owner = use_signal(String::new);
    let repo_name = use_signal(String::new);
    let repo_default_branch = use_signal(|| "main".to_string());
    let repo_webhook_secret = use_signal(String::new);

    rsx! {
        VStack { class: "gap-6",
            SectionHeader { label: "Integrations".to_string() }
            Text {
                variant: TextVariant::Muted,
                "Configure agent integrations and connected git repositories. Secrets are server-only."
            }

            // ── Hermes card ────────────────────────────────────────────
            Card {
                CardHeader {
                    HStack { class: "items-center justify-between",
                        CardTitle { "Hermes" }
                        StatusBadge { variant: status_variant, label: status_text }
                    }
                    CardDescription {
                        "Self-hosted agent runner. Set the base URL + session token to dispatch real runs."
                    }
                }
                CardContent { class: "flex flex-col gap-3",
                    div { class: "flex flex-col gap-1",
                        Label { "Base URL" }
                        Input {
                            value: base_url,
                            placeholder: "http://localhost:9119".to_string(),
                            size: InputSize::Small,
                        }
                    }
                    div { class: "flex flex-col gap-1",
                        Label { "Session token" }
                        Input {
                            value: session_token,
                            placeholder: "paste the hermes session cookie / token".to_string(),
                            size: InputSize::Small,
                        }
                    }
                    div { class: "flex flex-col gap-1",
                        Label { "Default agent profile" }
                        Input {
                            value: default_profile,
                            placeholder: "engineer".to_string(),
                            size: InputSize::Small,
                        }
                    }
                    div { class: "flex flex-col gap-1",
                        Label { "Webhook secret (optional)" }
                        Input {
                            value: webhook_secret,
                            placeholder: "verifies inbound /webhooks/hermes/event payloads".to_string(),
                            size: InputSize::Small,
                        }
                    }

                    // Cloudflare workaround note
                    div { class: "rounded-md border border-amber-500/40 bg-amber-500/5 p-3 text-xs text-foreground flex flex-col gap-2",
                        span { class: "font-medium",
                            "Hermes behind Cloudflare? Tunnel locally."
                        }
                        Text { variant: TextVariant::Muted,
                            "Cloudflare rejects requests with ‘Invalid Host header’ when the upstream is reached through the worker. Open an SSH tunnel and point Base URL at localhost instead:"
                        }
                        Kbd {
                            "ssh -L 9119:127.0.0.1:9119 root@starcommand"
                        }
                        Text { variant: TextVariant::Muted,
                            "Then use http://localhost:9119 above."
                        }
                    }
                }
                CardFooter { class: "gap-2",
                    Button {
                        variant: ButtonVariant::Outline,
                        size: ButtonSize::Small,
                        on_click: move |_| on_test_hermes.call(()),
                        "Test connection"
                    }
                    Button {
                        variant: ButtonVariant::Primary,
                        size: ButtonSize::Small,
                        on_click: move |_| {
                            let draft = HermesConfigDraft {
                                base_url: base_url.read().clone(),
                                session_token: session_token.read().clone(),
                                default_profile: default_profile.read().clone(),
                                webhook_secret: {
                                    let s = webhook_secret.read().clone();
                                    if s.is_empty() { None } else { Some(s) }
                                },
                            };
                            on_save_hermes.call(draft);
                        },
                        "Save"
                    }
                }
            }

            // ── GitHub repos card ──────────────────────────────────────
            Card {
                CardHeader {
                    HStack { class: "items-center justify-between",
                        div {
                            CardTitle { "Git repositories" }
                            CardDescription {
                                "Connect repos so agent runs can open PRs and post commit refs back to the originating task."
                            }
                        }
                        Button {
                            variant: ButtonVariant::Primary,
                            size: ButtonSize::Small,
                            on_click: move |_| repo_dialog_open.set(true),
                            Plus { size: 14 }
                            " Connect repo"
                        }
                    }
                }
                CardContent {
                    if git_connections.is_empty() {
                        EmptyState {
                            message: "No repositories connected yet.",
                        }
                    } else {
                        Table {
                            TableHeader {
                                TableRow {
                                    TableHead { "Provider" }
                                    TableHead { "Repository" }
                                    TableHead { "Default branch" }
                                    TableHead { "Webhook URL" }
                                    TableHead { "" }
                                }
                            }
                            TableBody {
                                for repo in git_connections.into_iter() {
                                    {
                                        let repo_id = repo.id;
                                        let webhook_url = format!(
                                            "{{base}}/webhooks/github/{}",
                                            repo.webhook_path
                                        );
                                        let webhook_url_for_copy = webhook_url.clone();
                                        rsx! {
                                            TableRow { key: "{repo_id}",
                                                TableCell { "{repo.provider}" }
                                                TableCell { "{repo.owner}/{repo.repo}" }
                                                TableCell { "{repo.default_branch}" }
                                                TableCell {
                                                    HStack { class: "items-center gap-2",
                                                        Kbd { "{webhook_url}" }
                                                        button {
                                                            r#type: "button",
                                                            class: "rounded p-1 text-muted-foreground hover:text-foreground",
                                                            title: "Copy URL",
                                                            onclick: move |_| {
                                                                copy_to_clipboard(&webhook_url_for_copy);
                                                            },
                                                            CopyIcon { size: 12 }
                                                        }
                                                    }
                                                }
                                                TableCell {
                                                    Button {
                                                        variant: ButtonVariant::Ghost,
                                                        size: ButtonSize::Small,
                                                        on_click: move |_| on_delete_repo.call(repo_id),
                                                        Trash2 { size: 14 }
                                                    }
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }

            // ── Add repo dialog ─────────────────────────────────────────
            Dialog {
                open: *repo_dialog_open.read(),
                on_open_change: move |o: bool| repo_dialog_open.set(o),
                DialogHeader {
                    DialogTitle { "Connect repository" }
                    DialogDescription { "Webhooks land at /webhooks/<provider>/<path>." }
                }
                div { class: "flex flex-col gap-3 px-1 py-2",
                    div { class: "flex flex-col gap-1",
                        Label { "Provider" }
                        NativeSelect { value: repo_provider,
                            NativeSelectOption { value: "github".to_string(), "github" }
                            NativeSelectOption { value: "gitlab".to_string(), "gitlab" }
                            NativeSelectOption { value: "forgejo".to_string(), "forgejo" }
                        }
                    }
                    div { class: "flex flex-col gap-1",
                        Label { "Owner" }
                        Input { value: repo_owner, placeholder: "Codys-Wright".to_string(), size: InputSize::Small }
                    }
                    div { class: "flex flex-col gap-1",
                        Label { "Repo name" }
                        Input { value: repo_name, placeholder: "Task".to_string(), size: InputSize::Small }
                    }
                    div { class: "flex flex-col gap-1",
                        Label { "Default branch" }
                        Input { value: repo_default_branch, placeholder: "main".to_string(), size: InputSize::Small }
                    }
                    div { class: "flex flex-col gap-1",
                        Label { "Webhook secret" }
                        Input { value: repo_webhook_secret, placeholder: "shared secret".to_string(), size: InputSize::Small }
                    }
                }
                DialogFooter {
                    Button {
                        variant: ButtonVariant::Outline,
                        size: ButtonSize::Small,
                        on_click: move |_| repo_dialog_open.set(false),
                        "Cancel"
                    }
                    Button {
                        variant: ButtonVariant::Primary,
                        size: ButtonSize::Small,
                        on_click: move |_| {
                            on_add_repo.call(GitRepoDraft {
                                provider: repo_provider.read().clone(),
                                owner: repo_owner.read().clone(),
                                repo: repo_name.read().clone(),
                                default_branch: repo_default_branch.read().clone(),
                                project_id: None,
                                webhook_secret: repo_webhook_secret.read().clone(),
                            });
                            repo_dialog_open.set(false);
                        },
                        "Connect"
                    }
                }
            }
        }
    }
}

fn copy_to_clipboard(text: &str) {
    #[cfg(target_arch = "wasm32")]
    {
        if let Some(win) = web_sys::window() {
            let _ = win.navigator().clipboard().write_text(text);
        }
    }
    #[cfg(not(target_arch = "wasm32"))]
    {
        // Native fallback — log so test runs are observable.
        let _ = text;
    }
}
