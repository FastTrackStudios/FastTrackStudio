//! Share dialog — create and manage share links for client review.

use dioxus::prelude::*;
use fts_ui::prelude::*;

/// Share link display data.
#[derive(Clone, PartialEq)]
pub struct ShareLinkData {
    pub token: String,
    pub label: String,
    pub url: String,
    pub allow_comments: bool,
    pub allow_download: bool,
    pub views: usize,
    pub downloads: usize,
    pub active: bool,
}

/// Dialog for creating and viewing share links.
#[component]
pub fn ShareDialog(
    open: bool,
    project: String,
    song: Option<String>,
    #[props(default)]
    existing_links: Vec<ShareLinkData>,
    #[props(default)]
    on_close: Option<Callback<()>>,
    #[props(default)]
    on_create: Option<Callback<(String, bool, bool)>>,
) -> Element {
    let mut label = use_signal(String::new);
    let mut allow_comments = use_signal(|| true);
    let mut allow_download = use_signal(|| true);
    let mut copied = use_signal(|| None::<String>);

    if !open {
        return rsx! {};
    }

    rsx! {
        Dialog {
            open: true,
            on_close: on_close.clone(),

            div { class: "flex flex-col gap-4",
                h3 { class: "text-base font-semibold", "Share Link" }
                p { class: "text-sm text-muted-foreground",
                    "Create a link for external review of "
                    span { class: "font-medium text-foreground",
                        if let Some(ref s) = song { "{s}" } else { "{project}" }
                    }
                }

                // Create new link
                div { class: "flex flex-col gap-3 p-3 rounded-lg bg-accent/30",
                    Input {
                        value: label,
                        placeholder: "Link label (e.g. 'Client Review')".to_string(),
                        size: InputSize::Small,
                    }
                    div { class: "flex items-center gap-4 text-xs",
                        label { class: "flex items-center gap-1.5 cursor-pointer",
                            input {
                                r#type: "checkbox",
                                checked: *allow_comments.read(),
                                onchange: move |_| { let v = *allow_comments.read(); allow_comments.set(!v); },
                            }
                            "Allow comments"
                        }
                        label { class: "flex items-center gap-1.5 cursor-pointer",
                            input {
                                r#type: "checkbox",
                                checked: *allow_download.read(),
                                onchange: move |_| { let v = *allow_download.read(); allow_download.set(!v); },
                            }
                            "Allow download"
                        }
                    }
                    Button {
                        variant: ButtonVariant::Primary,
                        size: ButtonSize::Small,
                        on_click: move |_| {
                            if let Some(ref cb) = on_create {
                                cb.call((label.read().clone(), *allow_comments.read(), *allow_download.read()));
                            }
                            label.set(String::new());
                        },
                        "Create Link"
                    }
                }

                // Existing links
                if !existing_links.is_empty() {
                    div { class: "flex flex-col gap-2",
                        span { class: "text-xs font-medium text-muted-foreground", "Active Links" }
                        for link in existing_links.iter() {
                            div { class: "flex items-center gap-3 p-2 rounded-lg border border-border",
                                div { class: "flex-1 min-w-0",
                                    p { class: "text-sm font-medium truncate", "{link.label}" }
                                    p { class: "text-[10px] text-muted-foreground truncate", "{link.url}" }
                                }
                                div { class: "flex items-center gap-2 text-[10px] text-muted-foreground shrink-0",
                                    span { { format!("{} views", link.views) } }
                                    span { { format!("{} downloads", link.downloads) } }
                                }
                                {
                                    let url = link.url.clone();
                                    let token = link.token.clone();
                                    rsx! {
                                        button {
                                            class: "text-xs text-muted-foreground hover:text-foreground transition-colors px-2 py-1 rounded bg-secondary",
                                            onclick: move |_| {
                                                // Copy to clipboard
                                                #[cfg(target_arch = "wasm32")]
                                                {
                                                    let _ = js_sys::eval(&format!("navigator.clipboard.writeText('{}')", url.replace('\'', "\\'")));
                                                }
                                                copied.set(Some(token.clone()));
                                                let token2 = token.clone();
                                                spawn(async move {
                                                    gloo_timers::future::TimeoutFuture::new(2000).await;
                                                    if copied.read().as_deref() == Some(&token2) {
                                                        copied.set(None);
                                                    }
                                                });
                                            },
                                            if copied.read().as_deref() == Some(link.token.as_str()) {
                                                "Copied!"
                                            } else {
                                                "Copy"
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
}
