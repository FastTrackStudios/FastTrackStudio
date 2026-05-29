//! `/vault` — browse the org's markdown vault and edit files
//! live in the rich `editor::Editor` surface.
//!
//! Data path mirrors `/projects`: the wasm UI talks to the
//! server's `/org/<slug>/vox` endpoint through the
//! architect-emitted [`vault_proto::VaultSyncClient`]. The
//! left pane lists `.md` files from `manifest`; selecting one
//! pulls its bytes via `get_file` and seeds a fresh
//! [`EditorState`]. Save round-trips through `put_file` with
//! an `IfMatch::Sha` conditional write so a concurrent server
//! change surfaces as a conflict rather than a silent
//! clobber. The conditional sha is the manifest entry's
//! `sha256` (server-computed), so it always matches the
//! server's own hashing.
//!
//! The server registers exactly one vault per org under the
//! id `"default"` (`vault::Backend::single("default", …)` in
//! `apps/server`), so that's the only id we pass.

use dioxus::prelude::*;
use editor::editor_view::slash::{SlashMenu, SlashState};
use editor::editor_vim::VimState;
use editor::{Editor, EditorState, editor_view};
use fts_ui::prelude::*;

/// The single vault id the server hosts per org. Referenced
/// only from the wasm client calls.
#[cfg(target_arch = "wasm32")]
const VAULT_ID: &str = "default";

/// A markdown file as shown in the browser pane.
#[derive(Clone, PartialEq)]
struct FileMeta {
    path: String,
    sha256: String,
}

#[component]
pub fn VaultView() -> Element {
    let files = use_resource(|| async move { fetch_manifest().await });

    // Currently-open file + its editing state. `sha` is the
    // last-known-server hash, threaded into `put_file` so the
    // write is conditional.
    let mut selected = use_signal(|| None::<String>);
    let mut state = use_signal(|| EditorState::new(String::new()));
    let mut sha = use_signal(|| None::<String>);
    let mut status = use_signal(String::new);

    // Editor extensions — the same standard markdown setup the
    // turnkey `editor::EditorApp` uses, reused here so the
    // persistent surface behaves identically to the demo.
    let keymap = use_signal(editor::standard_markdown_keymap);
    let vim = use_signal(VimState::new);
    let slash = use_signal(|| None::<SlashState>);

    // Open a file: pull bytes, seed a fresh state, remember the
    // server sha so the next save is a conditional write.
    let open_file = move |meta: FileMeta| {
        spawn(async move {
            status.set(String::new());
            match fetch_file(meta.path.clone()).await {
                Ok(text) => {
                    state.set(EditorState::new(text));
                    sha.set(Some(meta.sha256));
                    selected.set(Some(meta.path));
                }
                Err(e) => status.set(format!("Load failed: {e}")),
            }
        });
    };

    let list = match &*files.read_unchecked() {
        Some(Ok(rows)) => render_file_list(rows, selected, move |meta| open_file(meta)),
        Some(Err(e)) => rsx! {
            div { class: "px-3 py-2 text-sm text-destructive",
                "Couldn't reach the vault service: {e}"
            }
        },
        None => rsx! {
            div { class: "flex items-center gap-2 px-3 py-2 text-sm text-muted-foreground",
                Spinner { size: SpinnerSize::Small }
                "Loading vault…"
            }
        },
    };

    let save = move |_| {
        let Some(path) = selected.peek().clone() else {
            return;
        };
        let cur_sha = sha.peek().clone();
        let text = state.peek().doc.to_string();
        spawn(async move {
            status.set("Saving…".to_owned());
            match save_file(path, text, cur_sha).await {
                Ok(new_sha) => {
                    sha.set(Some(new_sha));
                    status.set("Saved".to_owned());
                }
                Err(e) => status.set(format!("Save failed: {e}")),
            }
        });
    };

    let has_file = selected.read().is_some();
    let current = selected.read().clone().unwrap_or_default();
    let status_msg = status.read().clone();

    rsx! {
        div { class: "flex h-full min-h-[80vh]",
            // ── File browser ──────────────────────────────
            aside { class: "flex w-72 shrink-0 flex-col overflow-y-auto border-r border-border bg-muted/30",
                div { class: "px-3 py-3",
                    Heading { level: HeadingLevel::H3, "Vault" }
                    Text { variant: TextVariant::Muted, class: "text-xs",
                        "Markdown files via `VaultSyncClient`."
                    }
                }
                {list}
            }
            // ── Editor pane ───────────────────────────────
            div { class: "flex min-w-0 flex-1 flex-col",
                div { class: "flex items-center justify-between gap-3 border-b border-border px-4 py-2",
                    div { class: "min-w-0 truncate text-sm font-medium",
                        if has_file {
                            "{current}"
                        } else {
                            "No file selected"
                        }
                    }
                    div { class: "flex items-center gap-3",
                        if !status_msg.is_empty() {
                            Text { variant: TextVariant::Muted, class: "text-xs", "{status_msg}" }
                        }
                        Button {
                            variant: ButtonVariant::Primary,
                            size: ButtonSize::Small,
                            disabled: !has_file,
                            on_click: save,
                            "Save"
                        }
                    }
                }
                div { class: "flex min-h-0 flex-1 flex-col overflow-y-auto",
                    if has_file {
                        div { class: "editor-app",
                            div { class: "editor-frame",
                                Editor {
                                    state,
                                    keymap: keymap.read().clone(),
                                    decorations: editor::combined_decorations as editor_view::DecorationSource,
                                    vim: Some(vim),
                                    slash: Some(slash),
                                }
                                SlashMenu { state, slash }
                            }
                        }
                    } else {
                        div { class: "flex h-full items-center justify-center p-8",
                            Text { variant: TextVariant::Muted,
                                "Select a file from the left to start editing."
                            }
                        }
                    }
                }
            }
        }
        document::Link { rel: "stylesheet", href: editor::EDITOR_STYLE }
    }
}

/// Render the markdown-file list. `on_open` fires with the
/// selected [`FileMeta`] when a row is clicked.
fn render_file_list(
    rows: &[FileMeta],
    selected: Signal<Option<String>>,
    on_open: impl FnMut(FileMeta) + Clone + 'static,
) -> Element {
    if rows.is_empty() {
        return rsx! {
            div { class: "px-3 py-2 text-sm text-muted-foreground",
                "No markdown files yet. Seed one under the vault root."
            }
        };
    }
    let active = selected.read().clone();
    rsx! {
        nav { class: "flex flex-col gap-0.5 px-2 pb-4",
            for f in rows.iter() {
                {
                    let meta = f.clone();
                    let is_active = active.as_deref() == Some(meta.path.as_str());
                    let mut open = on_open.clone();
                    let cls = if is_active {
                        "w-full truncate rounded px-2 py-1 text-left text-sm bg-accent text-accent-foreground"
                    } else {
                        "w-full truncate rounded px-2 py-1 text-left text-sm hover:bg-accent/50"
                    };
                    rsx! {
                        button {
                            key: "{meta.path}",
                            class: cls,
                            onclick: move |_| open(meta.clone()),
                            "{f.path}"
                        }
                    }
                }
            }
        }
    }
}

/// List the vault's `.md` files, sorted by path.
async fn fetch_manifest() -> Result<Vec<FileMeta>, String> {
    let client = crate::vox_clients::vault_client().await?;
    #[cfg(target_arch = "wasm32")]
    {
        let m = client
            .manifest(VAULT_ID.to_owned())
            .await
            .map_err(|e| format!("manifest: {e:?}"))?;
        let mut files: Vec<FileMeta> = m
            .files
            .into_iter()
            .filter(|e| {
                std::path::Path::new(&e.path)
                    .extension()
                    .is_some_and(|ext| ext.eq_ignore_ascii_case("md"))
            })
            .map(|e| FileMeta {
                path: e.path,
                sha256: e.sha256,
            })
            .collect();
        files.sort_by(|a, b| a.path.cmp(&b.path));
        Ok(files)
    }
    #[cfg(not(target_arch = "wasm32"))]
    {
        let _ = client;
        Err("native client not wired yet".to_owned())
    }
}

/// Read one file's bytes as UTF-8 text.
async fn fetch_file(path: String) -> Result<String, String> {
    let client = crate::vox_clients::vault_client().await?;
    #[cfg(target_arch = "wasm32")]
    {
        let bytes = client
            .get_file(VAULT_ID.to_owned(), path)
            .await
            .map_err(|e| format!("get_file: {e:?}"))?;
        Ok(String::from_utf8_lossy(&bytes.0).into_owned())
    }
    #[cfg(not(target_arch = "wasm32"))]
    {
        let _ = (client, path);
        Err("native client not wired yet".to_owned())
    }
}

/// Conditional-write the file back. `prev_sha` is the
/// last-known-server hash; `None` means create-only. Returns
/// the freshly-committed sha for the next conditional write.
async fn save_file(path: String, text: String, prev_sha: Option<String>) -> Result<String, String> {
    let client = crate::vox_clients::vault_client().await?;
    #[cfg(target_arch = "wasm32")]
    {
        use vault_proto::IfMatch;
        let if_match = match prev_sha {
            Some(s) => IfMatch::Sha(s),
            None => IfMatch::CreateOnly,
        };
        let ack = client
            .put_file(VAULT_ID.to_owned(), path, text.into_bytes(), if_match)
            .await
            .map_err(|e| format!("put_file: {e:?}"))?;
        Ok(ack.sha256)
    }
    #[cfg(not(target_arch = "wasm32"))]
    {
        let _ = (client, path, text, prev_sha);
        Err("native client not wired yet".to_owned())
    }
}
