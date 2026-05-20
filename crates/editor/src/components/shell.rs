//! Top-level `EditorApp` — the only component apps mount.
//! Provides [`AppState`] via context, runs the initial vault
//! load, then renders the sidebar + main page.

use dioxus::prelude::*;

use crate::components::{PageView, Sidebar};
use crate::db::{Vault, load_vault, write_vault};
use crate::state::{AppState, Pane};

const EDITOR_CSS: &str = include_str!("shell.css");

#[component]
pub fn EditorApp() -> Element {
    let state = use_context_provider(AppState::new);

    // Initial vault load from `~/Task/{pages,journals}/`. We
    // run it once in a use_hook so it survives across renders;
    // re-runs happen via the file watcher (added separately).
    use_hook(move || {
        spawn(async move {
            initial_load(state).await;
        });
    });

    let pane = *state.pane.read();
    rsx! {
        style { dangerous_inner_html: EDITOR_CSS }
        div { class: "editor-shell",
            Sidebar {}
            main { class: "editor-main",
                match pane {
                    Pane::Outliner => rsx! { PageView {} },
                    Pane::Settings => rsx! { settings_placeholder {} },
                    Pane::Graph => rsx! { graph_placeholder {} },
                }
            }
        }
    }
}

#[component]
fn settings_placeholder() -> Element {
    rsx! { div { class: "editor-pane", h1 { "Settings" }, p { "Coming soon." } } }
}

#[component]
fn graph_placeholder() -> Element {
    rsx! { div { class: "editor-pane", h1 { "Graph" }, p { "Coming soon." } } }
}

/// One-shot async loader: read the vault from disk, seed if it's
/// brand new, route active_page to Home or the first available.
async fn initial_load(state: AppState) {
    let root = state.vault_root.peek().clone();
    let Some(root) = root else {
        tracing::warn!("no vault root resolved — running with empty state");
        return;
    };

    // Make sure the directories exist so the writer doesn't
    // explode on a clean install.
    #[cfg(not(target_arch = "wasm32"))]
    {
        let _ = std::fs::create_dir_all(root.join("pages"));
        let _ = std::fs::create_dir_all(root.join("journals"));
        let _ = std::fs::create_dir_all(root.join("assets"));
    }

    let mut vault = match load_vault(&root) {
        Ok(v) => v,
        Err(e) => {
            tracing::warn!(?e, "vault load failed; starting fresh");
            Vault::default()
        }
    };

    // Seed a Home page on a brand-new vault so the user has
    // somewhere to land.
    if vault.pages.is_empty() {
        seed_home(&mut vault);
        #[cfg(not(target_arch = "wasm32"))]
        if let Err(e) = write_vault(&root, &vault) {
            tracing::warn!(?e, "initial seed write failed");
        }
    }

    // Pick the page to open: Home → today's journal → first.
    let pick = vault
        .page_by_basename("Home")
        .or_else(|| {
            let today = chrono::Local::now().format("%Y-%m-%d").to_string();
            vault.page_by_basename(&today)
        })
        .or_else(|| vault.pages.first())
        .map(|p| p.id);

    state.vault.clone().set(vault);
    state.active_page.clone().set(pick);
    state.vault_revision.clone().with_mut(|r| *r += 1);
}

fn seed_home(vault: &mut Vault) {
    use crate::db::{new_block, new_page};
    let mut home = new_page("Home");
    home.basename = "Home".into();
    let page_id = home.id;
    vault.pages.push(home);
    let lines: &[&str] = &[
        "Welcome to your Task vault.",
        "Your notes live in `~/Task/pages/` as plain markdown.",
        "Try a wikilink: [[Home]] · A tag: #demo",
        "Cmd-K opens the command palette.",
    ];
    for (i, line) in lines.iter().enumerate() {
        let mut b = new_block(page_id, None, *line);
        b.sort_key = format!("{}", (b'a' + i as u8) as char);
        vault.blocks.push(b);
    }
}
