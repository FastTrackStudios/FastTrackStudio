//! Stripped-down app shell: the only surface is the
//! CodeMirror-style markdown editor from the `editor` crate.
//! Other routes (projects, kanban, federated tasks, servers,
//! vox test, settings, dashboard) used to live behind a router
//! + sidebar — they're parked while we focus the product on the
//! editor + file-backed vault flow.
//!
//! Restoring them is a `git checkout` of this file.

use dioxus::prelude::*;
use editor::EditorApp;
use fts_ui::prelude::*;

/// Top-level component that the platform launchers mount.
///
/// Renders [`editor::EditorApp`] — the new editor crate's
/// turnkey markdown shell (live preview + vim + slash). The
/// legacy `editor_outliner::EditorApp` and the older
/// `knowledge_ui::LogseqShell` stay in tree for reference until
/// everything that needed them is reachable through the new
/// editor instead.
#[component]
pub fn App() -> Element {
    let theme_state = use_signal(|| {
        crate::theming::state_from_preset_name("logseq-classic-dark", ThemeMode::Dark)
    });
    rsx! {
        ThemeProvider { state: theme_state,
            div { class: "min-h-screen bg-background text-foreground",
                EditorApp {}
            }
        }
    }
}
