//! Stripped-down app shell: the only surface is the Logseq-style
//! knowledge outliner. Other routes (projects, kanban, federated
//! tasks, servers, vox test, settings, dashboard) used to live
//! behind a router + sidebar — they're parked while we focus the
//! product on the knowledge editor + file-backed vault flow.
//!
//! Restoring them is a `git checkout` of this file.

use dioxus::prelude::*;
use editor_outliner::EditorApp;
use fts_ui::prelude::*;

/// Top-level component that the platform launchers mount.
///
/// Renders the new file-backed `editor` crate's [`EditorApp`] —
/// the previous `knowledge_ui::LogseqShell` is left in place for
/// reference until the editor crate reaches feature parity.
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
