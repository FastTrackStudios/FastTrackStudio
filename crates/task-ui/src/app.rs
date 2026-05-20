//! Stripped-down app shell: the only surface is the Logseq-style
//! knowledge outliner. Other routes (projects, kanban, federated
//! tasks, servers, vox test, settings, dashboard) used to live
//! behind a router + sidebar — they're parked while we focus the
//! product on the knowledge editor + file-backed vault flow.
//!
//! Restoring them is a `git checkout` of this file.

use dioxus::prelude::*;
use fts_ui::prelude::*;
use knowledge_ui::LogseqShell;

/// Top-level component that the platform launchers mount.
#[component]
pub fn App() -> Element {
    // ThemeProvider keeps the design-token CSS variables wired so
    // any descendant still using `--ls-*` / shadcn tokens renders
    // correctly. We don't need an org switcher anymore — the
    // knowledge shell has its own theme toggle in Settings.
    let theme_state = use_signal(|| {
        crate::theming::state_from_preset_name("logseq-classic-dark", ThemeMode::Dark)
    });
    rsx! {
        ThemeProvider { state: theme_state,
            div { class: "min-h-screen bg-background text-foreground",
                LogseqShell {}
            }
        }
    }
}
