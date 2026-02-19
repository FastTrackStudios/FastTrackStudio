mod browser_dialog;
mod editor_tab;
mod manage_tab;
mod performance_tab;

use dioxus::prelude::*;
use session_ui::PerformanceLayout;

pub(crate) use browser_dialog::SignalBrowserDialog;
pub(crate) use editor_tab::SignalEditorTab;
pub(crate) use manage_tab::SignalManageTab;
pub(crate) use performance_tab::SignalPerformanceTab;

/// A preset item in the manage tab sidebar — either a Rig or Layer.
#[derive(Clone, PartialEq)]
pub(crate) struct ManagePresetItem {
    pub(crate) id: String,
    pub(crate) name: String,
    pub(crate) is_rig: bool,
    pub(crate) sub_items: Vec<(String, String)>, // (id, name) — scenes for rigs, variants for layers
}

/// A profile item in the manage tab sidebar — with expandable patches.
#[derive(Clone, PartialEq)]
pub(crate) struct ManageProfileItem {
    pub(crate) id: String,
    pub(crate) name: String,
    pub(crate) patches: Vec<(String, String)>, // (id, name)
}

/// Performance view — renders the PerformanceLayout.
#[component]
pub(crate) fn PerformanceWithChartToggle() -> Element {
    rsx! {
        div {
            class: "relative h-full w-full bg-background",
            PerformanceLayout {}
        }
    }
}

// ---------------------------------------------------------------------------
// Signal view — Performance + Manage sub-tabs, browser dialog
// ---------------------------------------------------------------------------

/// Sub-tabs for the Signal page.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum SignalTab {
    Performance,
    Manage,
    Editor,
}

/// Signal top-level view with Performance / Manage tabs and a Browser dialog.
#[component]
pub(crate) fn SignalView() -> Element {
    let mut active_tab = use_signal(|| SignalTab::Manage);
    let mut browser_open = use_signal(|| false);

    rsx! {
        div { class: "h-full w-full flex flex-col bg-card overflow-hidden",
            // Toolbar: sub-tabs (left) + browser button (right)
            div { class: "flex items-center justify-between px-3 py-1.5 border-b border-border bg-zinc-900/60 flex-shrink-0",
                // Left: sub-tab pills
                div { class: "flex items-center gap-0.5 bg-zinc-800/80 rounded-lg p-0.5",
                    for (tab, label) in [(SignalTab::Performance, "Performance"), (SignalTab::Manage, "Manage"), (SignalTab::Editor, "Editor")] {
                        {
                            let is_active = active_tab() == tab;
                            rsx! {
                                button {
                                    class: if is_active {
                                        "px-3 py-1 text-xs font-medium text-white bg-zinc-700 rounded-md transition-colors"
                                    } else {
                                        "px-3 py-1 text-xs font-medium text-zinc-400 hover:text-zinc-200 hover:bg-zinc-800 rounded-md transition-colors"
                                    },
                                    onclick: move |_| active_tab.set(tab),
                                    "{label}"
                                }
                            }
                        }
                    }
                }

                // Right: browser button
                button {
                    class: "px-3 py-1 text-xs font-medium text-zinc-400 hover:text-zinc-200 hover:bg-zinc-800 rounded-md transition-colors",
                    onclick: move |_| browser_open.set(true),
                    "Browser"
                }
            }

            // Tab content
            div { class: "flex-1 min-h-0 overflow-hidden",
                match active_tab() {
                    SignalTab::Performance => rsx! {
                        SignalPerformanceTab {}
                    },
                    SignalTab::Manage => rsx! {
                        SignalManageTab {}
                    },
                    SignalTab::Editor => rsx! {
                        SignalEditorTab {}
                    },
                }
            }

            // Browser dialog (near-full-screen)
            if browser_open() {
                SignalBrowserDialog {
                    on_close: move |_| browser_open.set(false),
                }
            }
        }
    }
}
