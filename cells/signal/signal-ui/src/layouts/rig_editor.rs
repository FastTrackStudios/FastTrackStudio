//! Rig Editor Panel — sub-tab host for Block, Module, and Preset editors.
//!
//! Replaces the old SnapshotTest panel in the F8 screenset. Provides a sub-tab
//! bar that switches between:
//! - Performance (placeholder)
//! - Edit (existing RigLayout)
//! - Block Editor
//! - Module Editor
//! - Preset Editor

use crate::components::advanced_inspector::AdvancedInspectorView;
use crate::components::block_editor::BlockEditorView;
use crate::components::module_editor::ModuleEditorView;
use crate::components::preset_editor::PresetEditorView;
use crate::hooks::rig_state::use_rig_subscription;
use crate::prelude::*;
use crate::signals::{init_rig_service, RigEditorTab, RIG_EDITOR_TAB};

/// Rig Editor — the main panel for F8 screenset.
///
/// Hosts a sub-tab bar and renders the selected editor view.
#[component]
pub fn RigEditorPanel() -> Element {
    init_rig_service();
    use_rig_subscription();

    let active_tab = *RIG_EDITOR_TAB.read();

    rsx! {
        div { class: "h-full w-full flex flex-col bg-card overflow-hidden",
            // ── Sub-tab bar ──────────────────────────────────────
            div { class: "flex items-center gap-1 px-3 py-1.5 border-b border-border bg-zinc-900/60 flex-shrink-0",
                div { class: "flex items-center gap-0.5 bg-zinc-800/80 rounded-lg p-0.5",
                    for tab in RigEditorTab::all() {
                        EditorTabButton {
                            key: "{tab.display_name()}",
                            tab: *tab,
                            is_active: *tab == active_tab,
                        }
                    }
                }
            }

            // ── Tab content ──────────────────────────────────────
            div { class: "flex-1 flex flex-col min-h-0 overflow-hidden",
                match active_tab {
                    RigEditorTab::Performance => rsx! {
                        div { class: "h-full w-full flex items-center justify-center bg-card",
                            div { class: "text-center",
                                p { class: "text-lg font-semibold text-muted-foreground mb-2",
                                    "Performance View"
                                }
                                p { class: "text-sm text-muted-foreground/60",
                                    "Live performance mode — coming soon"
                                }
                            }
                        }
                    },
                    RigEditorTab::Edit => rsx! {
                        crate::layouts::rig_layout::RigLayout {}
                    },
                    RigEditorTab::BlockEditor => rsx! {
                        BlockEditorView {}
                    },
                    RigEditorTab::ModuleEditor => rsx! {
                        ModuleEditorView {}
                    },
                    RigEditorTab::PresetEditor => rsx! {
                        PresetEditorView {}
                    },
                    RigEditorTab::Advanced => rsx! {
                        AdvancedInspectorView {}
                    },
                }
            }
        }
    }
}

// ── Tab Button ───────────────────────────────────────────────────

#[derive(Props, Clone, PartialEq)]
struct EditorTabButtonProps {
    tab: RigEditorTab,
    is_active: bool,
}

#[component]
fn EditorTabButton(props: EditorTabButtonProps) -> Element {
    let tab = props.tab;

    rsx! {
        button {
            class: if props.is_active {
                "px-3 py-1.5 rounded-md text-xs font-medium bg-primary text-primary-foreground transition-colors"
            } else {
                "px-3 py-1.5 rounded-md text-xs font-medium text-zinc-400 hover:text-zinc-200 hover:bg-zinc-700/50 transition-colors"
            },
            onclick: move |_| {
                *RIG_EDITOR_TAB.write() = tab;
            },
            "{props.tab.display_name()}"
        }
    }
}
