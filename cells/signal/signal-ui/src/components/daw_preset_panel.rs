//! DAW Preset Panel — save and recall full FX chain state (state chunks + parameters).
//!
//! Unlike snapshot slots (parameter values only), presets capture the complete
//! binary plugin state, including internal settings not exposed as parameters.
//!
//! ```text
//! ┌─────────────────────────────────────────────────────────────┐
//! │  DAW Presets                              [+ Save Preset]   │
//! ├─────────────────────────────────────────────────────────────┤
//! │  ● Clean Setup          3 FX   Jan 15, 2025                │
//! │    Verse Crunch          3 FX   Jan 15, 2025                │
//! │    Lead Tone             4 FX   Jan 16, 2025                │
//! │    Ambient Wash          5 FX   Jan 16, 2025                │
//! └─────────────────────────────────────────────────────────────┘
//! ```

use crate::prelude::*;
use signal_storage::snapshot_service::SnapshotSummary;
use uuid::Uuid;

// ─────────────────────────────────────────────────────────────────────────────
// State
// ─────────────────────────────────────────────────────────────────────────────

/// A preset entry in the list (loaded from SQLite on startup).
#[derive(Clone, Debug, PartialEq)]
pub struct DawPresetEntry {
    pub id: Uuid,
    pub name: String,
    pub created_at: String,
    pub fx_count: usize,
    /// Whether this preset includes bundled parameter snapshots.
    pub has_snapshots: bool,
    /// Whether this is a module preset (loaded as a named container).
    pub is_module: bool,
}

impl From<SnapshotSummary> for DawPresetEntry {
    fn from(s: SnapshotSummary) -> Self {
        Self {
            id: s.id,
            name: s.name,
            created_at: s.created_at.format("%b %d, %Y").to_string(),
            fx_count: 0, // populated later from the data
            has_snapshots: false,
            is_module: false,
        }
    }
}

/// Global preset list for the current track.
pub static DAW_PRESETS: GlobalSignal<Vec<DawPresetEntry>> = Signal::global(Vec::new);

/// Currently active preset UUID (last recalled).
pub static DAW_ACTIVE_PRESET: GlobalSignal<Option<Uuid>> = Signal::global(|| None);

// ─────────────────────────────────────────────────────────────────────────────
// Props
// ─────────────────────────────────────────────────────────────────────────────

#[derive(Props, Clone, PartialEq)]
pub struct DawPresetPanelProps {
    /// Called when user clicks "Save Preset". Provides the entered name.
    pub on_save: Callback<String>,
    /// Called when user clicks a preset to recall it.
    pub on_recall: Callback<Uuid>,
    /// Called when user renames a preset. Provides (id, new_name).
    pub on_rename: Callback<(Uuid, String)>,
    /// Called when user deletes a preset.
    pub on_delete: Callback<Uuid>,
}

// ─────────────────────────────────────────────────────────────────────────────
// Component
// ─────────────────────────────────────────────────────────────────────────────

#[component]
pub fn DawPresetPanel(props: DawPresetPanelProps) -> Element {
    let presets = DAW_PRESETS.read();
    let active_id = *DAW_ACTIVE_PRESET.read();

    let mut show_save_dialog = use_signal(|| false);
    let mut save_name = use_signal(String::new);
    let mut renaming_id = use_signal(|| Option::<Uuid>::None);
    let mut rename_text = use_signal(String::new);
    let mut confirm_delete = use_signal(|| Option::<Uuid>::None);

    rsx! {
        div { class: "h-full w-full flex flex-col bg-card",
            // Header
            div { class: "flex items-center justify-between px-3 py-2 border-b border-border",
                h3 { class: "text-xs font-semibold text-muted-foreground uppercase tracking-wider",
                    "DAW Presets"
                }
                button {
                    class: "flex items-center gap-1 px-2 py-1 rounded text-[10px] font-medium \
                            bg-green-900/40 text-green-400 border border-green-800/40 \
                            hover:bg-green-800/50 transition-colors",
                    onclick: move |_| {
                        *save_name.write() = String::new();
                        *show_save_dialog.write() = true;
                    },
                    "+ Save Preset"
                }
            }

            // Save dialog (inline)
            if show_save_dialog() {
                div { class: "px-3 py-2 border-b border-border bg-zinc-900/50",
                    div { class: "flex items-center gap-2",
                        input {
                            class: "flex-1 text-xs bg-zinc-800 text-zinc-200 rounded px-2 py-1.5 \
                                    border border-zinc-700 outline-none focus:border-blue-500",
                            placeholder: "Preset name...",
                            autofocus: true,
                            value: "{save_name}",
                            oninput: move |evt| { *save_name.write() = evt.value(); },
                            onkeydown: move |evt| {
                                if evt.key() == Key::Enter {
                                    let name = save_name().trim().to_string();
                                    if !name.is_empty() {
                                        props.on_save.call(name);
                                        *show_save_dialog.write() = false;
                                    }
                                } else if evt.key() == Key::Escape {
                                    *show_save_dialog.write() = false;
                                }
                            },
                        }
                        button {
                            class: "px-2 py-1.5 rounded text-[10px] font-medium bg-green-900/40 \
                                    text-green-400 border border-green-800/40 hover:bg-green-800/50 \
                                    transition-colors disabled:opacity-30",
                            disabled: save_name().trim().is_empty(),
                            onclick: {
                                let on_save = props.on_save.clone();
                                move |_| {
                                    let name = save_name().trim().to_string();
                                    if !name.is_empty() {
                                        on_save.call(name);
                                        *show_save_dialog.write() = false;
                                    }
                                }
                            },
                            "Save"
                        }
                        button {
                            class: "px-2 py-1.5 rounded text-[10px] font-medium text-zinc-400 \
                                    hover:text-zinc-200 transition-colors",
                            onclick: move |_| { *show_save_dialog.write() = false; },
                            "Cancel"
                        }
                    }
                }
            }

            // Preset list
            div { class: "flex-1 overflow-y-auto",
                if presets.is_empty() {
                    div { class: "flex items-center justify-center h-full",
                        p { class: "text-xs text-muted-foreground italic", "No presets saved" }
                    }
                } else {
                    for preset in presets.iter() {
                        {
                            let preset_id = preset.id;
                            let is_active = active_id == Some(preset_id);
                            let is_renaming = renaming_id() == Some(preset_id);
                            let is_confirming_delete = confirm_delete() == Some(preset_id);
                            let name = preset.name.clone();
                            let created_at = preset.created_at.clone();
                            let fx_count = preset.fx_count;
                            let has_snapshots = preset.has_snapshots;
                            let is_module = preset.is_module;

                            rsx! {
                                div {
                                    key: "{preset_id}",
                                    class: if is_active {
                                        "flex items-center gap-2 px-3 py-2 border-b border-border \
                                         bg-green-950/30 cursor-pointer hover:bg-green-950/40 transition-colors"
                                    } else {
                                        "flex items-center gap-2 px-3 py-2 border-b border-border \
                                         cursor-pointer hover:bg-zinc-800/50 transition-colors"
                                    },
                                    onclick: {
                                        let on_recall = props.on_recall.clone();
                                        move |_| {
                                            *DAW_ACTIVE_PRESET.write() = Some(preset_id);
                                            on_recall.call(preset_id);
                                        }
                                    },

                                    // Active indicator
                                    div {
                                        class: if is_active {
                                            "w-2 h-2 rounded-full bg-green-500 flex-shrink-0"
                                        } else {
                                            "w-2 h-2 rounded-full bg-transparent flex-shrink-0"
                                        },
                                    }

                                    // Name (editable if renaming)
                                    if is_renaming {
                                        input {
                                            class: "flex-1 text-xs bg-zinc-700 text-zinc-200 rounded px-1 py-0.5 \
                                                    border border-zinc-600 outline-none focus:border-blue-500",
                                            value: "{rename_text}",
                                            autofocus: true,
                                            onclick: move |evt| { evt.stop_propagation(); },
                                            oninput: move |evt| { *rename_text.write() = evt.value(); },
                                            onkeydown: {
                                                let on_rename = props.on_rename.clone();
                                                move |evt| {
                                                    if evt.key() == Key::Enter {
                                                        let new_name = rename_text().trim().to_string();
                                                        if !new_name.is_empty() {
                                                            on_rename.call((preset_id, new_name));
                                                        }
                                                        *renaming_id.write() = None;
                                                    } else if evt.key() == Key::Escape {
                                                        *renaming_id.write() = None;
                                                    }
                                                }
                                            },
                                            onfocusout: {
                                                let on_rename = props.on_rename.clone();
                                                move |_| {
                                                    let new_name = rename_text().trim().to_string();
                                                    if !new_name.is_empty() {
                                                        on_rename.call((preset_id, new_name));
                                                    }
                                                    *renaming_id.write() = None;
                                                }
                                            },
                                        }
                                    } else {
                                        // Preset info
                                        div { class: "flex-1 min-w-0",
                                            div { class: "text-xs text-zinc-200 truncate", "{name}" }
                                            div { class: "flex items-center gap-2 text-[10px] text-zinc-500",
                                                if is_module {
                                                    span { class: "text-purple-400 font-medium", "MODULE" }
                                                }
                                                span { "{fx_count} FX" }
                                                if has_snapshots {
                                                    span { class: "text-blue-400", "+ snaps" }
                                                }
                                                span { "{created_at}" }
                                            }
                                        }
                                    }

                                    // Action buttons
                                    if !is_renaming {
                                        div { class: "flex items-center gap-1 flex-shrink-0",
                                            onclick: move |evt| { evt.stop_propagation(); },
                                            // Rename
                                            button {
                                                class: "px-1.5 py-0.5 rounded text-[10px] text-zinc-500 \
                                                        hover:text-zinc-300 hover:bg-zinc-700 transition-colors",
                                                onclick: move |_| {
                                                    *rename_text.write() = name.clone();
                                                    *renaming_id.write() = Some(preset_id);
                                                },
                                                "Rename"
                                            }
                                            // Delete (with confirmation)
                                            if is_confirming_delete {
                                                button {
                                                    class: "px-1.5 py-0.5 rounded text-[10px] text-red-400 \
                                                            bg-red-900/30 border border-red-800/40",
                                                    onclick: {
                                                        let on_delete = props.on_delete.clone();
                                                        move |_| {
                                                            on_delete.call(preset_id);
                                                            *confirm_delete.write() = None;
                                                        }
                                                    },
                                                    "Confirm"
                                                }
                                                button {
                                                    class: "px-1.5 py-0.5 rounded text-[10px] text-zinc-400 \
                                                            hover:text-zinc-200 transition-colors",
                                                    onclick: move |_| {
                                                        *confirm_delete.write() = None;
                                                    },
                                                    "Cancel"
                                                }
                                            } else {
                                                button {
                                                    class: "px-1.5 py-0.5 rounded text-[10px] text-zinc-500 \
                                                            hover:text-red-400 hover:bg-zinc-700 transition-colors",
                                                    onclick: move |_| {
                                                        *confirm_delete.write() = Some(preset_id);
                                                    },
                                                    "Delete"
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

            // Footer
            div { class: "px-3 py-1.5 border-t border-border",
                p { class: "text-[10px] text-muted-foreground text-center",
                    "Click to recall  |  State chunks + parameters"
                }
            }
        }
    }
}
