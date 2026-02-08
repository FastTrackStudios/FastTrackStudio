//! Snapshot browser panel for instant snapshot recall.
//!
//! Lists all snapshots in the current preset and allows double-click
//! to instantly apply (recall) a snapshot. Shows the currently active
//! snapshot with a highlight.

use crate::prelude::*;
use crate::signals::{
    RIG_CURRENT_PRESET, RIG_CURRENT_PRESET_SNAPSHOT_ID, RIG_LAST_APPLIED_SNAPSHOT, RIG_LOADING,
};
use signal_control::PresetSnapshotInfo;
use uuid::Uuid;

// region: --- SnapshotBrowser

/// Props for the snapshot browser panel.
#[derive(Props, Clone, PartialEq)]
pub struct SnapshotBrowserProps {
    /// Callback when a snapshot is double-clicked (instant apply).
    pub on_activate_snapshot: Callback<Uuid>,
}

/// Snapshot browser panel — lists snapshots in the current preset.
///
/// Double-click a snapshot to instantly apply (recall) it. The currently
/// active snapshot is highlighted with a green accent.
#[component]
pub fn SnapshotBrowser(props: SnapshotBrowserProps) -> Element {
    let preset = RIG_CURRENT_PRESET.read();
    let active_snapshot_id = *RIG_CURRENT_PRESET_SNAPSHOT_ID.read();
    let last_applied = *RIG_LAST_APPLIED_SNAPSHOT.read();
    let is_loading = *RIG_LOADING.read();

    rsx! {
        div { class: "h-full w-full flex flex-col bg-card",
            // Header
            div { class: "p-3 border-b border-border",
                h2 { class: "text-sm font-semibold text-foreground", "Snapshots" }
                if let Some(ref preset) = *preset {
                    p { class: "text-xs text-muted-foreground mt-0.5 truncate",
                        "{preset.name}"
                    }
                }
            }

            // Snapshot list
            div { class: "flex-1 overflow-y-auto p-2",
                if let Some(ref preset) = *preset {
                    if preset.scenes.is_empty() {
                        div { class: "p-4 text-center text-muted-foreground text-sm",
                            "No snapshots"
                        }
                    } else {
                        for (index, snapshot) in preset.scenes.iter().enumerate() {
                            SnapshotItem {
                                key: "{snapshot.id}",
                                snapshot: snapshot.clone(),
                                index,
                                is_active: active_snapshot_id == Some(snapshot.id),
                                is_last_applied: last_applied == Some(snapshot.id),
                                is_loading,
                                on_activate: props.on_activate_snapshot.clone(),
                            }
                        }
                    }
                } else {
                    div { class: "p-4 text-center text-muted-foreground text-sm",
                        "No preset loaded"
                    }
                }
            }
        }
    }
}

// endregion: --- SnapshotBrowser

// region: --- SnapshotItem

/// Props for a single snapshot item.
#[derive(Props, Clone, PartialEq)]
struct SnapshotItemProps {
    snapshot: PresetSnapshotInfo,
    index: usize,
    is_active: bool,
    is_last_applied: bool,
    is_loading: bool,
    on_activate: Callback<Uuid>,
}

/// Single snapshot item — double-click to apply.
#[component]
fn SnapshotItem(props: SnapshotItemProps) -> Element {
    let snapshot_id = props.snapshot.id;

    let class = if props.is_active {
        "px-3 py-2.5 rounded cursor-pointer mb-1 bg-green-500/20 border border-green-500/40 \
         text-green-300 transition-colors"
    } else {
        "px-3 py-2.5 rounded cursor-pointer mb-1 hover:bg-muted border border-transparent \
         text-muted-foreground hover:text-foreground transition-colors"
    };

    rsx! {
        div {
            class,
            ondoubleclick: move |_| {
                props.on_activate.call(snapshot_id);
            },

            div { class: "flex items-center justify-between",
                div { class: "flex items-center gap-2 min-w-0",
                    // Active indicator dot
                    if props.is_active {
                        span { class: "w-2 h-2 rounded-full bg-green-400 flex-shrink-0" }
                    } else {
                        span { class: "w-2 h-2 rounded-full bg-transparent flex-shrink-0" }
                    }
                    span { class: "font-medium text-sm truncate", "{props.snapshot.name}" }
                }
                span { class: "text-xs opacity-50 ml-2 flex-shrink-0", "{props.index + 1}" }
            }

            // Brief flash feedback when this snapshot was just applied
            if props.is_last_applied && props.is_loading {
                div { class: "mt-1 h-0.5 bg-green-500/60 rounded animate-pulse" }
            }
        }
    }
}

// endregion: --- SnapshotItem
