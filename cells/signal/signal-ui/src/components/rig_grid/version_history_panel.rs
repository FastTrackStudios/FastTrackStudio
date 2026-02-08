//! Version history panel — displays preset version history with restore and diff.
//!
//! Shows a chronological list of versions for the currently loaded preset,
//! with the ability to:
//! - View version details (timestamp, message, author)
//! - Restore to a previous version
//! - Compare two versions (diff view)
//! - See what changed between consecutive versions

use crate::prelude::*;
use crate::signals::RIG_CURRENT_PRESET;
use uuid::Uuid;

// region: --- Types

/// Summary info for a single preset version, displayed in the list.
#[derive(Debug, Clone, PartialEq)]
pub struct PresetVersionEntry {
    pub id: Uuid,
    pub version: i64,
    pub message: Option<String>,
    pub created_by_name: Option<String>,
    pub created_at: String,
    pub is_current: bool,
}

/// A single field-level change for display in the diff view.
#[derive(Debug, Clone, PartialEq)]
pub struct VersionChange {
    pub path: String,
    pub kind: VersionChangeKind,
}

/// The kind of change detected between two versions.
#[derive(Debug, Clone, PartialEq)]
pub enum VersionChangeKind {
    Added,
    Removed,
    Modified,
}

// endregion: --- Types

// region: --- Global Signal

/// Version history for the currently loaded preset.
/// Populated by the versioning service when a preset is loaded.
pub static RIG_PRESET_VERSIONS: GlobalSignal<Vec<PresetVersionEntry>> =
    Signal::global(Vec::new);

// endregion: --- Global Signal

// region: --- VersionHistoryPanel

/// Displays the version history for the currently loaded preset.
///
/// Reads from `RIG_CURRENT_PRESET` to show the preset name, and from
/// `RIG_PRESET_VERSIONS` for the version list. When no preset is loaded,
/// shows a placeholder.
#[component]
pub fn VersionHistoryPanel() -> Element {
    let preset = RIG_CURRENT_PRESET.read();
    let versions = RIG_PRESET_VERSIONS.read();
    let mut selected_version = use_signal(|| Option::<i64>::None);
    let mut diff_target = use_signal(|| Option::<i64>::None);

    // If no preset loaded, show empty state
    let Some(preset_info) = preset.as_ref() else {
        return rsx! {
            div { class: "h-full w-full flex flex-col items-center justify-center text-zinc-500 p-4",
                div { class: "text-sm", "No preset loaded" }
                div { class: "text-xs mt-1", "Load a preset to view its version history" }
            }
        };
    };

    // If no versions, show empty history
    if versions.is_empty() {
        return rsx! {
            div { class: "h-full w-full flex flex-col bg-zinc-900",
                // Header
                div { class: "p-3 border-b border-zinc-700",
                    div { class: "text-sm font-medium text-zinc-200", "Version History" }
                    div { class: "text-xs text-zinc-500 mt-0.5", "{preset_info.name}" }
                }
                div { class: "flex-1 flex flex-col items-center justify-center text-zinc-500 p-4",
                    div { class: "text-sm", "No versions yet" }
                    div { class: "text-xs mt-1", "Versions are created automatically when you save" }
                }
            }
        };
    }

    let selected_ver = selected_version();
    let diff_ver = diff_target();
    let version_count = versions.len();
    let version_label = if version_count == 1 {
        "1 version".to_string()
    } else {
        format!("{version_count} versions")
    };

    rsx! {
        div { class: "h-full w-full flex flex-col bg-zinc-900 overflow-hidden",
            // Header
            div { class: "p-3 border-b border-zinc-700 flex-shrink-0",
                div { class: "flex items-center justify-between",
                    div {
                        div { class: "text-sm font-medium text-zinc-200", "Version History" }
                        div { class: "text-xs text-zinc-500 mt-0.5", "{preset_info.name}" }
                    }
                    div { class: "text-xs text-zinc-500",
                        "{version_label}"
                    }
                }
            }

            // Version list (scrollable)
            div { class: "flex-1 overflow-y-auto",
                for entry in versions.iter() {
                    VersionRow {
                        key: "{entry.id}",
                        entry: entry.clone(),
                        is_selected: selected_ver == Some(entry.version),
                        is_diff_target: diff_ver == Some(entry.version),
                        on_select: move |ver: i64| {
                            if selected_ver == Some(ver) {
                                selected_version.set(None);
                            } else {
                                selected_version.set(Some(ver));
                            }
                        },
                        on_diff: move |ver: i64| {
                            if diff_ver == Some(ver) {
                                diff_target.set(None);
                            } else {
                                diff_target.set(Some(ver));
                            }
                        },
                    }
                }
            }

            // Action bar (when version selected)
            if let Some(ver) = selected_ver {
                div { class: "p-2 border-t border-zinc-700 flex-shrink-0 flex gap-2",
                    button {
                        class: "flex-1 px-3 py-1.5 text-xs font-medium bg-blue-600 hover:bg-blue-500 text-white rounded transition-colors",
                        onclick: move |_| {
                            // Restore would trigger versioning service
                            tracing::info!("Restore to version {ver}");
                        },
                        "Restore v{ver}"
                    }
                    if let Some(diff_v) = diff_ver {
                        button {
                            class: "flex-1 px-3 py-1.5 text-xs font-medium bg-zinc-700 hover:bg-zinc-600 text-zinc-200 rounded transition-colors",
                            onclick: move |_| {
                                tracing::info!("Diff v{ver} → v{diff_v}");
                            },
                            "Diff v{ver} → v{diff_v}"
                        }
                    }
                }
            }
        }
    }
}

// endregion: --- VersionHistoryPanel

// region: --- VersionRow

#[derive(Props, Clone, PartialEq)]
struct VersionRowProps {
    entry: PresetVersionEntry,
    is_selected: bool,
    is_diff_target: bool,
    on_select: EventHandler<i64>,
    on_diff: EventHandler<i64>,
}

#[component]
fn VersionRow(props: VersionRowProps) -> Element {
    let entry = &props.entry;
    let ver = entry.version;

    let bg = if props.is_selected {
        "bg-blue-900/30 border-l-2 border-l-blue-500"
    } else if props.is_diff_target {
        "bg-amber-900/20 border-l-2 border-l-amber-500"
    } else if entry.is_current {
        "bg-zinc-800/50 border-l-2 border-l-green-500"
    } else {
        "border-l-2 border-l-transparent hover:bg-zinc-800/30"
    };

    let message = entry
        .message
        .as_deref()
        .unwrap_or("Auto-saved");

    rsx! {
        div {
            class: "px-3 py-2 cursor-pointer transition-colors {bg}",
            onclick: move |_| props.on_select.call(ver),

            // Top row: version badge + timestamp
            div { class: "flex items-center justify-between",
                div { class: "flex items-center gap-2",
                    span { class: "text-xs font-mono px-1.5 py-0.5 rounded bg-zinc-700 text-zinc-300",
                        "v{entry.version}"
                    }
                    if entry.is_current {
                        span { class: "text-[10px] px-1 py-0.5 rounded bg-green-900/50 text-green-400",
                            "current"
                        }
                    }
                }
                span { class: "text-[10px] text-zinc-500", "{entry.created_at}" }
            }

            // Message
            div { class: "text-xs text-zinc-400 mt-1 truncate", "{message}" }

            // Author + diff button
            div { class: "flex items-center justify-between mt-1",
                if let Some(author) = &entry.created_by_name {
                    span { class: "text-[10px] text-zinc-600", "by {author}" }
                }
                button {
                    class: "text-[10px] text-zinc-500 hover:text-amber-400 transition-colors",
                    onclick: move |e: Event<MouseData>| {
                        e.stop_propagation();
                        props.on_diff.call(ver);
                    },
                    if props.is_diff_target { "cancel diff" } else { "compare" }
                }
            }
        }
    }
}

// endregion: --- VersionRow

// region: --- Tests

#[cfg(test)]
mod tests {
    use super::*;

    type Result<T> = core::result::Result<T, Box<dyn std::error::Error>>;

    #[test]
    fn test_preset_version_entry_creation() -> Result<()> {
        // -- Setup & Fixtures
        let entry = PresetVersionEntry {
            id: Uuid::new_v4(),
            version: 1,
            message: Some("Initial save".to_string()),
            created_by_name: Some("Alice".to_string()),
            created_at: "2026-02-08 10:30".to_string(),
            is_current: true,
        };

        // -- Check
        assert_eq!(entry.version, 1);
        assert!(entry.is_current);
        assert_eq!(entry.message.as_deref(), Some("Initial save"));
        assert_eq!(entry.created_by_name.as_deref(), Some("Alice"));
        Ok(())
    }

    #[test]
    fn test_preset_version_entry_no_message() -> Result<()> {
        // -- Setup & Fixtures
        let entry = PresetVersionEntry {
            id: Uuid::new_v4(),
            version: 5,
            message: None,
            created_by_name: None,
            created_at: "2026-02-08 11:00".to_string(),
            is_current: false,
        };

        // -- Check
        assert!(entry.message.is_none());
        assert!(entry.created_by_name.is_none());
        assert!(!entry.is_current);
        Ok(())
    }

    #[test]
    fn test_version_change_kinds() -> Result<()> {
        // -- Setup & Fixtures
        let added = VersionChange {
            path: "blocks.reverb".to_string(),
            kind: VersionChangeKind::Added,
        };
        let removed = VersionChange {
            path: "blocks.chorus".to_string(),
            kind: VersionChangeKind::Removed,
        };
        let modified = VersionChange {
            path: "blocks.amp.gain".to_string(),
            kind: VersionChangeKind::Modified,
        };

        // -- Check
        assert_eq!(added.kind, VersionChangeKind::Added);
        assert_eq!(removed.kind, VersionChangeKind::Removed);
        assert_eq!(modified.kind, VersionChangeKind::Modified);
        assert_eq!(added.path, "blocks.reverb");
        Ok(())
    }

    #[test]
    fn test_version_entry_equality() -> Result<()> {
        // -- Setup & Fixtures
        let id = Uuid::new_v4();
        let a = PresetVersionEntry {
            id,
            version: 3,
            message: Some("Test".to_string()),
            created_by_name: None,
            created_at: "2026-02-08".to_string(),
            is_current: false,
        };
        let b = a.clone();

        // -- Check
        assert_eq!(a, b);
        Ok(())
    }
}

// endregion: --- Tests
