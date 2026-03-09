use dioxus::prelude::*;
use signal_proto::{BlockType, PresetId, SnapshotId, ALL_BLOCK_TYPES};

use crate::actions::{SIGNAL_ACTIVE_PATCH_ID, SIGNAL_PATCH_IDS};

/// A flattened preset entry for the grid, combining block type + preset + default snapshot info.
#[derive(Clone, PartialEq)]
struct PresetEntry {
    block_type: BlockType,
    preset_id: PresetId,
    preset_name: String,
    default_snapshot_id: SnapshotId,
    snapshot_count: usize,
}

/// Preset mode — browse and tap-to-load any block preset.
#[component]
pub(crate) fn SignalPresetTab() -> Element {
    let signal = signal_ui::use_signal_service();
    let mut entries = use_signal(Vec::<PresetEntry>::new);
    let mut loading = use_signal(|| true);
    let mut activating = use_signal(|| None::<String>);

    // Load all block presets on mount
    {
        let signal = signal.clone();
        use_effect(move || {
            let signal = signal.clone();
            spawn(async move {
                let mut all_entries = Vec::new();

                // Load Custom first (full NDSP rfxchain presets), then the rest
                let mut ordered_types = vec![BlockType::Custom];
                ordered_types.extend(
                    ALL_BLOCK_TYPES
                        .iter()
                        .copied()
                        .filter(|bt| *bt != BlockType::Custom),
                );

                for block_type in ordered_types {
                    if let Ok(presets) = signal.block_presets().list(block_type).await {
                        for preset in presets {
                            let default_snap = preset.default_snapshot();
                            all_entries.push(PresetEntry {
                                block_type,
                                preset_id: preset.id().clone(),
                                preset_name: preset.name().to_string(),
                                default_snapshot_id: default_snap.id().clone(),
                                snapshot_count: preset.snapshots().len(),
                            });
                        }
                    }
                }

                // Populate SIGNAL_PATCH_IDS with the first 8 preset IDs for keyboard shortcuts
                let patch_ids: Vec<String> = all_entries
                    .iter()
                    .take(8)
                    .map(|e| e.preset_id.to_string())
                    .collect();
                *SIGNAL_PATCH_IDS.write() = patch_ids;

                entries.set(all_entries);
                loading.set(false);
            });
        });
    }

    // Group entries by block type for section headers
    let current_entries = entries();

    if loading() {
        return rsx! {
            div { class: "flex items-center justify-center h-full",
                p { class: "text-sm text-zinc-500 animate-pulse", "Loading presets..." }
            }
        };
    }

    if current_entries.is_empty() {
        return rsx! {
            div { class: "flex items-center justify-center h-full",
                p { class: "text-sm text-zinc-500", "No block presets found." }
            }
        };
    }

    // Build grouped sections
    let mut sections: Vec<(BlockType, Vec<PresetEntry>)> = Vec::new();
    for entry in &current_entries {
        if let Some(last) = sections.last_mut() {
            if last.0 == entry.block_type {
                last.1.push(entry.clone());
                continue;
            }
        }
        sections.push((entry.block_type, vec![entry.clone()]));
    }

    let active_id = SIGNAL_ACTIVE_PATCH_ID();

    rsx! {
        div { class: "h-full overflow-y-auto p-3",
            for (block_type, group) in sections {
                div { class: "mb-4",
                    // Section header with block type badge
                    {
                        let color = block_type.color();
                        let display = block_type.display_name();
                        let count = group.len();
                        let count_label = if count == 1 { "preset" } else { "presets" };
                        rsx! {
                            div { class: "flex items-center gap-2 mb-2",
                                span {
                                    class: "text-[10px] font-bold uppercase tracking-wider px-2 py-0.5 rounded",
                                    style: "background: {color.bg}; color: {color.border};",
                                    "{display}"
                                }
                                span { class: "text-[10px] text-zinc-500",
                                    "{count} {count_label}"
                                }
                            }
                        }
                    }

                    // Grid of preset tiles
                    div { class: "grid grid-cols-2 sm:grid-cols-3 md:grid-cols-4 lg:grid-cols-6 gap-2",
                        for entry in group {
                            {
                                let is_active = active_id.as_deref() == Some(&entry.preset_id.to_string());
                                let is_activating = activating() == Some(entry.preset_id.to_string());
                                let preset_id = entry.preset_id.clone();
                                let snapshot_id = entry.default_snapshot_id.clone();
                                let bt = entry.block_type;
                                let signal = signal.clone();

                                rsx! {
                                    button {
                                        class: "relative flex flex-col items-start p-3 rounded-lg text-left transition-all",
                                        style: if is_active {
                                            "border: 2px solid rgb(59,130,246); background: rgba(59,130,246,0.1);"
                                        } else {
                                            "border: 1px solid rgba(255,255,255,0.06); background: rgba(255,255,255,0.04);"
                                        },
                                        onclick: move |_| {
                                            let pid = preset_id.clone();
                                            let sid = snapshot_id.clone();
                                            let signal = signal.clone();
                                            let pid_str = pid.to_string();
                                            activating.set(Some(pid_str.clone()));
                                            *SIGNAL_ACTIVE_PATCH_ID.write() = Some(pid_str);

                                            spawn(async move {
                                                match signal.block_presets().activate(bt, pid, sid).await {
                                                    Ok(_) => {
                                                        tracing::info!("Activated block preset");
                                                    }
                                                    Err(e) => {
                                                        tracing::warn!("Block preset activation failed: {e:?}");
                                                    }
                                                }
                                                activating.set(None);
                                            });
                                        },

                                        // Preset name
                                        span { class: "text-sm font-medium text-zinc-100 truncate w-full",
                                            "{entry.preset_name}"
                                        }

                                        // Snapshot count
                                        if entry.snapshot_count > 1 {
                                            span { class: "text-[10px] text-zinc-500 mt-0.5",
                                                "{entry.snapshot_count} snapshots"
                                            }
                                        }

                                        // Loading spinner overlay
                                        if is_activating {
                                            div { class: "absolute inset-0 flex items-center justify-center bg-black/30 rounded-lg",
                                                div { class: "w-4 h-4 rounded-full animate-spin",
                                                    style: "border: 2px solid rgb(59,130,246); border-top-color: transparent;",
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
        }
    }
}
