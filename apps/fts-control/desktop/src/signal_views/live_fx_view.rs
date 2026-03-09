//! Live FX View — infer and display signal chain from a REAPER track's FX chain.
//!
//! Provides a track selector and renders the inferred signal chain structure
//! using the existing `RigGridPanel` grid view. No pre-configured rig required.

use dioxus::prelude::*;
use signal_ui::components::GridSlot;
use signal_ui::views::{engines_to_grid_slots, EngineFlowData, EngineParamLookup, RigGridPanel};

use crate::daw_registry;

// ─── Types ─────────────────────────────────────────────────────

#[derive(Clone, Debug, PartialEq)]
struct TrackEntry {
    guid: String,
    name: String,
    index: u32,
}

// ─── Data fetching ─────────────────────────────────────────────

/// Fetch the list of tracks from the connected DAW.
async fn fetch_tracks() -> Vec<TrackEntry> {
    let Some(daw) = daw_registry::signal_daw() else {
        return Vec::new();
    };
    let Ok(project) = daw.current_project().await else {
        return Vec::new();
    };
    let Ok(tracks) = project.tracks().all().await else {
        return Vec::new();
    };
    tracks
        .into_iter()
        .enumerate()
        .map(|(i, t)| TrackEntry {
            guid: t.guid,
            name: if t.name.is_empty() {
                format!("Track {}", i + 1)
            } else {
                t.name
            },
            index: i as u32,
        })
        .collect()
}

/// Fetch the FX tree for a track and infer its signal chain.
async fn infer_track_chain(guid: &str) -> Option<Vec<EngineFlowData>> {
    let daw = daw_registry::signal_daw()?;
    let project = daw.current_project().await.ok()?;
    let handle = project.tracks().by_guid(guid).await.ok()??;
    let tree = handle.fx_chain().tree().await.ok()?;
    let chain = signal_daw_bridge::infer_chain_from_fx_tree(&tree);
    let engine = signal_ui::infer_adapter::inferred_chain_to_engine_flow(&chain, "FX Chain");
    Some(vec![engine])
}

// ─── Component ─────────────────────────────────────────────────

#[component]
pub(crate) fn LiveFxView() -> Element {
    let mut tracks = use_signal(Vec::<TrackEntry>::new);
    let mut selected_guid = use_signal(|| None::<String>);
    let mut engines = use_signal(Vec::<EngineFlowData>::new);
    let mut loading = use_signal(|| false);

    // Fetch tracks on mount
    use_effect(move || {
        spawn(async move {
            tracks.set(fetch_tracks().await);
        });
    });

    // Infer chain when selection changes
    use_effect(move || {
        let guid = selected_guid();
        if let Some(guid) = guid {
            loading.set(true);
            engines.set(Vec::new());
            spawn(async move {
                if let Some(result) = infer_track_chain(&guid).await {
                    engines.set(result);
                }
                loading.set(false);
            });
        } else {
            engines.set(Vec::new());
        }
    });

    let track_list = tracks();
    let current_engines = engines();
    let is_loading = loading();

    rsx! {
        div { class: "flex flex-col h-full overflow-hidden",
            // ── Track selector bar ──
            div { class: "flex items-center gap-2 px-3 py-1.5 border-b border-border bg-zinc-900/40 flex-shrink-0",
                span { class: "text-[10px] text-zinc-500 flex-shrink-0", "Track:" }
                select {
                    class: "bg-zinc-800 text-zinc-200 text-xs rounded px-2 py-1 border border-zinc-700 focus:border-zinc-500 outline-none max-w-xs",
                    onchange: move |evt: Event<FormData>| {
                        let val = evt.value();
                        if val.is_empty() {
                            selected_guid.set(None);
                        } else {
                            selected_guid.set(Some(val));
                        }
                    },
                    option { value: "", "— Select a track —" }
                    for entry in track_list.iter() {
                        option {
                            key: "{entry.guid}",
                            value: "{entry.guid}",
                            "{entry.index}. {entry.name}"
                        }
                    }
                }
                // Refresh button
                button {
                    class: "text-[10px] text-zinc-500 hover:text-zinc-300 px-1.5 py-0.5 rounded hover:bg-zinc-800",
                    onclick: move |_| {
                        spawn(async move {
                            tracks.set(fetch_tracks().await);
                            // Re-infer if a track is selected
                            if let Some(guid) = selected_guid() {
                                if let Some(result) = infer_track_chain(&guid).await {
                                    engines.set(result);
                                }
                            }
                        });
                    },
                    "Refresh"
                }
            }

            // ── Grid view ──
            div { class: "flex-1 min-h-0 overflow-hidden",
                if !current_engines.is_empty() {
                    {
                        let grid_slots = engines_to_grid_slots(&current_engines, &EngineParamLookup::new());
                        rsx! {
                            RigGridPanel {
                                key: "{selected_guid().unwrap_or_default()}",
                                initial_slots: grid_slots,
                                on_selection_change: move |_sel| {},
                                on_param_change: move |_change: (_, String, f32)| {},
                                on_save: move |_slot: GridSlot| {},
                            }
                        }
                    }
                } else if is_loading {
                    div { class: "flex items-center justify-center h-full",
                        p { class: "text-sm text-muted-foreground", "Scanning FX chain..." }
                    }
                } else if selected_guid().is_some() {
                    div { class: "flex items-center justify-center h-full",
                        p { class: "text-sm text-muted-foreground", "No FX found on this track." }
                    }
                } else {
                    div { class: "flex items-center justify-center h-full",
                        p { class: "text-sm text-muted-foreground", "Select a track to view its signal chain." }
                    }
                }
            }
        }
    }
}
