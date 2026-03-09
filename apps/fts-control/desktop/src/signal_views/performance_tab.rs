use dioxus::prelude::*;
use signal_ui::views::{ProfilePatchGrid, SongSectionGrid};

use super::macro_bar::{build_bypass_rules, mock_preset_macro_bar, MacroBar};
use super::{SignalMode, SIGNAL_MODE};
use crate::actions::{SIGNAL_ACTIVE_PATCH_ID, SIGNAL_PATCH_IDS};
use crate::daw_registry;

/// Performance tab — macro bar on top, 4x2 tile grid below.
///
/// The macro bar shows the current preset's macro knobs. When the active patch
/// changes, the macro bank updates to reflect the new preset's macros.
/// For now, a mock macro bank is used until real macro banks are wired to presets.
#[component]
pub(crate) fn SignalPerformanceTab() -> Element {
    let signal = signal_ui::use_signal_service();
    let mode = SIGNAL_MODE();

    // Macro bank state — initialized with mock data, will be swapped per-preset later
    let macro_bank = use_signal(mock_preset_macro_bar);
    let bypass_rules = use_signal(build_bypass_rules);

    // Discover the first profile and first song IDs on mount.
    let mut profile_id = use_signal(|| None::<String>);
    let mut song_id = use_signal(|| None::<String>);
    let active_id = SIGNAL_ACTIVE_PATCH_ID();

    {
        let signal = signal.clone();
        use_effect(move || {
            let signal = signal.clone();
            spawn(async move {
                // Load first profile
                if let Ok(profiles) = signal.profiles().list().await {
                    if let Some(first) = profiles.first() {
                        let pid = first.id.to_string();

                        // Populate SIGNAL_PATCH_IDS with this profile's patch IDs
                        let patch_ids: Vec<String> =
                            first.patches.iter().map(|p| p.id.to_string()).collect();
                        *SIGNAL_PATCH_IDS.write() = patch_ids;

                        // Set default active patch if none set
                        if SIGNAL_ACTIVE_PATCH_ID.peek().is_none() {
                            if let Some(first_patch) = first.patches.first() {
                                *SIGNAL_ACTIVE_PATCH_ID.write() =
                                    Some(first_patch.id.to_string());
                            }
                        }

                        profile_id.set(Some(pid));
                    }
                }

                // Load first song
                if let Ok(songs) = signal.songs().list().await {
                    if let Some(first) = songs.first() {
                        song_id.set(Some(first.id.to_string()));
                    }
                }
            });
        });
    }

    // Handler for macro knob changes — drives DAW FX parameters directly
    let on_macro_change = move |(knob_id, new_val): (String, f32)| {
        let targets = signal_live::macro_registry::get_targets(&knob_id);
        if targets.is_empty() {
            return;
        }

        let Some(daw) = daw_registry::signal_daw() else {
            return;
        };

        spawn(async move {
            let Ok(project) = daw.current_project().await else {
                return;
            };

            for target in targets {
                // Map macro value (0.0–1.0) through the parameter range [min, max]
                let param_val = (target.min + (target.max - target.min) * new_val) as f64;

                let Ok(Some(track)) = project.tracks().by_guid(&target.track_guid).await else {
                    continue;
                };

                let Ok(Some(fx)) = track.fx_chain().by_guid(&target.fx_guid).await else {
                    continue;
                };

                // Set the parameter on the FX plugin
                let _ = fx.param(target.param_index).set(param_val).await;
            }
        });
    };

    rsx! {
        div { class: "flex flex-col h-full w-full overflow-hidden",
            // Macro bar at the top
            MacroBar {
                bank: macro_bank,
                bypass_rules: bypass_rules,
                on_macro_change: Some(EventHandler::new(on_macro_change)),
            }

            // Tile grid below
            div { class: "flex-1 min-h-0 overflow-hidden",
                {match mode {
                    SignalMode::Profile => {
                        match profile_id() {
                            Some(pid) => {
                                let signal = signal.clone();
                                rsx! {
                                    ProfilePatchGrid {
                                        profile_id: pid,
                                        active_patch_id: active_id.clone(),
                                        on_patch_select: move |(profile_id, patch_id): (String, String)| {
                                            let signal = signal.clone();
                                            // Optimistic highlight
                                            *SIGNAL_ACTIVE_PATCH_ID.write() = Some(patch_id.clone());
                                            spawn(async move {
                                                let patch_id_typed: signal::profile::PatchId = patch_id.clone().into();
                                                match signal.profiles().activate(profile_id, Some(patch_id_typed)).await {
                                                    Ok(_) => {
                                                        tracing::info!("Activated patch '{patch_id}'");
                                                    }
                                                    Err(e) => {
                                                        tracing::warn!("Patch activation failed: {e:?}");
                                                    }
                                                }
                                            });
                                        },
                                    }
                                }
                            }
                            None => rsx! {
                                div { class: "flex items-center justify-center h-full",
                                    p { class: "text-sm text-zinc-500 animate-pulse", "Loading profile..." }
                                }
                            },
                        }
                    }
                    SignalMode::Song => {
                        match song_id() {
                            Some(sid) => {
                                let signal = signal.clone();
                                rsx! {
                                    SongSectionGrid {
                                        song_id: sid,
                                        active_section_id: active_id.clone(),
                                        on_section_select: move |(song_id_str, section_id_str): (String, String)| {
                                            let signal = signal.clone();
                                            // Optimistic highlight
                                            *SIGNAL_ACTIVE_PATCH_ID.write() = Some(section_id_str.clone());
                                            spawn(async move {
                                                let target = signal_proto::resolve::ResolveTarget::SongSection {
                                                    song_id: song_id_str.into(),
                                                    section_id: section_id_str.clone().into(),
                                                };
                                                match signal.resolve_target(target).await {
                                                    Ok(_graph) => {
                                                        if signal.has_daw_applier() {
                                                            tracing::info!("Resolved section '{section_id_str}' — applying to DAW");
                                                        }
                                                    }
                                                    Err(e) => {
                                                        tracing::warn!("Section resolve failed: {e:?}");
                                                    }
                                                }
                                            });
                                        },
                                    }
                                }
                            }
                            None => rsx! {
                                div { class: "flex items-center justify-center h-full",
                                    p { class: "text-sm text-zinc-500 animate-pulse", "Loading song..." }
                                }
                            },
                        }
                    }
                    SignalMode::Preset => rsx! {
                        div { class: "flex items-center justify-center h-full",
                            p { class: "text-sm text-zinc-500", "Switch to Profile or Song mode to see the performance grid." }
                        }
                    },
                }}
            }
        }
    }
}
