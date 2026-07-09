//! Profile selection step — choose which FTS rig types to install.

use dioxus::prelude::*;
use installer_core::profiles::{RigProfile, ALL_PROFILES};

// Pre-register all icon assets so Dioxus bundles them.
const ICON_REAPER: Asset = asset!("/assets/icons/reaper.png");
const ICON_LIVE: Asset = asset!("/assets/icons/live.png");
const ICON_GUITAR: Asset = asset!("/assets/icons/guitar.png");
const ICON_BASS: Asset = asset!("/assets/icons/bass.png");
const ICON_KEYS: Asset = asset!("/assets/icons/keys.png");
const ICON_DRUMS: Asset = asset!("/assets/icons/drums.png");
const ICON_DRUM_ENHANCE: Asset = asset!("/assets/icons/drum-enhancement.png");
const ICON_VOCALS: Asset = asset!("/assets/icons/vocals.png");
const ICON_SESSION: Asset = asset!("/assets/icons/session.png");

fn icon_for_profile(id: &str) -> Asset {
    match id {
        "reaper" => ICON_REAPER,
        "live" => ICON_LIVE,
        "guitar" => ICON_GUITAR,
        "bass" => ICON_BASS,
        "keys" => ICON_KEYS,
        "drums" => ICON_DRUMS,
        "drum-enhancement" => ICON_DRUM_ENHANCE,
        "vocals" => ICON_VOCALS,
        "session" => ICON_SESSION,
        _ => ICON_REAPER,
    }
}

#[component]
pub fn ProfilesStep(
    selected: Signal<Vec<String>>,
    on_next: EventHandler,
    on_back: EventHandler,
) -> Element {
    let all_selected = selected.read().len() == ALL_PROFILES.len();
    let count = selected.read().len();

    // Use absolute positioning to guarantee the bottom bar is always visible
    // and the profile list scrolls independently.
    rsx! {
        div {
            style: "position: absolute; top: 0; left: 0; right: 0; bottom: 0; display: flex; flex-direction: column;",

            // Header
            div {
                style: "padding: 16px 24px 8px 24px; flex-shrink: 0;",
                h2 { style: "font-size: 18px; font-weight: 600; margin-bottom: 4px; color: #eee;", "Choose Profiles" }
                div {
                    style: "display: flex; align-items: center; justify-content: space-between;",
                    p { style: "font-size: 12px; color: rgba(255,255,255,0.5);",
                        "Each profile creates a dedicated REAPER app."
                    }
                    button {
                        style: "font-size: 12px; color: rgba(255,255,255,0.4); cursor: pointer; text-decoration: underline; background: none; border: none;",
                        onclick: {
                            let mut selected = selected;
                            move |_| {
                                if all_selected {
                                    selected.set(vec![]);
                                } else {
                                    selected.set(ALL_PROFILES.iter().map(|p| p.id.to_string()).collect());
                                }
                            }
                        },
                        if all_selected { "Deselect all" } else { "Select all" }
                    }
                }
            }

            // Scrollable profile list — explicit max-height with overflow scroll
            div {
                style: "flex: 1 1 0; overflow-y: scroll; padding: 0 24px; -webkit-overflow-scrolling: touch;",
                div {
                    style: "display: flex; flex-direction: column; gap: 4px; padding-bottom: 8px;",
                    for profile in ALL_PROFILES.iter() {
                        ProfileRow {
                            profile: profile.clone(),
                            selected: selected,
                        }
                    }
                }
            }

            // Fixed bottom bar
            div {
                style: "display: flex; align-items: center; justify-content: space-between; padding: 12px 24px; border-top: 1px solid rgba(255,255,255,0.1); background: #111; flex-shrink: 0;",
                button {
                    style: "padding: 8px 16px; border-radius: 8px; border: 1px solid rgba(255,255,255,0.1); font-size: 14px; color: #eee; background: transparent; cursor: pointer;",
                    onclick: move |_| on_back.call(()),
                    "Back"
                }
                span {
                    style: "font-size: 12px; color: rgba(255,255,255,0.3);",
                    "{count} selected"
                }
                button {
                    style: "padding: 8px 24px; border-radius: 8px; font-size: 14px; font-weight: 500; color: #111; background: white; border: none; cursor: pointer;",
                    disabled: selected.read().is_empty(),
                    onclick: move |_| on_next.call(()),
                    "Install"
                }
            }
        }
    }
}

#[component]
fn ProfileRow(profile: RigProfile, selected: Signal<Vec<String>>) -> Element {
    let is_selected = selected.read().contains(&profile.id.to_string());
    let profile_id = profile.id.to_string();
    let icon_path = icon_for_profile(profile.id);

    let bg = if is_selected { "rgba(255,255,255,0.07)" } else { "transparent" };
    let opacity = if is_selected { "1" } else { "0.4" };

    rsx! {
        div {
            style: "display: flex; align-items: center; gap: 12px; padding: 6px 12px; border-radius: 8px; cursor: pointer; background: {bg}; opacity: {opacity}; transition: opacity 0.15s;",
            onclick: {
                let id = profile_id.clone();
                let mut selected = selected;
                move |_| {
                    let mut current = selected.read().clone();
                    if current.contains(&id) {
                        current.retain(|x| x != &id);
                    } else {
                        current.push(id.clone());
                    }
                    selected.set(current);
                }
            },

            // Checkbox
            div {
                style: format!(
                    "width: 16px; height: 16px; border-radius: 4px; border: 1px solid {}; display: flex; align-items: center; justify-content: center; flex-shrink: 0; {}",
                    if is_selected { "rgba(255,255,255,0.4)" } else { "rgba(255,255,255,0.1)" },
                    if is_selected { "background: rgba(255,255,255,0.2);" } else { "" }
                ),
                if is_selected {
                    span { style: "font-size: 10px; color: white;", "✓" }
                }
            }

            img {
                src: "{icon_path}",
                style: "width: 28px; height: 28px; border-radius: 4px;",
            }
            div {
                style: "flex: 1; min-width: 0;",
                span { style: "font-size: 14px; font-weight: 500; color: #eee;", "{profile.display_name}" }
                span { style: "font-size: 12px; color: rgba(255,255,255,0.3); margin-left: 8px;", "{profile.description}" }
            }
        }
    }
}
