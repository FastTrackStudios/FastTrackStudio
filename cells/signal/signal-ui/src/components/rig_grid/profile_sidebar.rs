//! Profile sidebar component for Profile view mode.
//!
//! Displays the list of available profiles with expandable scenes.
//! Similar to the profiles section in the left sidebar but as a dedicated right sidebar.

use crate::prelude::*;
use crate::signals::{RIG_AVAILABLE_PROFILES, RIG_CURRENT_PRESET, RIG_PROFILE};
use signal_control::{PatchInfo, ProfileInfo};
use uuid::Uuid;

/// Props for the profile sidebar.
#[derive(Props, Clone, PartialEq)]
pub struct GuitarRigProfileSidebarProps {
    /// Callback when a profile is selected (loads with default scene).
    pub on_profile_select: Callback<Uuid>,
    /// Callback when a profile + scene is selected (by scene index).
    #[props(default)]
    pub on_profile_scene_select: Option<Callback<(Uuid, usize)>>,
    /// Callback to create a new profile.
    #[props(default)]
    pub on_create_profile: Option<Callback<()>>,
}

/// Right sidebar for browsing and selecting profiles (Profile mode only).
///
/// Features:
/// - List of all available profiles
/// - Expandable to show scene templates
/// - Active state highlighting
#[component]
pub fn GuitarRigProfileSidebar(props: GuitarRigProfileSidebarProps) -> Element {
    // Read global signals
    let all_profiles = RIG_AVAILABLE_PROFILES.read();
    let current_profile = RIG_PROFILE.read();
    let current_preset = RIG_CURRENT_PRESET.read();
    let _current_preset_id = current_preset.as_ref().map(|p| p.id);

    rsx! {
        div { class: "h-full w-full flex flex-col bg-card border-l border-border",
            // Header
            div { class: "p-3 border-b border-zinc-800 flex-shrink-0",
                div { class: "flex items-center justify-between",
                    h3 { class: "text-xs font-semibold text-zinc-500 uppercase tracking-wider",
                        "Profiles"
                    }
                    div { class: "flex items-center gap-1.5",
                        span { class: "text-xs text-zinc-600 bg-zinc-800 px-1.5 py-0.5 rounded",
                            "{all_profiles.len()}"
                        }
                        if let Some(ref on_create) = props.on_create_profile {
                            {
                                let on_create = on_create.clone();
                                rsx! {
                                    button {
                                        class: "w-5 h-5 flex items-center justify-center rounded \
                                                text-zinc-500 hover:text-zinc-200 hover:bg-zinc-700 \
                                                transition-colors text-sm leading-none",
                                        title: "New Profile",
                                        onclick: move |_| on_create.call(()),
                                        "+"
                                    }
                                }
                            }
                        }
                    }
                }
            }

            // Profile list (scrollable)
            div { class: "flex-1 overflow-y-auto",
                if all_profiles.is_empty() {
                    div { class: "p-4 text-center text-zinc-500 text-sm",
                        p { "No profiles available" }
                    }
                } else {
                    for profile in all_profiles.iter() {
                        // Only expand the currently selected profile
                        ProfileItem {
                            profile: profile.clone(),
                            is_active: current_profile.as_ref().map(|p| p.id) == Some(profile.id),
                            is_expanded: current_profile.as_ref().map(|p| p.id) == Some(profile.id),
                            on_click: props.on_profile_select.clone(),
                            on_scene_click: props.on_profile_scene_select.clone(),
                        }
                    }
                }
            }
        }
    }
}

/// Props for profile item.
#[derive(Props, Clone, PartialEq)]
struct ProfileItemProps {
    profile: ProfileInfo,
    is_active: bool,
    is_expanded: bool,
    on_click: Callback<Uuid>,
    on_scene_click: Option<Callback<(Uuid, usize)>>,
}

/// Individual profile item with expandable patches.
#[component]
fn ProfileItem(props: ProfileItemProps) -> Element {
    let has_patches = !props.profile.patches.is_empty();
    let profile_id = props.profile.id;

    rsx! {
        div { class: "border-b border-zinc-800/50",
            // Profile header
            div {
                class: if props.is_active {
                    "px-3 py-2 cursor-pointer bg-zinc-800 border-l-2 border-green-500 transition-colors"
                } else {
                    "px-3 py-2 cursor-pointer hover:bg-zinc-800/50 border-l-2 border-transparent transition-colors"
                },
                onclick: move |_| props.on_click.call(profile_id),

                div { class: "flex items-start justify-between",
                    div { class: "flex-1 min-w-0",
                        div { class: "font-medium text-sm text-zinc-200 truncate",
                            "{props.profile.name}"
                        }
                        div { class: "text-xs text-zinc-500",
                            "{props.profile.patch_count} patches"
                        }
                    }
                }
            }

            // Expanded patches
            if props.is_expanded && has_patches {
                div { class: "bg-zinc-850 py-1",
                    for (patch_index, patch) in props.profile.patches.iter().enumerate() {
                        ProfilePatchItem {
                            key: "{patch_index}",
                            profile_id,
                            patch_index,
                            patch: patch.clone(),
                            is_active: false,
                            on_click: props.on_scene_click.clone(),
                        }
                    }
                }
            }
        }
    }
}

/// Props for profile patch item.
#[derive(Props, Clone, PartialEq)]
struct ProfilePatchItemProps {
    profile_id: Uuid,
    patch_index: usize,
    patch: PatchInfo,
    is_active: bool,
    on_click: Option<Callback<(Uuid, usize)>>,
}

/// Individual patch item within a profile.
#[component]
fn ProfilePatchItem(props: ProfilePatchItemProps) -> Element {
    let profile_id = props.profile_id;
    let patch_index = props.patch_index;
    let on_click = props.on_click.clone();

    rsx! {
        div {
            class: if props.is_active {
                "pl-6 pr-3 py-1.5 text-xs cursor-pointer bg-green-500/10 text-green-400 transition-colors"
            } else {
                "pl-6 pr-3 py-1.5 text-xs cursor-pointer text-zinc-400 hover:text-zinc-300 hover:bg-zinc-800/50 transition-colors"
            },
            onclick: move |_| {
                if let Some(ref cb) = on_click {
                    cb.call((profile_id, patch_index));
                }
            },
            "• {props.patch.name}"
        }
    }
}
