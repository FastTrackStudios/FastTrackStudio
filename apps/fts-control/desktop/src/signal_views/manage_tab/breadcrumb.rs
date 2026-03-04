use dioxus::prelude::*;
use std::collections::HashMap;

use super::ManageMode;
use crate::signal_views::{ManagePresetItem, ManageProfileItem};
use signal::song::SectionSource;
use signal_ui::views::{SectionEntry, SongEntry};

/// Holds the resolved breadcrumb display strings.
pub(crate) struct BreadcrumbValues {
    pub song: Option<String>,
    pub section: Option<String>,
    pub profile: Option<String>,
    pub patch: Option<String>,
    pub preset: Option<String>,
    pub scene: Option<String>,
}

/// Derives breadcrumb display values from the current signal state.
pub(crate) fn resolve_breadcrumbs(
    mode: ManageMode,
    selected_song_id: &Option<String>,
    songs: &[SongEntry],
    selected_section_id: &Option<String>,
    song_sections: &[SectionEntry],
    section_sources: &HashMap<String, SectionSource>,
    selected_profile: &Option<String>,
    selected_patch: &Option<String>,
    manage_profiles: &[ManageProfileItem],
    current_preset: &Option<String>,
    current_sub: &Option<String>,
    manage_presets: &[ManagePresetItem],
) -> BreadcrumbValues {
    let bc_song = if mode == ManageMode::Song {
        selected_song_id
            .as_ref()
            .and_then(|sid| songs.iter().find(|s| s.id == *sid).map(|s| s.name.clone()))
    } else {
        None
    };

    let bc_section = if mode == ManageMode::Song {
        selected_section_id.as_ref().and_then(|sid| {
            song_sections
                .iter()
                .find(|s| s.id == *sid)
                .map(|s| s.name.clone())
        })
    } else {
        None
    };

    let (bc_profile, bc_patch) = {
        let from_source = selected_section_id
            .as_ref()
            .and_then(|sid| section_sources.get(sid).cloned())
            .and_then(|source| match source {
                SectionSource::Patch { patch_id } => {
                    let patch_str = patch_id.to_string();
                    manage_profiles.iter().find_map(|prof| {
                        prof.patches
                            .iter()
                            .find(|(pid, _)| *pid == patch_str)
                            .map(|(_, pname)| (Some(prof.name.clone()), Some(pname.clone())))
                    })
                }
                _ => None,
            });
        from_source.unwrap_or_else(|| {
            let prof_name = selected_profile.as_ref().and_then(|pid| {
                manage_profiles
                    .iter()
                    .find(|p| p.id == *pid)
                    .map(|p| p.name.clone())
            });
            let patch_name = selected_patch.as_ref().and_then(|patch_id| {
                manage_profiles.iter().find_map(|prof| {
                    prof.patches
                        .iter()
                        .find(|(pid, _)| *pid == *patch_id)
                        .map(|(_, pname)| pname.clone())
                })
            });
            (prof_name, patch_name)
        })
    };

    let bc_preset = current_preset.as_ref().and_then(|pid| {
        manage_presets
            .iter()
            .find(|p| &p.id == pid)
            .map(|p| p.name.clone())
    });

    let bc_scene = current_preset.as_ref().and_then(|pid| {
        current_sub.as_ref().and_then(|sid| {
            manage_presets.iter().find(|p| &p.id == pid).and_then(|p| {
                p.sub_items
                    .iter()
                    .find(|(id, _)| id == sid)
                    .map(|(_, n)| n.clone())
            })
        })
    });

    BreadcrumbValues {
        song: bc_song,
        section: bc_section,
        profile: bc_profile,
        patch: bc_patch,
        preset: bc_preset,
        scene: bc_scene,
    }
}

/// Renders the breadcrumb context row.
pub(crate) fn render_breadcrumb(mode: ManageMode, bc: &BreadcrumbValues) -> Element {
    if mode == ManageMode::Preset {
        return rsx! {};
    }

    rsx! {
        div { class: "flex items-center gap-1 px-3 py-1 border-b border-border/50 bg-zinc-950/30 flex-shrink-0 text-[10px]",
            if let Some(ref name) = bc.song {
                span { class: "text-zinc-500", "Song:" }
                span { class: "text-zinc-300 mr-2", "{name}" }
            }
            if let Some(ref name) = bc.section {
                span { class: "text-zinc-600", "\u{203A}" }
                span { class: "text-zinc-500 ml-1", "Section:" }
                span { class: "text-zinc-300 mr-2", "{name}" }
            }
            if let Some(ref name) = bc.profile {
                span { class: "text-zinc-600", "\u{203A}" }
                span { class: "text-zinc-500 ml-1", "Profile:" }
                span { class: "text-zinc-300 mr-2", "{name}" }
            }
            if let Some(ref name) = bc.patch {
                span { class: "text-zinc-600", "\u{203A}" }
                span { class: "text-zinc-500 ml-1", "Patch:" }
                span { class: "text-zinc-300 mr-2", "{name}" }
            }
            if let Some(ref name) = bc.preset {
                span { class: "text-zinc-600", "\u{203A}" }
                span { class: "text-zinc-500 ml-1", "Preset:" }
                span { class: "text-zinc-300 mr-2", "{name}" }
            }
            if let Some(ref name) = bc.scene {
                span { class: "text-zinc-600", "\u{203A}" }
                span { class: "text-zinc-500 ml-1", "Scene:" }
                span { class: "text-zinc-300", "{name}" }
            }
            if bc.song.is_none() && bc.preset.is_none() {
                span { class: "text-zinc-600 italic", "No selection" }
            }
        }
    }
}
