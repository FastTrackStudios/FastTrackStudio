//! Save handlers for capturing REAPER track/FX state as Signal presets.
//!
//! Each handler:
//! 1. Queries the selected track via `daw` services
//! 2. Auto-detects the track role from its name prefix
//! 3. Shows a `GetUserInputs` dialog with pre-filled defaults
//! 4. Captures the track/FX chunk and writes it with a `.signal.styx` sidecar
//!
//! All functions are designed to be called from `tokio::task::spawn_local`
//! inside action handlers (main thread, async context).

use daw::service::{FxChainContext, FxService, ProjectContext, TrackRef, TrackService, UiService};
use signal::plugin_block::{FxRole, TrackRole};
use signal::sidecar::{write_sidecar, PresetKind, SignalSidecar};
use signal::track_template::{self, Instrument, TemplateTier};
use tracing::{info, warn};

// ─── Shared helpers ──────────────────────────────────────────

/// Show a REAPER GetUserInputs dialog. Returns field values if OK, None if cancelled.
async fn show_save_dialog(title: &str, prompts: &[&str], defaults: &[&str]) -> Option<Vec<String>> {
    let ui = daw::reaper::ui::ReaperUi::new();
    let result = ui
        .get_user_inputs(
            title.to_string(),
            prompts.iter().map(|s| s.to_string()).collect(),
            defaults.iter().map(|s| s.to_string()).collect(),
        )
        .await?;

    if result.ok {
        Some(result.values)
    } else {
        None
    }
}

/// Get info for the first selected track. Returns (Track, TrackRef).
async fn selected_track() -> Option<(daw::Track, TrackRef)> {
    let track_svc = daw::reaper::ReaperTrack::new();
    let tracks = track_svc.get_selected_tracks(ProjectContext::Current).await;
    let track = tracks.into_iter().next()?;
    let guid = track.guid.clone();
    Some((track, TrackRef::Guid(guid)))
}

/// Get the full track chunk for a track.
async fn get_chunk(track_ref: &TrackRef) -> Option<String> {
    let track_svc = daw::reaper::ReaperTrack::new();
    track_svc
        .get_track_chunk(ProjectContext::Current, track_ref.clone())
        .await
        .ok()
}

/// Get the FX chain chunk text for a track's main FX chain.
async fn get_fx_chain_chunk(track_guid: &str) -> Option<String> {
    let fx_svc = daw::reaper::ReaperFx::new();
    fx_svc
        .get_fx_chain_chunk_text(
            ProjectContext::Current,
            FxChainContext::Track(track_guid.to_string()),
        )
        .await
}

/// Get all tracks in the project (for finding folder children).
async fn all_tracks() -> Vec<daw::Track> {
    let track_svc = daw::reaper::ReaperTrack::new();
    track_svc.get_tracks(ProjectContext::Current).await
}

/// Generate a UUID v4 string.
fn new_uuid() -> String {
    uuid::Uuid::new_v4().to_string()
}

/// Detect instrument from ExtState or default to Guitar.
fn detect_instrument() -> Instrument {
    let low = reaper_high::Reaper::get().medium_reaper().low();
    let section = c"FTS";
    let key = c"rig_type";
    let mut buf = [0i8; 128];
    let has_value = unsafe {
        low.GetExtState(section.as_ptr(), key.as_ptr());
        // GetExtState returns a pointer to static string; copy into buf
        let ptr = low.GetExtState(section.as_ptr(), key.as_ptr());
        if ptr.is_null() {
            false
        } else {
            let cstr = std::ffi::CStr::from_ptr(ptr);
            let bytes = cstr.to_bytes();
            if bytes.is_empty() {
                false
            } else {
                let len = bytes.len().min(buf.len() - 1);
                std::ptr::copy_nonoverlapping(bytes.as_ptr(), buf.as_mut_ptr() as *mut u8, len);
                buf[len] = 0;
                true
            }
        }
    };

    if has_value {
        let cstr = unsafe { std::ffi::CStr::from_ptr(buf.as_ptr()) };
        match cstr.to_string_lossy().to_lowercase().as_str() {
            "guitar" => Instrument::Guitar,
            "bass" => Instrument::Bass,
            "vocal" | "vocals" => Instrument::Vocal,
            "keys" | "keyboard" => Instrument::Keys,
            "drums" => Instrument::Drums,
            "drum-enhance" | "drum_enhance" => Instrument::DrumEnhance,
            _ => Instrument::Guitar,
        }
    } else {
        Instrument::Guitar
    }
}

/// Show a console message in REAPER.
fn console_msg(msg: &str) {
    reaper_high::Reaper::get().show_console_msg(format!("{msg}\n"));
}

use signal::strip_fxchain_wrapper;

// ─── Save Block ──────────────────────────────────────────────

/// Save the selected track's FX chain as a Block preset (.RfxChain).
///
/// Writes to `FXChains/FTS-Signal/01-Blocks/{block_type}/{preset_name}/{variation}.RfxChain`
/// with a `.signal.styx` sidecar.
pub async fn save_block() {
    let Some((track, _track_ref)) = selected_track().await else {
        console_msg("[Signal Save] No track selected");
        return;
    };

    // Get the FX chain chunk
    let Some(chunk) = get_fx_chain_chunk(&track.guid).await else {
        console_msg("[Signal Save] Could not capture FX chain");
        return;
    };

    // Try to infer block type from FX name
    let fx_svc = daw::reaper::ReaperFx::new();
    let fx_list = fx_svc
        .get_fx_list(
            ProjectContext::Current,
            FxChainContext::Track(track.guid.clone()),
        )
        .await;
    let first_fx_name = fx_list
        .first()
        .map(|fx| fx.name.clone())
        .unwrap_or_default();
    let role = FxRole::parse(&first_fx_name);
    let (default_name, default_type) = match &role {
        FxRole::Block { block_type, name } => (name.clone(), block_type.as_str().to_string()),
        FxRole::Module { name, .. } => (name.clone(), "module".to_string()),
        FxRole::GenericModule { name } => (name.clone(), "custom".to_string()),
        FxRole::Unknown { name } => (name.clone(), "custom".to_string()),
    };

    let Some(values) = show_save_dialog(
        "FTS: Save Block",
        &["Preset Name", "Variation", "Block Type"],
        &[&default_name, "Default", &default_type],
    )
    .await
    else {
        return; // User cancelled
    };

    let preset_name = &values[0];
    let variation = &values[1];
    let block_type = &values[2];

    // Build output path: FXChains/FTS-Signal/01-Blocks/{BlockType}/{PresetName}/{Variation}.RfxChain
    let root = signal::fxchains::fxchains_root();
    let block_type_folder = capitalize(block_type);
    let dir = root
        .join("01-Blocks")
        .join(&block_type_folder)
        .join(preset_name);
    if let Err(e) = std::fs::create_dir_all(&dir) {
        console_msg(&format!("[Signal Save] Failed to create directory: {e}"));
        return;
    }

    let file_path = dir.join(format!("{variation}.RfxChain"));

    // Write the FX chain content (strip wrapper for bare block)
    let content = strip_fxchain_wrapper(&chunk);
    if let Err(e) = std::fs::write(&file_path, &content) {
        console_msg(&format!("[Signal Save] Failed to write .RfxChain: {e}"));
        return;
    }

    // Write sidecar
    let sidecar = SignalSidecar {
        version: 1,
        id: new_uuid(),
        kind: PresetKind::Block {
            block_type: block_type.to_string(),
        },
        tags: vec![],
        description: None,
        parameters: vec![],
    };
    if let Err(e) = write_sidecar(&file_path, &sidecar) {
        warn!("Failed to write sidecar: {e}");
    }

    console_msg(&format!(
        "[Signal Save] Block saved: {file_path}",
        file_path = file_path.display()
    ));
    info!(
        "Saved block preset: {} / {} → {}",
        preset_name,
        variation,
        file_path.display()
    );
}

// ─── Save Module ─────────────────────────────────────────────

/// Save the selected track's FX chain as a Module preset (.RfxChain).
///
/// Writes to `FXChains/FTS-Signal/02-Modules/{preset_name}/{variation}.RfxChain`
/// with a `.signal.styx` sidecar.
pub async fn save_module() {
    let Some((track, _track_ref)) = selected_track().await else {
        console_msg("[Signal Save] No track selected");
        return;
    };

    let Some(chunk) = get_fx_chain_chunk(&track.guid).await else {
        console_msg("[Signal Save] Could not capture FX chain");
        return;
    };

    // Infer name from track name, stripping role prefix
    let default_name = match TrackRole::parse(&track.name) {
        Some(TrackRole::Layer { name }) => name,
        _ => track.name.clone(),
    };

    let Some(values) = show_save_dialog(
        "FTS: Save Module",
        &["Preset Name", "Variation"],
        &[&default_name, "Default"],
    )
    .await
    else {
        return;
    };

    let preset_name = &values[0];
    let variation = &values[1];

    let root = signal::fxchains::fxchains_root();
    let dir = root.join("02-Modules").join(preset_name);
    if let Err(e) = std::fs::create_dir_all(&dir) {
        console_msg(&format!("[Signal Save] Failed to create directory: {e}"));
        return;
    }

    let file_path = dir.join(format!("{variation}.RfxChain"));

    // Write the full FX chain (modules keep the chain wrapper)
    let content = strip_fxchain_wrapper(&chunk);
    if let Err(e) = std::fs::write(&file_path, &content) {
        console_msg(&format!("[Signal Save] Failed to write .RfxChain: {e}"));
        return;
    }

    let sidecar = SignalSidecar {
        version: 1,
        id: new_uuid(),
        kind: PresetKind::Module,
        tags: vec![],
        description: None,
        parameters: vec![],
    };
    if let Err(e) = write_sidecar(&file_path, &sidecar) {
        warn!("Failed to write sidecar: {e}");
    }

    console_msg(&format!(
        "[Signal Save] Module saved: {}",
        file_path.display()
    ));
    info!(
        "Saved module preset: {} / {} → {}",
        preset_name,
        variation,
        file_path.display()
    );
}

// ─── Save Layer ──────────────────────────────────────────────

/// Save the selected track as a Layer preset (.RTrackTemplate).
///
/// Uses `signal::track_template::save_track_template()` to write to
/// `TrackTemplates/FTS-Signal/{instrument}/01-Layers/{preset_name}/{variation}.RTrackTemplate`.
pub async fn save_layer() {
    let Some((track, track_ref)) = selected_track().await else {
        console_msg("[Signal Save] No track selected");
        return;
    };

    let Some(chunk) = get_chunk(&track_ref).await else {
        console_msg("[Signal Save] Could not capture track chunk");
        return;
    };

    let default_name = match TrackRole::parse(&track.name) {
        Some(TrackRole::Layer { name }) => name,
        _ => track.name.clone(),
    };

    let instrument = detect_instrument();
    let instrument_name = instrument.folder_name();

    let Some(values) = show_save_dialog(
        "FTS: Save Layer",
        &["Preset Name", "Variation", "Instrument"],
        &[&default_name, "Default", instrument_name],
    )
    .await
    else {
        return;
    };

    let preset_name = &values[0];
    let variation = &values[1];
    let instrument_str = &values[2];

    // Resolve instrument from user input
    let instrument = match instrument_str.to_lowercase().as_str() {
        "guitar" => Instrument::Guitar,
        "bass" => Instrument::Bass,
        "vocal" | "vocals" => Instrument::Vocal,
        "keys" | "keyboard" => Instrument::Keys,
        "drums" => Instrument::Drums,
        "drum-enhance" | "drum_enhance" => Instrument::DrumEnhance,
        _ => Instrument::Guitar,
    };

    match track_template::save_track_template(
        preset_name,
        variation,
        instrument,
        TemplateTier::Layer,
        &chunk,
        &new_uuid(),
        &[],
        None,
    ) {
        Ok(path) => {
            console_msg(&format!("[Signal Save] Layer saved: {}", path.display()));
            info!(
                "Saved layer preset: {} / {} → {}",
                preset_name,
                variation,
                path.display()
            );
        }
        Err(e) => {
            console_msg(&format!("[Signal Save] Failed to save layer: {e}"));
        }
    }
}

// ─── Save Rig ────────────────────────────────────────────────

/// Save the selected folder track + children as a Rig preset (.RTrackTemplate).
///
/// Concatenates track chunks from the folder track and all children into a single
/// `.RTrackTemplate` file.
pub async fn save_rig() {
    let Some((track, track_ref)) = selected_track().await else {
        console_msg("[Signal Save] No track selected");
        return;
    };

    if !track.is_folder {
        console_msg("[Signal Save] Selected track is not a folder — select a [R] rig folder");
        return;
    }

    // Collect folder track chunk
    let Some(folder_chunk) = get_chunk(&track_ref).await else {
        console_msg("[Signal Save] Could not capture folder track chunk");
        return;
    };

    // Collect child track chunks by walking folder_depth
    let tracks = all_tracks().await;
    let folder_idx = track.index as usize;
    let mut chunks = vec![folder_chunk];
    let mut depth: i32 = 1; // We're inside the folder

    for child in tracks.iter().skip(folder_idx + 1) {
        if depth <= 0 {
            break; // Exited the folder
        }
        if let Some(child_chunk) = get_chunk(&TrackRef::Guid(child.guid.clone())).await {
            chunks.push(child_chunk);
        }
        // folder_depth > 0 means this track opens a sub-folder,
        // folder_depth < 0 means it closes folder(s)
        depth += child.folder_depth;
    }

    let combined = chunks.join("\n");

    let default_name = match TrackRole::parse(&track.name) {
        Some(TrackRole::Rig { name }) => name,
        _ => track.name.clone(),
    };

    let instrument = detect_instrument();
    let instrument_name = instrument.folder_name();

    let Some(values) = show_save_dialog(
        "FTS: Save Rig",
        &["Preset Name", "Variation", "Instrument"],
        &[&default_name, "Default", instrument_name],
    )
    .await
    else {
        return;
    };

    let preset_name = &values[0];
    let variation = &values[1];
    let instrument_str = &values[2];

    let instrument = match instrument_str.to_lowercase().as_str() {
        "guitar" => Instrument::Guitar,
        "bass" => Instrument::Bass,
        "vocal" | "vocals" => Instrument::Vocal,
        "keys" | "keyboard" => Instrument::Keys,
        "drums" => Instrument::Drums,
        "drum-enhance" | "drum_enhance" => Instrument::DrumEnhance,
        _ => Instrument::Guitar,
    };

    match track_template::save_track_template(
        preset_name,
        variation,
        instrument,
        TemplateTier::Rig,
        &combined,
        &new_uuid(),
        &[],
        None,
    ) {
        Ok(path) => {
            console_msg(&format!(
                "[Signal Save] Rig saved ({} tracks): {}",
                chunks.len(),
                path.display()
            ));
            info!(
                "Saved rig preset: {} / {} ({} tracks) → {}",
                preset_name,
                variation,
                chunks.len(),
                path.display()
            );
        }
        Err(e) => {
            console_msg(&format!("[Signal Save] Failed to save rig: {e}"));
        }
    }
}

// ─── Load Profile ────────────────────────────────────────────

/// Load a profile by reading all `.RTrackTemplate` variations from a profile folder,
/// creating a folder track + one child track per variation, and applying chunks.
///
/// This is the fast path — no module resolution or signal pipeline. Just file reads
/// and REAPER chunk loading.
pub async fn load_profile() {
    let root = track_template::track_templates_root();

    // Scan for available profiles (folders in 04-Profiles/ under any instrument)
    let mut profiles: Vec<(String, String, std::path::PathBuf)> = Vec::new();
    if let Ok(instruments) = std::fs::read_dir(&root) {
        for inst_entry in instruments.flatten() {
            let inst_dir = inst_entry.path();
            if !inst_dir.is_dir() {
                continue;
            }
            let instrument = inst_dir
                .file_name()
                .unwrap_or_default()
                .to_string_lossy()
                .to_string();
            let profiles_dir = inst_dir.join("04-Profiles");
            if let Ok(entries) = std::fs::read_dir(&profiles_dir) {
                for entry in entries.flatten() {
                    let p = entry.path();
                    if p.is_dir() {
                        let name = p
                            .file_name()
                            .unwrap_or_default()
                            .to_string_lossy()
                            .to_string();
                        profiles.push((instrument.clone(), name, p));
                    }
                }
            }
        }
    }

    if profiles.is_empty() {
        console_msg("[Signal Load] No profiles found in TrackTemplates/FTS-Signal/");
        return;
    }

    // Build a list for the dialog
    let profile_names: Vec<String> = profiles
        .iter()
        .map(|(inst, name, _)| format!("{inst}/{name}"))
        .collect();

    let Some(values) = show_save_dialog(
        "FTS: Load Profile",
        &["Profile (e.g. Guitar/All-Around)"],
        &[&profile_names[0]],
    )
    .await
    else {
        return;
    };

    let chosen = &values[0];

    // Find the matching profile
    let Some((_instrument, profile_name, profile_dir)) = profiles
        .iter()
        .find(|(inst, name, _)| format!("{inst}/{name}") == *chosen)
    else {
        console_msg(&format!("[Signal Load] Profile not found: {chosen}"));
        return;
    };

    // Read all .RTrackTemplate files from the profile folder
    let mut variations: Vec<(String, String)> = Vec::new();
    if let Ok(entries) = std::fs::read_dir(profile_dir) {
        let mut paths: Vec<std::path::PathBuf> = entries
            .flatten()
            .map(|e| e.path())
            .filter(|p| {
                p.extension()
                    .map_or(false, |ext| ext.eq_ignore_ascii_case("rtracktemplate"))
            })
            .collect();
        paths.sort();

        for path in paths {
            let name = path
                .file_stem()
                .unwrap_or_default()
                .to_string_lossy()
                .to_string();
            match std::fs::read_to_string(&path) {
                Ok(chunk) => variations.push((name, chunk)),
                Err(e) => warn!("Failed to read {}: {e}", path.display()),
            }
        }
    }

    if variations.is_empty() {
        console_msg(&format!(
            "[Signal Load] No .RTrackTemplate files in {}",
            profile_dir.display()
        ));
        return;
    }

    console_msg(&format!(
        "[Signal Load] Loading profile '{}' ({} variations)...",
        profile_name,
        variations.len()
    ));

    let track_svc = daw::reaper::ReaperTrack::new();

    // Create a folder track for the profile
    let folder_guid = track_svc
        .add_track(ProjectContext::Current, format!("[R] {profile_name}"), None)
        .await;
    if folder_guid.is_empty() {
        console_msg("[Signal Load] Failed to create folder track");
        return;
    }

    // Set folder depth to +1 (start folder)
    track_svc
        .set_folder_depth(
            ProjectContext::Current,
            TrackRef::Guid(folder_guid.clone()),
            1,
        )
        .await;

    // Load each variation as a child track
    let mut loaded = 0usize;
    let total = variations.len();

    for (i, (name, chunk)) in variations.iter().enumerate() {
        let track_guid = track_svc
            .add_track(ProjectContext::Current, format!("[L] {name}"), None)
            .await;
        if track_guid.is_empty() {
            warn!("Failed to create track for variation '{name}'");
            continue;
        }

        // Apply the saved chunk (replaces entire track state including FX)
        match track_svc
            .set_track_chunk(
                ProjectContext::Current,
                TrackRef::Guid(track_guid.clone()),
                chunk.clone(),
            )
            .await
        {
            Ok(()) => {
                loaded += 1;
            }
            Err(e) => {
                warn!("Failed to load chunk for '{name}': {e}");
            }
        }

        // Close the folder on the last track
        if i == total - 1 {
            track_svc
                .set_folder_depth(ProjectContext::Current, TrackRef::Guid(track_guid), -1)
                .await;
        }
    }

    console_msg(&format!(
        "[Signal Load] Profile '{}' loaded: {}/{} variations",
        profile_name, loaded, total
    ));
    info!(
        "Loaded profile '{}': {}/{} variations from {}",
        profile_name,
        loaded,
        total,
        profile_dir.display()
    );
}

// ─── Utilities ───────────────────────────────────────────────

fn capitalize(s: &str) -> String {
    let mut c = s.chars();
    match c.next() {
        None => String::new(),
        Some(f) => f.to_uppercase().collect::<String>() + c.as_str(),
    }
}
