//! Project file import support for REAPER.
//!
//! Registers a `projectimport` handler so REAPER's File > Open dialog
//! can open supported external DAW project/session formats. The flow is:
//!
//! 1. REAPER calls `want_project_file()` to check if we handle the extension
//! 2. REAPER calls `enum_file_extensions()` to populate the file dialog filter
//! 3. REAPER calls `load_project()` with a `ProjectStateContext` to feed RPP lines into

use std::ffi::{CStr, CString};
use std::os::raw::{c_char, c_int};
use std::sync::OnceLock;

use dawfile_ableton::devices::BuiltinParams;
use dawfile_ableton::{Device, DeviceFormat};
use dawfile_reaper::RppSerialize;
use dawfile_reaper::builder::{ItemBuilder, ReaperProjectBuilder, TrackBuilder};
use reaper_medium::{OwnedProjectImportRegister, ReaperFunctionError, ReaperSession};

// ============================================================================
// Extension registry
// ============================================================================

/// File extensions we handle, with human-readable descriptions for the file dialog.
static EXTENSIONS: &[(&str, &str)] = &[
    ("als", "Ableton Live Set (*.als)"),
    ("ptx", "Pro Tools Session (*.ptx)"),
    ("ptf", "Pro Tools Session (*.ptf)"),
    ("pts", "Pro Tools Session (*.pts)"),
    ("aaf", "Advanced Authoring Format (*.aaf)"),
    ("dawproject", "DAWproject (*.dawproject)"),
];

/// Pre-computed CString pairs (extension, description), leaked for stable pointers.
static CACHED_EXTENSIONS: OnceLock<Vec<(CString, CString)>> = OnceLock::new();

fn cached_extensions() -> &'static Vec<(CString, CString)> {
    CACHED_EXTENSIONS.get_or_init(|| {
        EXTENSIONS
            .iter()
            .map(|(ext, desc)| (CString::new(*ext).unwrap(), CString::new(*desc).unwrap()))
            .collect()
    })
}

/// Register the shared DAW project importer with REAPER.
pub fn register_project_importer(session: &mut ReaperSession) -> Result<(), ReaperFunctionError> {
    let import_register =
        OwnedProjectImportRegister::new(want_project_file, enum_file_extensions, load_project);
    session.plugin_register_add_project_import(import_register)?;
    tracing::info!(
        extensions = EXTENSIONS
            .iter()
            .map(|(ext, _)| *ext)
            .collect::<Vec<_>>()
            .join(","),
        "Project import handler registered"
    );
    Ok(())
}

// ============================================================================
// C callbacks (registered with REAPER via projectimport)
// ============================================================================

/// Called by REAPER to check if we handle this file.
///
/// # Safety
///
/// `fn_` must be a valid null-terminated C string pointer from REAPER.
pub unsafe extern "C" fn want_project_file(fn_: *const c_char) -> bool {
    let path = unsafe { CStr::from_ptr(fn_) }.to_string_lossy();
    let lower = path.to_lowercase();
    EXTENSIONS
        .iter()
        .any(|(ext, _)| lower.ends_with(&format!(".{ext}")))
}

/// Called by REAPER to enumerate supported extensions for the file dialog.
///
/// REAPER calls with increasing `i` until we return null.
/// `descptr` is an output parameter for the description string.
///
/// # Safety
///
/// `descptr` must be a valid writable pointer (or null) from REAPER.
pub unsafe extern "C" fn enum_file_extensions(
    i: c_int,
    descptr: *mut *mut c_char,
) -> *const c_char {
    let cached = cached_extensions();

    let idx = i as usize;
    if idx >= cached.len() {
        return std::ptr::null();
    }

    if !descptr.is_null() {
        unsafe {
            *descptr = cached[idx].1.as_ptr() as *mut c_char;
        }
    }
    cached[idx].0.as_ptr()
}

/// Called by REAPER to load/import the project file.
///
/// Returns 0 on success, -1 on error.
///
/// # Safety
///
/// - `fn_` must be a valid null-terminated C string pointer.
/// - `genstate` must be a valid `ProjectStateContext` pointer from REAPER.
pub unsafe extern "C" fn load_project(
    fn_: *const c_char,
    genstate: *mut reaper_low::raw::ProjectStateContext,
) -> c_int {
    let path = unsafe { CStr::from_ptr(fn_) }.to_string_lossy();

    match import_file(&path, genstate) {
        Ok(()) => 0,
        Err(e) => {
            tracing::error!("Failed to import {}: {e}", path);
            -1
        }
    }
}

// ============================================================================
// Import dispatcher
// ============================================================================

fn import_file(
    path: &str,
    genstate: *mut reaper_low::raw::ProjectStateContext,
) -> Result<(), Box<dyn std::error::Error>> {
    let lower = path.to_lowercase();

    let rpp_text = if lower.ends_with(".als") {
        import_ableton(path)?
    } else if lower.ends_with(".ptx") || lower.ends_with(".ptf") || lower.ends_with(".pts") {
        import_protools(path)?
    } else if lower.ends_with(".aaf") {
        import_aaf(path)?
    } else if lower.ends_with(".dawproject") {
        import_dawproject(path)?
    } else {
        return Err(format!("Unsupported format: {path}").into());
    };

    // Feed RPP text to REAPER line by line
    emit_rpp_to_context(genstate, &rpp_text)?;
    Ok(())
}

// ============================================================================
// RPP → ProjectStateContext emitter
// ============================================================================

fn emit_rpp_to_context(
    genstate: *mut reaper_low::raw::ProjectStateContext,
    rpp_text: &str,
) -> Result<(), Box<dyn std::error::Error>> {
    for line in rpp_text.lines() {
        let c_line = CString::new(line)?;
        unsafe {
            let ctx = &mut *genstate;
            ctx.AddLine(c_line.as_ptr());
        }
    }
    Ok(())
}

// ============================================================================
// Ableton → REAPER conversion
// ============================================================================

fn import_ableton(path: &str) -> Result<String, Box<dyn std::error::Error>> {
    let set = dawfile_ableton::read_live_set(path)?;

    let mut builder = ReaperProjectBuilder::new();
    builder = builder.tempo_with_time_sig(
        set.tempo,
        set.time_signature.numerator as i32,
        set.time_signature.denominator as i32,
    );

    // Tempo automation
    if !set.tempo_automation.is_empty() {
        builder = builder.tempo_envelope(|env| {
            let mut env = env;
            for point in &set.tempo_automation {
                env = env.point(point.time, point.value);
            }
            env
        });
    }

    // Markers (locators)
    for (i, loc) in set.locators.iter().enumerate() {
        builder = builder.marker(i as i32 + 1, loc.time, &loc.name);
    }

    // Audio tracks
    for track in &set.audio_tracks {
        builder = builder.track(&track.common.effective_name, |t| {
            build_audio_track(t, track)
        });
    }

    // MIDI tracks
    for track in &set.midi_tracks {
        builder = builder.track(&track.common.effective_name, |t| build_midi_track(t, track));
    }

    // Return tracks → REAPER tracks
    for track in &set.return_tracks {
        let name = format!("[Return] {}", track.common.effective_name);
        builder = builder.track(&name, |t| apply_common_track_props(t, &track.common));
    }

    // Group tracks → REAPER folder tracks
    for track in &set.group_tracks {
        let name = format!("[Group] {}", track.common.effective_name);
        builder = builder.track(&name, |t| {
            apply_common_track_props(t, &track.common).folder_start()
        });
    }

    let project = builder.build();
    Ok(project.to_rpp_string())
}

// ── Track builders ─────────────────────────────────────────────────────────

/// Synthesize a deterministic REAPER-style GUID from a stable input string.
///
/// REAPER's `TRACKID` is canonically `{XXXXXXXX-XXXX-XXXX-XXXX-XXXXXXXXXXXX}`.
/// We derive 16 bytes via SHA-256 of the input and format them as a v4-shaped
/// UUID so re-running the conversion on the same PT session produces the
/// same GUID for the same track (matters for round-trip identity and for
/// tools that diff converter output).
fn deterministic_guid(seed: &str) -> String {
    use std::collections::hash_map::DefaultHasher;
    use std::hash::{Hash, Hasher};
    // Two 64-bit hashes of the seed give us 128 bits of GUID material.
    // We don't need cryptographic strength here — just stability + low
    // collision risk across at most a few hundred tracks per session.
    let mut h1 = DefaultHasher::new();
    seed.hash(&mut h1);
    let lo = h1.finish();
    let mut h2 = DefaultHasher::new();
    "salt:".hash(&mut h2);
    seed.hash(&mut h2);
    let hi = h2.finish();
    format!(
        "{{{:08X}-{:04X}-{:04X}-{:04X}-{:012X}}}",
        (hi >> 32) as u32,
        (hi >> 16) as u16,
        // Variant + version bits zeroed to keep this trivially distinguishable
        // from a true v4 UUID — REAPER doesn't care, but it makes the source
        // of the GUID self-documenting.
        hi as u16,
        (lo >> 48) as u16,
        lo & 0x0000_FFFF_FFFF_FFFFu64,
    )
}

fn apply_common_track_props(
    t: TrackBuilder,
    common: &dawfile_ableton::TrackCommon,
) -> TrackBuilder {
    let mut t = t.volume(common.mixer.volume).pan(common.mixer.pan);

    if common.color != 0 {
        t = t.color(ableton_color_to_rgb(common.color));
    }

    if !common.mixer.speaker_on {
        t = t.muted();
    }

    t
}

fn build_audio_track(t: TrackBuilder, track: &dawfile_ableton::AudioTrack) -> TrackBuilder {
    let mut t = apply_common_track_props(t, &track.common);

    // Arrangement clips → items
    for clip in &track.arrangement_clips {
        let position = clip.common.time;
        let length = clip.common.current_end - clip.common.current_start;

        if length <= 0.0 {
            continue;
        }

        t = t.item(position, length, |item| build_audio_item(item, clip));
    }

    // FX / Devices
    for device in &track.common.devices {
        t = add_device_to_track(t, device);
    }

    t
}

fn build_audio_item(item: ItemBuilder, clip: &dawfile_ableton::AudioClip) -> ItemBuilder {
    let mut item = item.name(&clip.common.name);

    // Audio source
    if let Some(ref sr) = clip.sample_ref
        && let Some(ref path) = sr.path
    {
        item = item.source_wave(path.to_string_lossy().into_owned());
    }

    // Pitch
    if clip.pitch_coarse != 0.0 {
        item = item.pitch(clip.pitch_coarse);
    }

    // Fades
    if let Some(ref fades) = clip.fades {
        if fades.fade_in_length > 0.0 {
            item = item.fade_in(
                fades.fade_in_length,
                dawfile_reaper::types::item::FadeCurveType::Linear,
            );
        }
        if fades.fade_out_length > 0.0 {
            item = item.fade_out(
                fades.fade_out_length,
                dawfile_reaper::types::item::FadeCurveType::Linear,
            );
        }
    }

    item
}

fn build_midi_track(t: TrackBuilder, track: &dawfile_ableton::MidiTrack) -> TrackBuilder {
    let mut t = apply_common_track_props(t, &track.common);

    // Arrangement MIDI clips
    for clip in &track.arrangement_clips {
        let position = clip.common.time;
        let length = clip.common.current_end - clip.common.current_start;

        if length <= 0.0 {
            continue;
        }

        t = t.item(position, length, |item| build_midi_item(item, clip));
    }

    // FX / Devices
    for device in &track.common.devices {
        t = add_device_to_track(t, device);
    }

    t
}

fn build_midi_item(item: ItemBuilder, clip: &dawfile_ableton::MidiClip) -> ItemBuilder {
    let item = item.name(&clip.common.name);

    // Build MIDI source from key tracks
    item.source_midi().midi(|midi| {
        let mut midi = midi;
        for kt in &clip.key_tracks {
            for note in &kt.notes {
                if !note.is_enabled {
                    continue;
                }
                // Convert beat-relative time to ticks (960 tpqn default)
                let tick_pos = (note.time * 960.0) as u64;
                let tick_dur = (note.duration * 960.0) as u64;
                midi = midi
                    .at(tick_pos)
                    .note(0, 0, kt.midi_key, note.velocity, tick_dur as u32);
            }
        }
        midi
    })
}

// ── Device / FX mapping ────────────────────────────────────────────────────

fn add_device_to_track(mut t: TrackBuilder, device: &Device) -> TrackBuilder {
    if !device.is_on {
        return t; // Skip disabled devices
    }

    match device.format {
        DeviceFormat::Vst2 => {
            t = t.vst(&device.name, "");
        }
        DeviceFormat::Vst3 => {
            t = t.vst3(&device.name, "");
        }
        DeviceFormat::AudioUnit => {
            // REAPER on Linux doesn't support AU, skip
        }
        DeviceFormat::Builtin => {
            if let Some(ref params) = device.builtin_params {
                t = map_builtin_fx(t, params);
            }
        }
        _ => {} // MaxForLive, NoteAlgorithm, Unknown — skip
    }

    t
}

fn map_builtin_fx(mut t: TrackBuilder, params: &BuiltinParams) -> TrackBuilder {
    match params {
        BuiltinParams::Eq8(_) => {
            t = t.js("ReaEQ");
        }
        BuiltinParams::Compressor(_) | BuiltinParams::GlueCompressor(_) => {
            t = t.js("ReaComp");
        }
        BuiltinParams::Gate(_) => {
            t = t.js("ReaGate");
        }
        BuiltinParams::Limiter(_) => {
            t = t.js("ReaLimit");
        }
        BuiltinParams::Reverb(_) => {
            t = t.js("ReaVerbate");
        }
        BuiltinParams::Delay(_) | BuiltinParams::Echo(_) => {
            t = t.js("ReaDelay");
        }
        BuiltinParams::Utility(_) => {
            // Utility is just gain/phase/width — often not needed as separate FX
        }
        _ => {
            tracing::debug!("Unmapped Ableton built-in device, skipping");
        }
    }
    t
}

// ── Color mapping ──────────────────────────────────────────────────────────

// ============================================================================
// Pro Tools → REAPER conversion
// ============================================================================

/// Convert a Pro Tools session at `path` to a REAPER `.rpp` string.
///
/// Public so that command-line tools and tests can run the conversion without
/// going through the REAPER plugin entry point.
pub fn protools_to_rpp(path: &str) -> Result<String, Box<dyn std::error::Error>> {
    import_protools(path)
}

fn import_protools(path: &str) -> Result<String, Box<dyn std::error::Error>> {
    let session = dawfile_protools::read_session(path, 48000)?;
    let sample_rate = session.session_sample_rate as f64;

    // Pro Tools stores audio files in `Audio Files/` next to the .ptx.
    // Resolve a bare filename ("kick.wav") to the on-disk absolute path so
    // REAPER items point at real audio instead of dangling-source stubs.
    let session_dir = std::path::Path::new(path)
        .parent()
        .map(|p| p.to_path_buf())
        .unwrap_or_else(|| std::path::PathBuf::from("."));
    let audio_files_dir = session_dir.join("Audio Files");
    let resolve_audio_path = |filename: &str| -> String {
        if filename.is_empty() {
            return String::new();
        }
        // Already absolute → use as-is.
        let p = std::path::Path::new(filename);
        if p.is_absolute() {
            return filename.to_string();
        }
        // Try Audio Files/ subdir first (the standard layout).
        let in_audio = audio_files_dir.join(filename);
        if in_audio.exists() {
            return in_audio.to_string_lossy().into_owned();
        }
        // Fall back to session dir.
        let in_session = session_dir.join(filename);
        if in_session.exists() {
            return in_session.to_string_lossy().into_owned();
        }
        // Last resort: emit the Audio Files/ path even if the file is
        // missing — REAPER will show a "missing media" item rather than
        // an empty one, and the user can relink.
        in_audio.to_string_lossy().into_owned()
    };

    let mut builder = ReaperProjectBuilder::new();
    builder = builder.sample_rate(session.session_sample_rate as i32);

    // Initial tempo + time signature
    //
    // Pro Tools stores BPM relative to the click note value (encoded in
    // `ticks_per_beat`: 960000 = quarter, 480000 = eighth, 1440000 = dotted-quarter, etc.).
    // REAPER always interprets tempo as quarter-note BPM regardless of time signature.
    // Convert: reaper_bpm = pt_bpm × (pt_tpb / 960000).
    fn pt_bpm_to_reaper(pt_bpm: f64, pt_tpb: u64) -> f64 {
        if pt_tpb == 0 {
            pt_bpm
        } else {
            pt_bpm * (pt_tpb as f64 / 960_000.0)
        }
    }
    let init_tpb = session
        .tempo_events
        .first()
        .map(|t| t.ticks_per_beat)
        .unwrap_or(960_000);
    let init_bpm = pt_bpm_to_reaper(session.bpm, init_tpb);
    let (init_num, init_den) = session
        .meter_events
        .first()
        .map(|m| (m.numerator as i32, m.denominator as i32))
        .unwrap_or((4, 4));
    builder = builder.tempo_with_time_sig(init_bpm, init_num, init_den);

    // Tempo / meter envelope — emit only if there's more than one change.
    let has_tempo_changes = session.tempo_events.len() > 1;
    let has_meter_changes = session.meter_events.len() > 1;
    if has_tempo_changes || has_meter_changes {
        // Merge tempo + meter events on a single timeline keyed by sample position.
        // At each point we know the active (bpm, num, den); emit one envelope point.
        let mut positions: Vec<u64> = session
            .tempo_events
            .iter()
            .map(|t| t.sample_start)
            .chain(session.meter_events.iter().map(|m| m.sample_start))
            .collect();
        positions.sort_unstable();
        positions.dedup();

        builder = builder.tempo_envelope(|mut env| {
            let mut cur_bpm = init_bpm;
            let mut cur_num = init_num;
            let mut cur_den = init_den;
            for sample_pos in positions {
                if let Some(t) = session
                    .tempo_events
                    .iter()
                    .find(|t| t.sample_start == sample_pos)
                {
                    cur_bpm = pt_bpm_to_reaper(t.bpm, t.ticks_per_beat);
                }
                let meter_changed = session
                    .meter_events
                    .iter()
                    .find(|m| m.sample_start == sample_pos)
                    .map(|m| {
                        cur_num = m.numerator as i32;
                        cur_den = m.denominator as i32;
                    })
                    .is_some();
                let pos_secs = sample_pos as f64 / sample_rate;
                env = if meter_changed {
                    env.point_with_time_sig(pos_secs, cur_bpm, cur_num, cur_den)
                } else {
                    env.point(pos_secs, cur_bpm)
                };
            }
            env
        });
    }

    // Markers (Pro Tools Memory Locations)
    for marker in &session.markers {
        let pos_secs = marker.sample_pos as f64 / sample_rate;
        builder = builder.marker(marker.number as i32, pos_secs, &marker.name);
    }

    // Build the unified track-emission order from PT's 0x251a display order
    // (`Track.display_order`). PT interleaves audio, MIDI, master and
    // folder/divider tracks in one on-screen list; emitting audio-then-MIDI
    // (or sorting by channel index) scrambles that. We assemble one merged
    // list — applying stereo-sibling dedup to the audio side first — then
    // stable-sort it by display order so the REAPER layout matches the source.
    #[derive(Clone, Copy)]
    enum Src {
        Audio,
        Midi,
    }
    struct Emit {
        src: Src,
        idx: usize,
        is_stereo: bool,
    }
    let mut emit: Vec<Emit> = Vec::new();
    // Pro Tools stores stereo tracks as TWO consecutive entries (one per
    // channel) sharing the same `name`, referencing the same stereo source via
    // `.L` / `.R` region siblings. Skip the R-channel sibling so we emit ONE
    // REAPER track per stereo pair.
    let mut i = 0;
    while i < session.audio_tracks.len() {
        let is_stereo = session
            .audio_tracks
            .get(i + 1)
            .map(|next| next.name == session.audio_tracks[i].name)
            .unwrap_or(false);
        emit.push(Emit {
            src: Src::Audio,
            idx: i,
            is_stereo,
        });
        i += if is_stereo { 2 } else { 1 };
    }
    for j in 0..session.midi_tracks.len() {
        emit.push(Emit {
            src: Src::Midi,
            idx: j,
            is_stereo: false,
        });
    }
    // Stable sort by PT display order. Tracks that didn't match a 0x251a entry
    // carry `u32::MAX` and sink to the bottom, keeping their relative order.
    emit.sort_by_key(|e| match e.src {
        Src::Audio => session.audio_tracks[e.idx].display_order,
        Src::Midi => session.midi_tracks[e.idx].display_order,
    });

    // Compute REAPER folder open/close from PT's reconstructed `folder_depth`
    // (0 = top level) over the merged emission order. PT's per-track depth and
    // folder-parent flag come from the `0x210b`/`0x210c` group decode in
    // dawfile-protools (see `Track::folder_depth` / `Track::is_folder`).
    //
    // REAPER models folders implicitly: a folder PARENT carries `ISBUS 1 1`
    // (handled below via `folder_start()`), and the LAST track in a folder
    // carries `ISBUS 2 -N` to pop N levels (`folder_end(N)`). We walk the
    // emission order tracking the current depth; whenever the NEXT track is
    // shallower (or we hit the end) the current track closes the difference.
    //
    // Folder parents are depth `d` and their children depth `d+1`; a leaf at
    // depth `d` that is the last before a return to depth `d-k` closes `k`
    // levels. Sessions with no `0x210c` groups have all-zero depth → no
    // folders, matching the official converter's flattening of PT "divider"
    // tracks.
    let depths: Vec<u32> = emit
        .iter()
        .map(|e| match e.src {
            Src::Audio => session.audio_tracks[e.idx].folder_depth,
            Src::Midi => session.midi_tracks[e.idx].folder_depth,
        })
        .collect();
    let folder_end_levels: Vec<i32> = {
        let n = depths.len();
        let mut ends = vec![0i32; n];
        for i in 0..n {
            let next = depths.get(i + 1).copied().unwrap_or(0);
            if depths[i] > next {
                ends[i] = (depths[i] - next) as i32;
            }
        }
        ends
    };
    // A track OPENS a REAPER folder only when the following track is deeper.
    // We drive folder-open off the reconstructed depth — NOT the raw
    // `is_folder` role byte — so PT "divider" folder-parents that have no
    // children (empty `0x210c`, e.g. the ALL THAT I AM session) stay flat,
    // matching the official converter instead of opening a folder that never
    // closes and swallows every later track.
    let folder_starts: Vec<bool> = (0..depths.len())
        .map(|i| depths.get(i + 1).copied().unwrap_or(0) > depths[i])
        .collect();

    // PT track comments are emitted as SWS/S&M Track Notes (a project-level
    // <EXTENSIONS> block keyed by track GUID), matching the official converter.
    // Collected here and appended after the project is serialized.
    let mut track_notes: Vec<(String, String)> = Vec::new();

    for (plan_idx, e) in emit.iter().enumerate() {
        let end_levels = folder_end_levels[plan_idx];
        let is_folder_start = folder_starts[plan_idx];
        match e.src {
            Src::Audio => {
                let track = &session.audio_tracks[e.idx];
                let is_stereo = e.is_stereo;
                // REAPER track names must be clean; the PT output assignment
                // (`track.output`, e.g. "Analog 1-2"/"Bus 1") is routing, not part of
                // the name. Routing-to-bus emission is a separate follow-on.
                let display_name = track.name.clone();
                let track_guid = deterministic_guid(&format!("pt:{}:{}", plan_idx, track.name));
                if !track.comment.is_empty() {
                    track_notes.push((track_guid.clone(), track.comment.clone()));
                }
                builder = builder.track(&display_name, |t| {
                    let mut t = t
                        .guid(&track_guid)
                        .automation_mode(dawfile_reaper::types::track::AutomationMode::Read);
                    if is_folder_start {
                        t = t.folder_start();
                    }
                    // Apply per-track mix state from PT's 0x1029 block.
                    // Volume is in 0.1 dB units (centibel); ≤ -1440 means -∞.
                    // Pan is -100..+100. For STEREO PT tracks the stored "pan" is the
                    // left-channel pan (defaults to -100 = full-left, the natural
                    // state), NOT a balance adjustment. REAPER's default stereo-pan
                    // mode would interpret pan = -1 as "mute the right channel", so
                    // for stereo we emit pan = 0 (centered balance) regardless.
                    let linear_vol = if track.volume_centibel <= -1440 {
                        0.0
                    } else {
                        10f64.powf(track.volume_centibel as f64 / 200.0)
                    };
                    let reaper_pan = if is_stereo {
                        0.0
                    } else {
                        (track.pan as f64 / 100.0).clamp(-1.0, 1.0)
                    };
                    let mut t = t.volume(linear_vol).pan(reaper_pan);
                    // Mute via `0x1029 +5`. Accurate for the LORD family stems +
                    // ClickPrint + Inst stack; over-reports on SYZ / AC GTR / El
                    // Gtr / Bass Demo where the byte is set but PT shows audible.
                    // See `lord_of_the_fight_mute_pattern` test for the diff.
                    if track.mute {
                        t = t.muted();
                    }
                    if let Some(rgb) = pt_color_to_rgb(track.color_byte) {
                        t = t.color(rgb);
                    }
                    for tr in &track.regions {
                        if tr.region_index as usize >= session.audio_regions.len() {
                            continue;
                        }
                        let region = &session.audio_regions[tr.region_index as usize];

                        let position_secs = tr.start_pos as f64 / sample_rate;
                        let length_secs = region.length as f64 / sample_rate;
                        let offset_secs = region.sample_offset as f64 / sample_rate;

                        if length_secs <= 0.0 {
                            continue;
                        }

                        // Get source filename and resolve to absolute on-disk path.
                        //
                        // The region's `audio_file_index` from the parser is currently
                        // unreliable (PT12 stores file ↔ region mapping in a block we
                        // don't decode yet). Fall back to matching the region NAME
                        // against the audio file stems: PT auto-generates region names
                        // like "02 LORD OF THE FIGHT (Vocals)_1-03.L" where the part
                        // before the trailing "-NN.L/R" is the source filename stem.
                        fn region_to_file_stem(region_name: &str) -> &str {
                            // Strip optional ".L"/".R" channel suffix.
                            let without_chan = region_name
                                .strip_suffix(".L")
                                .or_else(|| region_name.strip_suffix(".R"))
                                .unwrap_or(region_name);
                            // Strip optional "-NN" sub-region suffix.
                            if let Some(idx) = without_chan.rfind('-') {
                                let suffix = &without_chan[idx + 1..];
                                if !suffix.is_empty() && suffix.chars().all(|c| c.is_ascii_digit())
                                {
                                    return &without_chan[..idx];
                                }
                            }
                            without_chan
                        }
                        let stem = region_to_file_stem(&region.name);
                        let filename = session
                            .audio_files
                            .iter()
                            // Exact match first.
                            .find(|f| {
                                f.filename
                                    .strip_suffix(".wav")
                                    .or_else(|| f.filename.strip_suffix(".WAV"))
                                    .map(|s| s == stem)
                                    .unwrap_or(false)
                            })
                            // Then try a starts-with (handles ".dup1" / ".01" tail).
                            .or_else(|| {
                                session.audio_files.iter().find(|f| {
                                    f.filename
                                        .strip_suffix(".wav")
                                        .or_else(|| f.filename.strip_suffix(".WAV"))
                                        .map(|s| stem.starts_with(s) || s.starts_with(stem))
                                        .unwrap_or(false)
                                })
                            })
                            .map(|f| f.filename.as_str())
                            .unwrap_or("");
                        let absolute_path = resolve_audio_path(filename);

                        // Match fade entries to this item by timeline position.
                        // PT 0x262f fade defs carry both an in-length and an out-length:
                        //   - `out_length == 0` ⇒ pure fade-in starting at the item start
                        //   - `in_length == 0` ⇒ pure fade-out ending at the item end
                        //   - both non-zero ⇒ crossfade. The fade entry's start_pos is
                        //     the START of the in-fade (= end of the previous item).
                        //     For the LEADING item of the crossfade pair, we want the
                        //     fade-out (= the matching `in_length` of the crossfade
                        //     "into" the next item). For the TRAILING item, we want
                        //     the fade-in (= the same in_length).
                        let item_start = tr.start_pos;
                        let item_end = item_start.saturating_add(region.length);
                        let mut fade_in_secs: Option<f64> = None;
                        let mut fade_out_secs: Option<f64> = None;
                        let tolerance: u64 = (sample_rate as u64) / 1000; // ±1 ms
                        for f in &track.fades {
                            let crossfade = f.in_length > 0 && f.out_length > 0;
                            let fade_total = f.in_length.max(f.out_length);
                            if fade_total == 0 {
                                continue;
                            }
                            // Fade-IN: the fade's start coincides with this item's start.
                            if f.start_pos.abs_diff(item_start) <= tolerance && f.in_length > 0 {
                                fade_in_secs = Some(f.in_length as f64 / sample_rate);
                            }
                            // Fade-OUT: the fade ends at this item's end. For pure
                            // fade-outs (`in == 0`) PT places start_pos = item_end - out_length.
                            // For crossfades, start_pos sits at the boundary where the
                            // out-fading item ends, so item_end ≈ start_pos.
                            let f_end = f.start_pos.saturating_add(fade_total);
                            if (crossfade && f.start_pos.abs_diff(item_end) <= tolerance)
                                || (f.out_length > 0 && f_end.abs_diff(item_end) <= tolerance)
                            {
                                let out_len = if f.out_length > 0 {
                                    f.out_length
                                } else {
                                    f.in_length
                                };
                                fade_out_secs = Some(out_len as f64 / sample_rate);
                            }
                        }

                        t = t.item(position_secs, length_secs, |item| {
                            // Set the TAKE name (not the item name) to the region
                            // name so REAPER displays a single readable label rather
                            // than stacking item-level + take-level labels.
                            let mut item = item;
                            if !absolute_path.is_empty() {
                                item = item.source_wave(&absolute_path).take_name(&region.name);
                            } else {
                                item = item.name(&region.name);
                            }
                            if offset_secs > 0.0 {
                                item = item.slip_offset(offset_secs);
                            }
                            if let Some(fi) = fade_in_secs {
                                item = item.fade_in(
                                    fi,
                                    dawfile_reaper::types::item::FadeCurveType::Linear,
                                );
                            }
                            if let Some(fo) = fade_out_secs {
                                item = item.fade_out(
                                    fo,
                                    dawfile_reaper::types::item::FadeCurveType::Linear,
                                );
                            }
                            item
                        });
                    }
                    if end_levels > 0 {
                        t = t.folder_end(end_levels);
                    }
                    t
                });
            }
            Src::Midi => {
                let track = &session.midi_tracks[e.idx];
                // REAPER track names must be clean; the PT output assignment
                // (`track.output`, e.g. "Analog 1-2"/"Bus 1") is routing, not part of
                // the name. Routing-to-bus emission is a separate follow-on.
                let display_name = track.name.clone();
                let track_guid = deterministic_guid(&format!("pt:{}:{}", plan_idx, track.name));
                if !track.comment.is_empty() {
                    track_notes.push((track_guid.clone(), track.comment.clone()));
                }
                builder = builder.track(&display_name, |t| {
                    let mut t = t
                        .guid(&track_guid)
                        .automation_mode(dawfile_reaper::types::track::AutomationMode::Read);
                    if is_folder_start {
                        t = t.folder_start();
                    }
                    let linear_vol = if track.volume_centibel <= -1440 {
                        0.0
                    } else {
                        10f64.powf(track.volume_centibel as f64 / 200.0)
                    };
                    let reaper_pan = (track.pan as f64 / 100.0).clamp(-1.0, 1.0);
                    let mut t = t.volume(linear_vol).pan(reaper_pan);
                    // Mute via `0x1029 +5`. Accurate for the LORD family stems +
                    // ClickPrint + Inst stack; over-reports on SYZ / AC GTR / El
                    // Gtr / Bass Demo where the byte is set but PT shows audible.
                    // See `lord_of_the_fight_mute_pattern` test for the diff.
                    if track.mute {
                        t = t.muted();
                    }
                    if let Some(rgb) = pt_color_to_rgb(track.color_byte) {
                        t = t.color(rgb);
                    }
                    for tr in &track.regions {
                        if tr.region_index as usize >= session.midi_regions.len() {
                            continue;
                        }
                        let region = &session.midi_regions[tr.region_index as usize];

                        let position_secs = tr.start_pos as f64 / sample_rate;
                        let length_secs = region.length as f64 / sample_rate;

                        if length_secs <= 0.0 || region.events.is_empty() {
                            continue;
                        }

                        t = t.item(position_secs, length_secs, |item| {
                            // `.midi(...)` adds the MIDI take; do NOT also call
                            // `.source_midi()` — that would push an empty MIDI take
                            // first, leaving the real data on take #1 (and REAPER
                            // would render the item as silent take #0).
                            item.name(&region.name).midi(|midi| {
                                // Pro Tools stores MIDI at 960,000 ticks/quarter; tell
                                // the REAPER builder to use the same PPQN so positions
                                // and durations don't need numeric rescaling.
                                let mut midi = midi.ticks_per_qn(960_000);

                                // Many PT regions store duration=0 for every event
                                // (a paired note-on/note-off encoding the parser
                                // doesn't fully reconstruct yet). For those events,
                                // fall back to "until the next note of the same
                                // pitch" so notes sustain naturally instead of
                                // truncating to silence.
                                const FALLBACK_DUR: u64 = 480_000; // half a quarter
                                let region_end = region.length.max(FALLBACK_DUR);

                                for (i, event) in region.events.iter().enumerate() {
                                    if event.velocity == 0 {
                                        continue;
                                    }
                                    let dur = if event.duration > 0 {
                                        event.duration as u32
                                    } else {
                                        // Find the next note of the SAME pitch and
                                        // end this one just before it. Failing that,
                                        // use the region end, capped at one quarter.
                                        let next = region.events[i + 1..]
                                            .iter()
                                            .find(|e| e.note == event.note)
                                            .map(|e| e.position);
                                        let end = next.unwrap_or(region_end);
                                        let span = end.saturating_sub(event.position);
                                        span.clamp(FALLBACK_DUR, 960_000) as u32
                                    };
                                    midi = midi.at(event.position).note(
                                        0,
                                        0,
                                        event.note,
                                        event.velocity,
                                        dur,
                                    );
                                }
                                midi
                            })
                        });
                    }
                    if end_levels > 0 {
                        t = t.folder_end(end_levels);
                    }
                    t
                });
            }
        }
    }

    let project = builder.build();
    let mut rpp = project.to_rpp_string();

    // Append the track comments as an <EXTENSIONS> block of <S&M_TRACKNOTES>
    // entries (the SWS Track Notes format the official converter uses),
    // inserted as the last child before the project's closing `>`. Each note
    // is keyed by the track's GUID; comment lines are emitted verbatim with a
    // leading `|`.
    if !track_notes.is_empty() {
        let mut ext = String::from("  <EXTENSIONS\n");
        for (guid, comment) in &track_notes {
            ext.push_str(&format!("    <S&M_TRACKNOTES {guid}\n"));
            for line in comment.split('\n') {
                ext.push_str(&format!("      |{line}\n"));
            }
            ext.push_str("    >\n");
        }
        ext.push_str("  >\n");
        if let Some(pos) = rpp.rfind("\n>") {
            rpp.insert_str(pos + 1, &ext);
        } else {
            rpp.push_str(&ext);
        }
    }

    Ok(rpp)
}

// ============================================================================
// AAF → REAPER conversion
// ============================================================================

fn import_aaf(path: &str) -> Result<String, Box<dyn std::error::Error>> {
    let session = dawfile_aaf::read_session(path)?;

    let mut builder = ReaperProjectBuilder::new();
    builder = builder.sample_rate(session.session_sample_rate as i32);

    // Markers
    for (i, marker) in session.markers.iter().enumerate() {
        let pos_secs = marker.edit_rate.to_seconds(marker.position);
        builder = builder.marker(i as i32 + 1, pos_secs, &marker.comment);
    }

    // Tracks
    for track in &session.tracks {
        // Skip non-audio/midi tracks
        match track.kind {
            dawfile_aaf::TrackKind::Audio | dawfile_aaf::TrackKind::Midi => {}
            _ => continue,
        }

        builder = builder.track(&track.name, |t| {
            let mut t = t;
            for clip in &track.clips {
                let position_secs = track.edit_rate.to_seconds(clip.start_position);
                let length_secs = track.edit_rate.to_seconds(clip.length);

                if length_secs <= 0.0 {
                    continue;
                }

                match &clip.kind {
                    dawfile_aaf::ClipKind::SourceClip {
                        source_file,
                        source_start,
                        ..
                    } => {
                        t = t.item(position_secs, length_secs, |item| {
                            let mut item = item;

                            // Strip file:/// prefix from source path
                            if let Some(path_url) = source_file {
                                let file_path = path_url
                                    .strip_prefix("file:///")
                                    .or_else(|| path_url.strip_prefix("file://"))
                                    .unwrap_or(path_url);
                                item = item.source_wave(file_path);
                            }

                            // Source offset
                            let offset_secs = track.edit_rate.to_seconds(*source_start);
                            if offset_secs > 0.0 {
                                item = item.slip_offset(offset_secs);
                            }

                            item
                        });
                    }
                    dawfile_aaf::ClipKind::Filler => {
                        // Silence gap — skip
                    }
                    _ => {
                        // Transitions, operations — skip for now
                    }
                }
            }
            t
        });
    }

    let project = builder.build();
    Ok(project.to_rpp_string())
}

// ============================================================================
// DAWproject → REAPER conversion
// ============================================================================

fn import_dawproject(path: &str) -> Result<String, Box<dyn std::error::Error>> {
    let project = dawfile_dawproject::read_project(path)?;

    let mut builder = ReaperProjectBuilder::new();
    builder = builder.tempo_with_time_sig(
        project.transport.tempo,
        project.transport.numerator as i32,
        project.transport.denominator as i32,
    );

    // Markers and tempo automation from arrangement
    if let Some(ref arr) = project.arrangement {
        for (i, marker) in arr.markers.iter().enumerate() {
            builder = builder.marker(i as i32 + 1, marker.time, &marker.name);
        }

        // Tempo automation
        if !arr.tempo_automation.is_empty() {
            builder = builder.tempo_envelope(|env| {
                let mut env = env;
                for tp in &arr.tempo_automation {
                    env = env.point(tp.time, tp.bpm);
                }
                env
            });
        }
    }

    // Build tracks
    add_dawproject_tracks(&mut builder, &project.tracks, project.arrangement.as_ref());

    let rpp_project = builder.build();
    Ok(rpp_project.to_rpp_string())
}

fn add_dawproject_tracks(
    builder: &mut ReaperProjectBuilder,
    tracks: &[dawfile_dawproject::Track],
    arrangement: Option<&dawfile_dawproject::Arrangement>,
) {
    for track in tracks {
        let is_folder = !track.children.is_empty();
        let track_id = track.id.clone();

        // Take ownership via std::mem::replace to work around borrow issues
        let old = std::mem::replace(builder, ReaperProjectBuilder::new());
        *builder = old.track(&track.name, |t| {
            let mut t = t;

            // Mixer state from channel
            if let Some(ref ch) = track.channel {
                t = t.volume(ch.volume).pan(ch.pan);
                if ch.muted {
                    t = t.muted();
                }
                if ch.solo {
                    t = t.soloed();
                }

                // Devices/plugins
                for dev in &ch.devices {
                    if !dev.enabled {
                        continue;
                    }
                    match dev.format {
                        dawfile_dawproject::DeviceFormat::Vst2 => {
                            t = t.vst(&dev.name, "");
                        }
                        dawfile_dawproject::DeviceFormat::Vst3 => {
                            t = t.vst3(&dev.name, "");
                        }
                        dawfile_dawproject::DeviceFormat::Clap => {
                            t = t.clap(&dev.name, "");
                        }
                        _ => {}
                    }
                }
            }

            // Color
            if let Some(ref color_hex) = track.color
                && let Some(color) = parse_hex_color(color_hex)
            {
                t = t.color(color);
            }

            if is_folder {
                t = t.folder_start();
            }

            // Find lanes for this track in the arrangement
            if let Some(arr) = arrangement {
                for lane in &arr.lanes {
                    if lane.track != track_id {
                        continue;
                    }
                    if let dawfile_dawproject::LaneContent::Clips(clips) = &lane.content {
                        for clip in clips {
                            if !clip.enabled {
                                continue;
                            }
                            let pos = clip.time;
                            let len = clip.duration;
                            if len <= 0.0 {
                                continue;
                            }

                            t = t.item(pos, len, |item| {
                                let mut item = item;
                                if let Some(ref name) = clip.name {
                                    item = item.name(name);
                                }

                                match &clip.content {
                                    dawfile_dawproject::ClipContent::Audio(audio) => {
                                        if let Some(ref file_ref) = audio.file {
                                            item = item.source_wave(&file_ref.path);
                                        }
                                    }
                                    dawfile_dawproject::ClipContent::Notes(notes) => {
                                        item = item.source_midi().midi(|midi| {
                                            let mut midi = midi;
                                            for note in notes {
                                                let tick = (note.time * 960.0) as u64;
                                                let dur = (note.duration * 960.0) as u32;
                                                let vel = (note.velocity * 127.0) as u8;
                                                midi = midi.at(tick).note(
                                                    0,
                                                    note.channel,
                                                    note.key,
                                                    vel,
                                                    dur,
                                                );
                                            }
                                            midi
                                        });
                                    }
                                    _ => {}
                                }

                                // Fades
                                if let Some(fade_in) = clip.fade_in_time
                                    && fade_in > 0.0
                                {
                                    item = item.fade_in(
                                        fade_in,
                                        dawfile_reaper::types::item::FadeCurveType::Linear,
                                    );
                                }
                                if let Some(fade_out) = clip.fade_out_time
                                    && fade_out > 0.0
                                {
                                    item = item.fade_out(
                                        fade_out,
                                        dawfile_reaper::types::item::FadeCurveType::Linear,
                                    );
                                }

                                item
                            });
                        }
                    }
                }
            }

            t
        });

        // Recurse for child tracks
        if !track.children.is_empty() {
            add_dawproject_tracks(builder, &track.children, arrangement);

            // Add a folder-end marker on the last child
            // We use an empty track with folder_end to close the folder
            let old = std::mem::replace(builder, ReaperProjectBuilder::new());
            *builder = old.track("", |t| t.folder_end(1));
        }
    }
}

/// Parse a hex color string like "#FF8800" into a REAPER-compatible RGB u32.
fn parse_hex_color(hex: &str) -> Option<u32> {
    let hex = hex.strip_prefix('#')?;
    if hex.len() != 6 {
        return None;
    }
    let r = u32::from_str_radix(&hex[0..2], 16).ok()?;
    let g = u32::from_str_radix(&hex[2..4], 16).ok()?;
    let b = u32::from_str_radix(&hex[4..6], 16).ok()?;
    Some((r << 16) | (g << 8) | b)
}

// ── Color mapping ──────────────────────────────────────────────────────────

/// Convert a Pro Tools color palette byte to a REAPER-compatible color
/// integer (`0x01000000 | (B << 16) | (G << 8) | R`).
///
/// The byte is the raw value of `0x200b +163` per the dawfile-protools
/// decoder. Encoding (user-confirmed against the 23×3 Color Testing
/// fixture):
///
/// - byte `0x00..0x01` = no color / default (returns `None`)
/// - byte `>= 0x02`: `(byte - 2) / 24` is the row (0..2), `(byte - 2) % 24`
///   is the column (0..22). PT's palette has 23 hues sweeping the color
///   wheel starting at dark blue.
///
/// Each subsequent row is a darker variant of the same column hue.
/// REAPER's `COLOR` field needs the high bit (`0x01000000`) set for the
/// custom color to be honored; without it REAPER falls back to default.
pub fn pt_color_to_rgb(byte: u8) -> Option<u32> {
    if byte < 2 {
        return None;
    }
    let pos = (byte - 2) as usize;
    let row = pos / 24;
    let col = pos % 24;
    if col >= 23 || row >= 3 {
        return None;
    }

    // 23-hue palette at row 0 (lightest / most-saturated row).
    // Approximations based on PT's stock palette ordering described by the
    // user: dark blue → purple → magenta → pink → fuchsia → light red →
    // red → red-orange → orange → light orange → yellow → yellow-green →
    // light green → green → green-with-some-blue → cyan → teal → light
    // blue → blue → darker blue → very dark blue → (wraps to blue) → blue.
    const ROW0: [(u8, u8, u8); 23] = [
        (0x4A, 0x5C, 0xBE), // 0  dark blue
        (0x7D, 0x5A, 0xC2), // 1  purple
        (0xB0, 0x4A, 0xB8), // 2  magenta
        (0xCF, 0x66, 0xA7), // 3  pink
        (0xD1, 0x4C, 0x8E), // 4  fuchsia
        (0xE0, 0x6B, 0x6B), // 5  light red
        (0xD9, 0x3D, 0x3D), // 6  red
        (0xE0, 0x55, 0x2A), // 7  red-orange
        (0xE6, 0x7E, 0x22), // 8  orange
        (0xEF, 0xA8, 0x4A), // 9  light orange
        (0xEC, 0xCB, 0x3D), // 10 yellow
        (0xC4, 0xCB, 0x3D), // 11 yellow-green
        (0x9B, 0xCB, 0x4E), // 12 light green
        (0x68, 0xB0, 0x4C), // 13 green
        (0x4F, 0xB0, 0x80), // 14 green-with-blue
        (0x42, 0xB6, 0xB0), // 15 cyan
        (0x35, 0x97, 0xA8), // 16 teal
        (0x52, 0x9F, 0xC2), // 17 light blue
        (0x42, 0x7D, 0xC2), // 18 blue
        (0x36, 0x5F, 0xA8), // 19 darker blue
        (0x2A, 0x46, 0x8C), // 20 very dark blue
        (0x36, 0x5F, 0xA8), // 21 wrap (≈ darker blue)
        (0x42, 0x7D, 0xC2), // 22 wrap (≈ blue)
    ];

    let (r0, g0, b0) = ROW0[col];
    // Row 1: ~70% brightness, Row 2: ~45% brightness.
    let scale = match row {
        0 => 100,
        1 => 70,
        _ => 45,
    };
    let r = ((r0 as u32) * scale / 100) as u8;
    let g = ((g0 as u32) * scale / 100) as u8;
    let b = ((b0 as u32) * scale / 100) as u8;

    // REAPER COLOR field: high byte 0x01 = "use this color".
    Some(0x01_00_00_00 | ((b as u32) << 16) | ((g as u32) << 8) | (r as u32))
}

/// Map Ableton color index (0-69) to a REAPER-compatible RGB integer.
///
/// Ableton uses a fixed 70-color palette. For now, we use a simplified
/// 7-hue mapping. A full palette can be added later.
fn ableton_color_to_rgb(color_index: i32) -> u32 {
    let (r, g, b): (u32, u32, u32) = match color_index % 7 {
        0 => (0xFF, 0x00, 0x00), // Red
        1 => (0xFF, 0xA5, 0x00), // Orange
        2 => (0xFF, 0xFF, 0x00), // Yellow
        3 => (0x00, 0xFF, 0x00), // Green
        4 => (0x00, 0xBF, 0xFF), // Blue
        5 => (0x80, 0x00, 0xFF), // Purple
        6 => (0xFF, 0x69, 0xB4), // Pink
        _ => (0x80, 0x80, 0x80), // Gray (unreachable)
    };
    (r << 16) | (g << 8) | b
}

#[cfg(test)]
mod folder_tests {
    use super::protools_to_rpp;

    fn fixture(name: &str) -> String {
        format!(
            "{}/tests/reaper-assets/protools/{name}",
            env!("CARGO_MANIFEST_DIR")
        )
    }

    /// Extract the (NAME, ISBUS) sequence from RPP track blocks in order.
    fn track_isbus(rpp: &str) -> Vec<(String, Option<String>)> {
        let mut out = Vec::new();
        let mut cur_name: Option<String> = None;
        let mut cur_isbus: Option<String> = None;
        for line in rpp.lines() {
            let t = line.trim();
            if let Some(rest) = t.strip_prefix("NAME ") {
                if let Some(n) = cur_name.take() {
                    out.push((n, cur_isbus.take()));
                }
                cur_name = Some(rest.trim_matches('"').to_string());
                cur_isbus = None;
            } else if let Some(rest) = t.strip_prefix("ISBUS ") {
                cur_isbus = Some(rest.to_string());
            }
        }
        if let Some(n) = cur_name.take() {
            out.push((n, cur_isbus.take()));
        }
        out
    }

    /// Authored: FolderA{ChildA1, ChildA2{GrandA2a, GrandA2b}}, SiblingB.
    /// Expect FolderA + ChildA2 open folders, GrandA2b closes 2 levels.
    #[test]
    fn nested_folder_round_trip_emits_isbus() {
        let rpp = protools_to_rpp(&fixture("folder-nesting.ptx")).expect("convert");
        let seq = track_isbus(&rpp);
        let get = |n: &str| {
            seq.iter()
                .find(|(name, _)| name == n)
                .map(|(_, i)| i.clone())
        };
        assert_eq!(get("FolderA"), Some(Some("1 1".to_string())));
        assert_eq!(get("ChildA1"), Some(None));
        assert_eq!(get("ChildA2"), Some(Some("1 1".to_string())));
        assert_eq!(get("GrandA2a"), Some(None));
        assert_eq!(get("GrandA2b"), Some(Some("2 -2".to_string())));
        assert_eq!(get("SiblingB"), Some(None));
    }

    /// Authored: two independent sibling folders F1{leafA,leafB} F2{leafC,leafD}
    /// plus a top-level leaf. Each folder closes one level on its last child.
    #[test]
    fn sibling_folders_round_trip_emits_isbus() {
        let rpp = protools_to_rpp(&fixture("folder-siblings.ptx")).expect("convert");
        let seq = track_isbus(&rpp);
        let get = |n: &str| {
            seq.iter()
                .find(|(name, _)| name == n)
                .map(|(_, i)| i.clone())
        };
        assert_eq!(get("F1"), Some(Some("1 1".to_string())));
        assert_eq!(get("leafA"), Some(None));
        assert_eq!(get("leafB"), Some(Some("2 -1".to_string())));
        assert_eq!(get("F2"), Some(Some("1 1".to_string())));
        assert_eq!(get("leafC"), Some(None));
        assert_eq!(get("leafD"), Some(Some("2 -1".to_string())));
        assert_eq!(get("leafTop"), Some(None));
    }
}
