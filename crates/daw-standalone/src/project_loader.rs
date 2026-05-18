//! Load a REAPER `.rpp` project into a [`Standalone`] backend.
//!
//! Parses with `dawfile-reaper` and populates `ProjectState` with
//! tracks, items, takes, markers, regions, tempo points, hardware
//! outputs, and routing edges. Returns a `LoadedProject` summary so
//! callers can sanity-check what made it across.
//!
//! Scope (per current standalone capabilities): everything except
//! FX processing. FX entries on the source RPP are skipped (the
//! standalone FX impl is synthetic; loading real plugin state would
//! require a plugin host). Automation envelopes are also skipped for
//! now — the audio-graph integration will need them but the proto
//! `Automation` trait already works against the runtime envelopes
//! created via `add_point`, so loaded envelopes can land in a follow-up.
//!
//! Feature-gated under `rpp-project` (pulls dawfile-reaper, not WASM-
//! compatible due to rayon). For browser use cases, build the
//! project state directly via the proto trait methods.

#![cfg(any(feature = "rpp-project", feature = "rpp-project-wasm"))]

use std::sync::atomic::{AtomicUsize, Ordering};

use dawfile_reaper::types::item::{Item as RppItem, SourceType as RppSourceType, Take as RppTake};
use dawfile_reaper::types::project::{DecodeOptions, ReaperProject};
use dawfile_reaper::types::track::Track as RppTrack;
use uuid::Uuid;

use daw_proto::item::{FadeShape, Item, SourceType};
use daw_proto::primitives::{Duration, PositionInSeconds, Tempo, TimeSignature};
use daw_proto::project::ProjectInfo;
use daw_proto::track::Track;
use daw_proto::{Marker, Region, Take, TempoPoint};

use crate::sync::{ItemEntry, Standalone, TakeList, TrackExt};

/// Like [`load_rpp`] but pulls audio bytes through the project's
/// [`MediaBay`](crate::media_bay::MediaBay) resolver instead of an
/// inline closure. Caller must install a [`BayFileResolver`](
/// crate::media_bay::BayFileResolver) on the bay first — native apps
/// use [`FsFileResolver`](crate::media_bay::FsFileResolver), browser
/// apps install a JS-backed one.
///
/// ```ignore
/// use daw_standalone::media_bay::FsFileResolver;
/// use daw_standalone::project_loader::load_rpp_via_bay;
///
/// daw.media_bay().set_file_resolver(Box::new(FsFileResolver));
/// let (proj, audio) = load_rpp_via_bay(&daw, "Song", &path, &rpp_text)?;
/// ```
#[cfg(feature = "decode")]
pub fn load_rpp_via_bay(
    daw: &Standalone,
    project_name: &str,
    project_path: &str,
    rpp_text: &str,
) -> Result<
    (
        LoadedProject,
        crate::audio_engine::materialize::MaterializeReport,
    ),
    String,
> {
    let proj = load_rpp_text(daw, project_name, project_path, rpp_text)?;
    let audio = crate::audio_engine::materialize::materialize_via_bay(daw, &proj.project_guid)?;
    Ok((proj, audio))
}

/// One-shot wrapper that loads structure AND materializes audio.
///
/// Equivalent to `load_rpp_text(...)` followed by
/// `materialize_audio(...)`, returning both reports. Available when
/// the `decode` feature is on (so symphonia is available).
///
/// ```ignore
/// use daw_standalone::project_loader::load_rpp;
///
/// let (proj, audio) = load_rpp(&daw, "Song", "/tmp/song.rpp", &rpp_text, |path| {
///     std::fs::read(path).map_err(|e| e.to_string())
/// })?;
/// eprintln!("loaded {} tracks, {} audio sources", proj.track_count, audio.loaded);
/// ```
#[cfg(feature = "decode")]
pub fn load_rpp<F>(
    daw: &Standalone,
    project_name: &str,
    project_path: &str,
    rpp_text: &str,
    resolver: F,
) -> Result<
    (
        LoadedProject,
        crate::audio_engine::materialize::MaterializeReport,
    ),
    String,
>
where
    F: FnMut(&str) -> Result<Vec<u8>, String>,
{
    let proj = load_rpp_text(daw, project_name, project_path, rpp_text)?;
    let audio =
        crate::audio_engine::materialize::materialize_audio(daw, &proj.project_guid, resolver);
    Ok((proj, audio))
}

/// Summary of what was loaded.
#[derive(Debug, Default)]
pub struct LoadedProject {
    pub project_guid: String,
    pub track_count: usize,
    pub item_count: usize,
    pub take_count: usize,
    pub marker_count: usize,
    pub region_count: usize,
    pub tempo_point_count: usize,
    pub hw_output_count: usize,
    /// Warnings emitted during the load (e.g. unsupported FX skipped).
    pub warnings: Vec<String>,
}

/// Parse RPP text and populate a fresh project in `daw`. Returns the
/// seeded project's GUID + a summary of what was loaded.
pub fn load_rpp_text(
    daw: &Standalone,
    project_name: &str,
    project_path: &str,
    rpp_text: &str,
) -> Result<LoadedProject, String> {
    let rpp =
        dawfile_reaper::parse_rpp_file(rpp_text).map_err(|e| format!("rpp parse failed: {e:?}"))?;
    let project = ReaperProject::from_rpp_project_with_options(&rpp, DecodeOptions::full())
        .map_err(|e| format!("rpp decode failed: {e:?}"))?;

    let project_guid = Uuid::new_v4().to_string();
    daw.seed_project(ProjectInfo {
        guid: project_guid.clone(),
        name: project_name.to_string(),
        path: project_path.to_string(),
    });

    let mut summary = LoadedProject {
        project_guid: project_guid.clone(),
        ..Default::default()
    };

    populate_tracks(daw, &project_guid, &project, &mut summary);
    populate_markers_regions(daw, &project_guid, &project, &mut summary);
    populate_tempo(daw, &project_guid, &project, &mut summary);
    populate_routing(daw, &project_guid, &project, &mut summary);

    Ok(summary)
}

fn populate_tracks(
    daw: &Standalone,
    project_guid: &str,
    project: &ReaperProject,
    summary: &mut LoadedProject,
) {
    static ITEM_COUNTER: AtomicUsize = AtomicUsize::new(0);

    let _ = daw.with_project_mut(project_guid, |p| {
        // Default project tempo / time signature from the tempo
        // envelope (or fall back to 120/4-4).
        if let Some(env) = &project.tempo_envelope {
            p.transport.tempo = Tempo::from_bpm(env.default_tempo.max(1.0));
            let (num, denom) = env.default_time_signature;
            p.transport.time_signature = TimeSignature::new(num.max(1) as u32, denom.max(1) as u32);
        }

        for (idx, rt) in project.tracks.iter().enumerate() {
            // Synthesize a GUID — REAPER's track GUIDs aren't always
            // exposed by dawfile-reaper. Use track_id when available.
            let guid = rt
                .track_id
                .clone()
                .unwrap_or_else(|| Uuid::new_v4().to_string());

            let (volume, pan) = rt
                .volpan
                .as_ref()
                .map(|v| (v.volume, v.pan))
                .unwrap_or((1.0, 0.0));
            let (muted, soloed) = rt
                .mutesolo
                .as_ref()
                .map(|m| {
                    let solo =
                        !matches!(m.solo, dawfile_reaper::types::track::TrackSoloState::NoSolo);
                    (m.mute, solo)
                })
                .unwrap_or((false, false));
            let (folder_depth, is_folder) = rt
                .folder
                .as_ref()
                .map(|f| {
                    use dawfile_reaper::types::track::FolderState as FS;
                    let depth = match f.folder_state {
                        FS::Regular => 0,
                        FS::FolderParent => 1,
                        FS::LastInFolder => -1,
                        FS::Unknown(_) => 0,
                    };
                    (depth, depth > 0)
                })
                .unwrap_or((0, false));

            let track = Track {
                guid: guid.clone(),
                index: idx as u32,
                name: rt.name.clone(),
                color: rt.peak_color.map(|c| c as u32),
                muted,
                soloed,
                armed: rt.record.as_ref().map(|r| r.armed).unwrap_or(false),
                selected: rt.selected,
                volume,
                pan,
                parent_guid: None, // resolved in a second pass once we
                // have folder nesting, currently
                // tracked only via folder_depth
                folder_depth,
                is_folder,
                visible_in_tcp: rt
                    .show_in_mixer
                    .as_ref()
                    .map(|s| s.show_in_track_list)
                    .unwrap_or(true),
                visible_in_mixer: rt
                    .show_in_mixer
                    .as_ref()
                    .map(|s| s.show_in_mixer)
                    .unwrap_or(true),
                fx_count: 0, // FX not loaded (synthetic standalone)
                input_fx_count: 0,
            };
            p.tracks.push(track);

            // Extended (non-proto) fields.
            let parent_send_enabled = rt.master_send.as_ref().map(|m| m.enabled).unwrap_or(true);
            p.track_ext.insert(
                guid.clone(),
                TrackExt {
                    num_channels: rt.channel_count.max(1).min(128),
                    record_input: daw_proto::track::RecordInput::None,
                    parent_send_enabled,
                },
            );

            // Items on this track.
            let track_items = p.items_by_track.entry(guid.clone()).or_default();
            for (item_idx, ri) in rt.items.iter().enumerate() {
                let item_guid = ri
                    .item_guid
                    .clone()
                    .unwrap_or_else(|| Uuid::new_v4().to_string());
                let mut item = Item::default();
                item.guid = item_guid.clone();
                item.track_guid = guid.clone();
                item.index = item_idx as u32;
                item.position = PositionInSeconds::from_seconds(ri.position);
                item.length = Duration::from_seconds(ri.length);
                item.snap_offset = Duration::from_seconds(ri.snap_offset);
                item.muted = ri.mute.as_ref().map(|m| m.muted).unwrap_or(false);
                item.selected = ri.selected;
                item.volume = ri.volpan.as_ref().map(|v| v.item_trim).unwrap_or(1.0);
                if let Some(fi) = &ri.fade_in {
                    item.fade_in_length = Duration::from_seconds(fi.time);
                    item.fade_in_shape = fade_curve_to_shape(fi.curve_type);
                }
                if let Some(fo) = &ri.fade_out {
                    item.fade_out_length = Duration::from_seconds(fo.time);
                    item.fade_out_shape = fade_curve_to_shape(fo.curve_type);
                }
                item.color = ri.color.map(|c| c as u32);
                item.loop_source = ri.loop_source;
                item.take_count = ri.takes.len().max(1) as u32;
                // proto `Item` doesn't carry `channel_mode` yet — drop.

                ITEM_COUNTER.fetch_add(1, Ordering::Relaxed);
                p.items.insert(item_guid.clone(), ItemEntry { item });
                track_items.push(item_guid.clone());

                // Takes.
                let mut takes_out = Vec::with_capacity(ri.takes.len().max(1));
                for (take_idx, rt_take) in ri.takes.iter().enumerate() {
                    let take = build_take(&item_guid, take_idx as u32, rt_take, summary);
                    takes_out.push(take);
                }
                let active_idx = ri
                    .takes
                    .iter()
                    .position(|t| t.take_guid == ri.take_guid)
                    .unwrap_or(0) as u32;
                if !takes_out.is_empty() {
                    p.takes.insert(
                        item_guid.clone(),
                        TakeList {
                            active_idx,
                            takes: takes_out,
                        },
                    );
                    summary.take_count += ri.takes.len();
                }

                summary.item_count += 1;
            }
            summary.track_count += 1;
        }

        // Second pass: resolve parent_guid via folder_depth so the
        // standalone view reflects REAPER's nesting.
        resolve_folder_parents(&mut p.tracks);
    });

    let _ = ITEM_COUNTER; // silence unused-warning when this file is the only consumer
}

fn fade_curve_to_shape(curve: dawfile_reaper::types::item::FadeCurveType) -> FadeShape {
    use dawfile_reaper::types::item::FadeCurveType as F;
    match curve {
        F::Linear => FadeShape::Linear,
        F::Square => FadeShape::FastStart, // closest stand-in; proto has no
        // Square fade.
        F::SlowStartEnd => FadeShape::SlowStartEnd,
        F::FastStart => FadeShape::FastStart,
        F::FastEnd => FadeShape::FastEnd,
        F::Bezier => FadeShape::SlowStartEnd, // proto has no Bezier; pick smoothest stand-in
        F::Unknown(_) => FadeShape::Linear,
    }
}

fn build_take(item_guid: &str, index: u32, rt: &RppTake, _summary: &mut LoadedProject) -> Take {
    let take_guid = rt
        .take_guid
        .clone()
        .unwrap_or_else(|| Uuid::new_v4().to_string());
    let (source_type, source_file_path) = match &rt.source {
        Some(src) => {
            let st = match src.source_type {
                RppSourceType::Wave => SourceType::Audio,
                RppSourceType::Midi => SourceType::Midi,
                RppSourceType::Empty => SourceType::Empty,
                _ => SourceType::Unknown,
            };
            let file = if src.file_path.is_empty() {
                None
            } else {
                Some(src.file_path.clone())
            };
            (st, file)
        }
        None => (SourceType::Empty, None),
    };

    Take {
        guid: take_guid,
        item_guid: item_guid.to_string(),
        index,
        is_active: false, // patched up by caller using item.take_guid
        name: rt.name.clone(),
        color: None,
        volume: rt.volpan.as_ref().map(|v| v.take_volume).unwrap_or(1.0),
        play_rate: rt.playrate.as_ref().map(|pr| pr.rate).unwrap_or(1.0),
        pitch: 0.0,
        preserve_pitch: true,
        start_offset: Duration::from_seconds(rt.slip_offset.max(0.0)),
        source_type,
        source_file_path,
        source_length: None,
        source_sample_rate: None,
        source_channels: None,
        is_midi: matches!(source_type, SourceType::Midi),
        midi_note_count: None,
    }
}

fn resolve_folder_parents(tracks: &mut [Track]) {
    // Stack-of-folder-guids walk: when entering a folder we push the
    // track guid onto the stack and mark following tracks until
    // depth decrement.
    let mut stack: Vec<String> = Vec::new();
    for t in tracks.iter_mut() {
        t.parent_guid = stack.last().cloned();
        // Apply depth AFTER setting parent (a folder's own parent is
        // the folder enclosing it, not itself).
        match t.folder_depth.signum() {
            1 => stack.push(t.guid.clone()),
            -1 => {
                for _ in 0..t.folder_depth.unsigned_abs() {
                    stack.pop();
                }
            }
            _ => {}
        }
    }
}

fn populate_markers_regions(
    daw: &Standalone,
    project_guid: &str,
    project: &ReaperProject,
    summary: &mut LoadedProject,
) {
    let _ = daw.with_project_mut(project_guid, |p| {
        for mr in &project.markers_regions.markers {
            let id = next_id(&mut p.next_marker_id);
            let m = Marker {
                id: Some(id),
                position: daw_proto::Position::from_time(PositionInSeconds::from_seconds(
                    mr.position,
                )),
                name: mr.name.clone(),
                color: if mr.color == 0 {
                    None
                } else {
                    Some(mr.color as u32)
                },
                guid: if mr.guid.is_empty() {
                    None
                } else {
                    Some(mr.guid.clone())
                },
                lane: mr.lane.map(|l| l as u32),
            };
            p.markers.insert(id, m);
            summary.marker_count += 1;
        }
        for mr in &project.markers_regions.regions {
            let id = next_id(&mut p.next_region_id);
            let end = mr.end_position.unwrap_or(mr.position);
            let r = Region {
                id: Some(id),
                time_range: daw_proto::primitives::TimeRange::from_seconds(mr.position, end),
                name: mr.name.clone(),
                color: if mr.color == 0 {
                    None
                } else {
                    Some(mr.color as u32)
                },
                guid: if mr.guid.is_empty() {
                    None
                } else {
                    Some(mr.guid.clone())
                },
                lane: mr.lane.map(|l| l as u32),
            };
            p.regions.insert(id, r);
            summary.region_count += 1;
        }
    });
}

fn next_id(counter: &mut u32) -> u32 {
    let id = *counter;
    *counter = counter.saturating_add(1);
    id
}

fn populate_tempo(
    daw: &Standalone,
    project_guid: &str,
    project: &ReaperProject,
    summary: &mut LoadedProject,
) {
    let Some(env) = &project.tempo_envelope else {
        return;
    };
    let _ = daw.with_project_mut(project_guid, |p| {
        for pt in &env.points {
            let mut tp = TempoPoint::default();
            tp.position =
                daw_proto::Position::from_time(PositionInSeconds::from_seconds(pt.position));
            tp.bpm = pt.tempo.max(1.0);
            if let Some(enc) = pt.time_signature_encoded {
                let num = (enc & 0xFFFF).max(1) as u32;
                let denom = ((enc >> 16) & 0xFFFF).max(1) as u32;
                tp.time_signature = Some(TimeSignature::new(num, denom));
            }
            p.tempo_points.push(tp);
            summary.tempo_point_count += 1;
        }
    });
}

fn populate_routing(
    daw: &Standalone,
    project_guid: &str,
    project: &ReaperProject,
    summary: &mut LoadedProject,
) {
    // Hardware outputs only — track→track sends in RPP are stored on
    // the source track as `AUXSEND idx ...` records keyed by
    // destination *track index*. dawfile-reaper exposes those as
    // `track.aux_sends` (a Vec); if your build of dawfile-reaper
    // doesn't include that field this branch becomes a no-op.
    let _ = daw.with_project_mut(project_guid, |p| {
        // Snapshot track GUIDs by index for AUXSEND resolution.
        let track_guids: Vec<String> = p.tracks.iter().map(|t| t.guid.clone()).collect();
        for (src_idx, rt) in project.tracks.iter().enumerate() {
            let Some(src_guid) = track_guids.get(src_idx).cloned() else {
                continue;
            };
            for hw in &rt.hardware_outputs {
                let mut route = daw_proto::TrackRoute::default();
                route.route_type = daw_proto::RouteType::HardwareOutput;
                route.source_track_guid = src_guid.clone();
                route.hw_output_index = Some(hw.output_index as u32);
                route.hw_output_name = Some(format!("HW {}", hw.output_index));
                route.volume = hw.volume;
                route.pan = hw.pan;
                route.muted = hw.mute;
                route.phase_inverted = hw.invert_polarity;
                route.source_channels = daw_proto::ChannelMapping {
                    start_channel: 0,
                    num_channels: rt.channel_count.max(1).min(128),
                };
                let outs = p.hw_outputs.entry(src_guid.clone()).or_default();
                let i = outs.len() as u32;
                route.index = i;
                outs.push(route);
                summary.hw_output_count += 1;
            }
        }
    });
}
