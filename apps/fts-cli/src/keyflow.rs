use std::collections::HashMap;
use std::path::{Path, PathBuf};

use daw::service::routing::{MidiChannelMapping, MidiDestinationChannel, MidiSourceChannel};
use daw::file::types::item::Take;
use daw::file::{
    read_project, MidiEvent, MidiEventType, MidiSource, MidiSourceEvent, ReaperProject, SourceType,
    TempoTimeEnvelope, Track,
};
use eyre::{eyre, Result};
use midly::{
    num::{u15, u24, u28, u4, u7},
    Format, Header, MetaMessage, MidiMessage, Smf, Timing, TrackEvent, TrackEventKind,
};
use serde::Serialize;

const KEYFLOW_FOLDER_NAME: &str = "Keyflow";
const TICKS_PER_QUARTER: u16 = 960;

#[derive(Debug, Clone, Copy)]
struct ExportBounds {
    start_seconds: f64,
    end_seconds: f64,
}

#[derive(Debug, Clone, Serialize)]
pub struct KeyflowAnalysis {
    pub parent_track: String,
    pub project_measure_offset: i32,
    pub source_tracks: Vec<SourceTrackAnalysis>,
    pub song_start: Option<NamedPoint>,
    pub count_in: Option<NamedPoint>,
    pub first_section_region: Option<NamedRange>,
    pub first_keyflow_item: Option<NamedPoint>,
    pub song_end: Option<NamedPoint>,
    pub furthest_region_end_seconds: Option<f64>,
    pub last_keyflow_item_seconds: Option<f64>,
    pub resolved_start_seconds: Option<f64>,
    pub resolved_start_source: Option<String>,
    pub resolved_end_seconds: Option<f64>,
    pub resolved_end_source: Option<String>,
    pub section_starts: Vec<SectionStartAnalysis>,
}

#[derive(Debug, Clone, Serialize)]
pub struct SourceTrackAnalysis {
    pub index: usize,
    pub name: String,
    pub item_start_seconds: Option<f64>,
    pub item_end_seconds: Option<f64>,
    pub midi_source: String,
    pub midi_destination: String,
}

#[derive(Debug, Clone, Serialize)]
pub struct NamedPoint {
    pub name: String,
    pub seconds: f64,
}

#[derive(Debug, Clone, Serialize)]
pub struct NamedRange {
    pub name: String,
    pub start_seconds: f64,
    pub end_seconds: Option<f64>,
}

#[derive(Debug, Clone, Serialize)]
pub struct SectionStartAnalysis {
    pub name: String,
    pub raw_measure: i32,
    pub display_measure: i32,
    pub start_seconds: f64,
}

#[derive(Debug, Clone)]
struct AbsoluteEvent {
    tick: u32,
    priority: u8,
    kind: TrackEventKind<'static>,
}

#[derive(Debug, Default, Serialize)]
pub struct ExportSummary {
    pub parent_track: String,
    pub source_track_count: usize,
    pub note_count: usize,
    pub marker_count: usize,
    pub region_count: usize,
    pub tempo_event_count: usize,
    pub start_seconds: f64,
    pub end_seconds: f64,
}

#[derive(Debug, Clone)]
struct NoteSpan {
    start_tick: u32,
    end_tick: u32,
    channel: u8,
    pitch: u8,
    velocity: u8,
}

pub fn export_rpp_keyflow_chart(
    rpp_path: PathBuf,
    output_path: Option<PathBuf>,
    parent_track_name: Option<String>,
    as_json: bool,
) -> Result<()> {
    let project = read_project(&rpp_path)?;
    let parent_name = parent_track_name.unwrap_or_else(|| KEYFLOW_FOLDER_NAME.to_string());
    let output_path = output_path.unwrap_or_else(|| default_export_path(&rpp_path));
    let (bytes, summary) = build_keyflow_midi_bytes(&project, &parent_name)?;
    std::fs::write(&output_path, bytes)?;

    if as_json {
        println!(
            "{}",
            serde_json::to_string_pretty(&serde_json::json!({
                "rpp_path": rpp_path,
                "output_path": output_path,
                "summary": summary,
            }))?
        );
    } else {
        println!(
            "Exported Keyflow MIDI from {} to {}",
            rpp_path.display(),
            output_path.display()
        );
        println!(
            "Tracks: {}  Notes: {}  Markers: {}  Regions: {}  Tempo events: {}",
            summary.source_track_count,
            summary.note_count,
            summary.marker_count,
            summary.region_count,
            summary.tempo_event_count
        );
        println!(
            "Bounds: {:.3}s -> {:.3}s",
            summary.start_seconds, summary.end_seconds
        );
    }

    Ok(())
}

pub fn analyze_rpp_keyflow_chart(
    rpp_path: PathBuf,
    parent_track_name: Option<String>,
    as_json: bool,
) -> Result<()> {
    let project = read_project(&rpp_path)?;
    let parent_name = parent_track_name.unwrap_or_else(|| KEYFLOW_FOLDER_NAME.to_string());
    let analysis = analyze_keyflow_project(&project, &parent_name)?;

    if as_json {
        println!(
            "{}",
            serde_json::to_string_pretty(&serde_json::json!({
                "rpp_path": rpp_path,
                "analysis": analysis,
            }))?
        );
    } else {
        println!("RPP: {}", rpp_path.display());
        println!("Parent track: {}", analysis.parent_track);
        println!("Project measure offset: {}", analysis.project_measure_offset);
        println!(
            "Resolved start: {}",
            format_resolved_bound(
                analysis.resolved_start_seconds,
                analysis.resolved_start_source.as_deref()
            )
        );
        println!(
            "Resolved end: {}",
            format_resolved_bound(
                analysis.resolved_end_seconds,
                analysis.resolved_end_source.as_deref()
            )
        );
        println!(
            "SONGSTART: {}",
            format_named_point(analysis.song_start.as_ref())
        );
        println!(
            "COUNT-IN: {}",
            format_named_point(analysis.count_in.as_ref())
        );
        println!(
            "First section region: {}",
            format_named_range(analysis.first_section_region.as_ref())
        );
        println!(
            "First Keyflow item: {}",
            format_named_point(analysis.first_keyflow_item.as_ref())
        );
        println!(
            "SONGEND: {}",
            format_named_point(analysis.song_end.as_ref())
        );
        println!(
            "Furthest region end: {}",
            format_optional_seconds(analysis.furthest_region_end_seconds)
        );
        println!(
            "Last Keyflow item end: {}",
            format_optional_seconds(analysis.last_keyflow_item_seconds)
        );
        println!("Source tracks: {}", analysis.source_tracks.len());
        for track in &analysis.source_tracks {
            println!(
                "  - [{}] {}  items: {} -> {}  midi: {} -> {}",
                track.index,
                track.name,
                format_optional_seconds(track.item_start_seconds),
                format_optional_seconds(track.item_end_seconds),
                track.midi_source,
                track.midi_destination
            );
        }
        if !analysis.section_starts.is_empty() {
            println!("Section starts:");
            for section in &analysis.section_starts {
                println!(
                    "  - {}: measure {} (raw {}) @ {:.3}s",
                    section.name,
                    section.display_measure,
                    section.raw_measure,
                    section.start_seconds
                );
            }
        }
    }

    Ok(())
}

fn build_keyflow_midi_bytes(
    project: &ReaperProject,
    parent_track_name: &str,
) -> Result<(Vec<u8>, ExportSummary)> {
    let parent = find_parent_track(project, parent_track_name)?;
    let bounds = detect_export_bounds(project, parent)
        .ok_or_else(|| eyre!("Could not determine song bounds from markers/regions"))?;
    let tempo_envelope = project.tempo_envelope.clone().unwrap_or_default();

    let song_start_qn = tempo_envelope.beats_at_time(bounds.start_seconds);
    let song_end_qn = tempo_envelope.beats_at_time(bounds.end_seconds);
    let song_start_tick = qn_to_tick(song_start_qn);
    let song_end_tick = qn_to_tick(song_end_qn);

    let mut summary = ExportSummary {
        parent_track: parent.name.clone(),
        start_seconds: bounds.start_seconds,
        end_seconds: bounds.end_seconds,
        ..ExportSummary::default()
    };

    let mut events = gather_tempo_events(&tempo_envelope, &bounds, song_start_qn, &mut summary);
    events.extend(gather_marker_events(
        project,
        &tempo_envelope,
        &bounds,
        song_start_qn,
        &mut summary,
    ));
    events.extend(gather_region_events(
        project,
        &tempo_envelope,
        &bounds,
        song_start_qn,
        &mut summary,
    ));
    events.extend(gather_note_events(
        project,
        parent,
        &tempo_envelope,
        song_start_tick,
        song_end_tick,
        &mut summary,
    ));

    if events.is_empty() {
        return Err(eyre!("No Keyflow MIDI events found inside the song bounds"));
    }

    events.sort_by(|a, b| {
        a.tick
            .cmp(&b.tick)
            .then_with(|| a.priority.cmp(&b.priority))
    });

    let mut track_events = Vec::with_capacity(events.len() + 1);
    let mut last_tick = 0u32;
    for event in events {
        let delta = event.tick.saturating_sub(last_tick);
        last_tick = event.tick;
        track_events.push(TrackEvent {
            delta: u28::new(delta),
            kind: event.kind,
        });
    }
    track_events.push(TrackEvent {
        delta: u28::new(0),
        kind: TrackEventKind::Meta(MetaMessage::EndOfTrack),
    });

    let smf = Smf {
        header: Header::new(
            Format::SingleTrack,
            Timing::Metrical(u15::new(TICKS_PER_QUARTER)),
        ),
        tracks: vec![track_events],
    };

    let mut bytes = Vec::new();
    smf.write_std(&mut bytes)?;
    Ok((bytes, summary))
}

fn analyze_keyflow_project(
    project: &ReaperProject,
    parent_track_name: &str,
) -> Result<KeyflowAnalysis> {
    let parent = find_parent_track(project, parent_track_name)?;
    let project_measure_offset = project.properties.proj_offs.map(|(_, measure, _)| measure).unwrap_or(0);
    let source_tracks = analyze_source_tracks(project, parent);
    let song_start = find_named_marker(project, |name| {
        is_songstart_marker(name) || name.starts_with("=SONGSTART")
    });
    let count_in = find_named_marker(project, is_count_in_marker);
    let first_section_region = normalized_regions(project)
        .into_iter()
        .find(|region| !region.name.is_empty() && !is_structural_marker(&region.name));
    let item_bounds = detect_keyflow_item_bounds(project, parent);
    let first_keyflow_item = item_bounds.map(|(start, _)| NamedPoint {
        name: "First Keyflow item".to_string(),
        seconds: start,
    });
    let song_end = find_named_marker(project, |name| {
        is_songend_marker(name) || name.starts_with("=SONGEND") || name == "=END"
    });
    let furthest_region_end_seconds = normalized_regions(project)
        .into_iter()
        .filter_map(|region| region.end_seconds)
        .max_by(f64::total_cmp);
    let last_keyflow_item_seconds = item_bounds.map(|(_, end)| end);
    let (resolved_start_seconds, resolved_start_source) = resolve_start_bound(
        song_start.as_ref(),
        count_in.as_ref(),
        first_section_region.as_ref(),
        first_keyflow_item.as_ref(),
    );
    let (resolved_end_seconds, resolved_end_source) = resolve_end_bound(
        song_end.as_ref(),
        furthest_region_end_seconds,
        last_keyflow_item_seconds,
    );
    let section_starts = normalized_regions(project)
        .into_iter()
        .filter(|region| !region.name.is_empty() && !is_structural_marker(&region.name))
        .map(|region| {
            let raw_measure = seconds_to_measure_index(project, region.start_seconds);
            SectionStartAnalysis {
                name: region.name,
                raw_measure,
                display_measure: raw_measure + project_measure_offset,
                start_seconds: region.start_seconds,
            }
        })
        .collect::<Vec<_>>();

    Ok(KeyflowAnalysis {
        parent_track: parent.name.clone(),
        project_measure_offset,
        source_tracks,
        song_start,
        count_in,
        first_section_region,
        first_keyflow_item,
        song_end,
        furthest_region_end_seconds,
        last_keyflow_item_seconds,
        resolved_start_seconds,
        resolved_start_source,
        resolved_end_seconds,
        resolved_end_source,
        section_starts,
    })
}

fn seconds_to_measure_index(project: &ReaperProject, seconds: f64) -> i32 {
    let tempo_envelope = project.tempo_envelope.clone().unwrap_or_default();
    let (measure, _, _) = tempo_envelope.musical_position_at_time(seconds);
    measure
}

fn find_parent_track<'a>(project: &'a ReaperProject, parent_track_name: &str) -> Result<&'a Track> {
    project
        .tracks
        .iter()
        .find(|track| track.name.eq_ignore_ascii_case(parent_track_name))
        .ok_or_else(|| eyre!("Keyflow parent track '{}' not found", parent_track_name))
}

fn analyze_source_tracks(project: &ReaperProject, parent: &Track) -> Vec<SourceTrackAnalysis> {
    let mut tracks = parent
        .receives
        .iter()
        .filter_map(|receive| {
            let track_index = usize::try_from(receive.source_track_index).ok()?;
            let track = project.tracks.get(track_index)?;
            let item_bounds = track_item_bounds(track);
            let mapping = receive.midi_channel_mapping();

            Some(SourceTrackAnalysis {
                index: track_index,
                name: track.name.clone(),
                item_start_seconds: item_bounds.map(|bounds| bounds.0),
                item_end_seconds: item_bounds.map(|bounds| bounds.1),
                midi_source: describe_source_channel(mapping.source),
                midi_destination: describe_destination_channel(mapping.destination),
            })
        })
        .collect::<Vec<_>>();

    tracks.sort_by(|a, b| a.index.cmp(&b.index));
    tracks
}

fn track_item_bounds(track: &Track) -> Option<(f64, f64)> {
    let mut earliest = None;
    let mut latest = None;

    for item in &track.items {
        earliest = Some(earliest.map_or(item.position, |current: f64| current.min(item.position)));
        let item_end = item.position + item.length;
        latest = Some(latest.map_or(item_end, |current: f64| current.max(item_end)));
    }

    earliest.zip(latest)
}

fn find_named_marker(
    project: &ReaperProject,
    predicate: impl Fn(&str) -> bool,
) -> Option<NamedPoint> {
    project
        .markers_regions
        .all_sorted()
        .into_iter()
        .find(|marker| predicate(&marker.name))
        .map(|marker| NamedPoint {
            name: marker.name.clone(),
            seconds: marker.position,
        })
}

fn normalized_regions(project: &ReaperProject) -> Vec<NamedRange> {
    let mut regions = HashMap::<(String, u64), NamedRange>::new();
    let mut entries_by_id = HashMap::<i32, Vec<_>>::new();

    for entry in project.markers_regions.all_sorted() {
        entries_by_id
            .entry(entry.id)
            .or_default()
            .push(entry.clone());
    }

    for entries in entries_by_id.values_mut() {
        entries.sort_by(|a, b| a.position.total_cmp(&b.position));

        for idx in 0..entries.len() {
            let start = &entries[idx];
            if start.name.is_empty() {
                continue;
            }

            let paired_end = entries
                .iter()
                .skip(idx + 1)
                .find(|candidate| candidate.name.is_empty() && candidate.position > start.position)
                .map(|candidate| candidate.position);
            let end_seconds = paired_end.or(start.end_position);
            let candidate = NamedRange {
                name: start.name.clone(),
                start_seconds: start.position,
                end_seconds,
            };
            let key = (candidate.name.clone(), candidate.start_seconds.to_bits());

            match regions.get_mut(&key) {
                Some(existing) => {
                    let existing_end = existing.end_seconds.unwrap_or(existing.start_seconds);
                    let candidate_end = candidate.end_seconds.unwrap_or(candidate.start_seconds);
                    if candidate_end > existing_end {
                        *existing = candidate;
                    }
                }
                None => {
                    regions.insert(key, candidate);
                }
            }
        }
    }

    let mut result = regions.into_values().collect::<Vec<_>>();
    result.sort_by(|a, b| a.start_seconds.total_cmp(&b.start_seconds));
    result
}

fn resolve_start_bound(
    song_start: Option<&NamedPoint>,
    count_in: Option<&NamedPoint>,
    first_section_region: Option<&NamedRange>,
    first_keyflow_item: Option<&NamedPoint>,
) -> (Option<f64>, Option<String>) {
    if let Some(point) = song_start {
        return (Some(point.seconds), Some("SONGSTART".to_string()));
    }
    if let Some(point) = count_in {
        return (Some(point.seconds), Some("COUNT-IN".to_string()));
    }
    if let Some(region) = first_section_region {
        return (
            Some(region.start_seconds),
            Some("first section region".to_string()),
        );
    }
    if let Some(point) = first_keyflow_item {
        return (Some(point.seconds), Some("first Keyflow item".to_string()));
    }
    (None, None)
}

fn resolve_end_bound(
    song_end: Option<&NamedPoint>,
    furthest_region_end_seconds: Option<f64>,
    last_keyflow_item_seconds: Option<f64>,
) -> (Option<f64>, Option<String>) {
    if let Some(point) = song_end {
        return (Some(point.seconds), Some("SONGEND".to_string()));
    }
    if let Some(seconds) = furthest_region_end_seconds {
        return (Some(seconds), Some("furthest region end".to_string()));
    }
    if let Some(seconds) = last_keyflow_item_seconds {
        return (Some(seconds), Some("last Keyflow item".to_string()));
    }
    (None, None)
}

fn gather_note_events(
    project: &ReaperProject,
    parent: &Track,
    tempo_envelope: &TempoTimeEnvelope,
    song_start_tick: u32,
    song_end_tick: u32,
    summary: &mut ExportSummary,
) -> Vec<AbsoluteEvent> {
    let mut events = Vec::new();
    let mut source_track_count = 0usize;

    let receive_mappings: HashMap<usize, MidiChannelMapping> = parent
        .receives
        .iter()
        .filter_map(|receive| {
            usize::try_from(receive.source_track_index)
                .ok()
                .map(|idx| (idx, receive.midi_channel_mapping()))
        })
        .collect();

    for (track_index, mapping) in receive_mappings {
        let Some(track) = project.tracks.get(track_index) else {
            continue;
        };
        source_track_count += 1;

        for item in &track.items {
            let item_start_tick = qn_to_tick(tempo_envelope.beats_at_time(item.position));
            let item_end_tick =
                qn_to_tick(tempo_envelope.beats_at_time(item.position + item.length));
            if item_end_tick <= song_start_tick || item_start_tick >= song_end_tick {
                continue;
            }

            let Some(take) = active_take(item.takes.as_slice()) else {
                continue;
            };
            let Some(source) = &take.source else {
                continue;
            };
            if source.source_type != SourceType::Midi {
                continue;
            }
            let Some(midi) = &source.midi_data else {
                continue;
            };

            for note in extract_note_spans(midi) {
                let source_channel = note.channel.saturating_add(1);
                if !mapping_accepts_channel(&mapping, source_channel) {
                    continue;
                }

                let note_start_tick =
                    item_start_tick.saturating_add(scale_tick(note.start_tick, midi.ticks_per_qn));
                let note_end_tick = item_start_tick
                    .saturating_add(scale_tick(note.end_tick, midi.ticks_per_qn))
                    .min(item_end_tick);
                if note_end_tick <= song_start_tick || note_start_tick >= song_end_tick {
                    continue;
                }

                let output_channel =
                    mapping_output_channel(&mapping, source_channel).saturating_sub(1);
                let clipped_start = note_start_tick
                    .max(song_start_tick)
                    .saturating_sub(song_start_tick);
                let clipped_end = note_end_tick
                    .min(song_end_tick)
                    .saturating_sub(song_start_tick);
                if clipped_end <= clipped_start {
                    continue;
                }

                summary.note_count += 1;
                events.push(AbsoluteEvent {
                    tick: clipped_start,
                    priority: 20,
                    kind: TrackEventKind::Midi {
                        channel: u4::new(output_channel),
                        message: MidiMessage::NoteOn {
                            key: u7::new(note.pitch),
                            vel: u7::new(note.velocity),
                        },
                    },
                });
                events.push(AbsoluteEvent {
                    tick: clipped_end,
                    priority: 10,
                    kind: TrackEventKind::Midi {
                        channel: u4::new(output_channel),
                        message: MidiMessage::NoteOff {
                            key: u7::new(note.pitch),
                            vel: u7::new(0),
                        },
                    },
                });
            }
        }
    }

    summary.source_track_count = source_track_count;
    events
}

fn gather_marker_events(
    project: &ReaperProject,
    tempo_envelope: &TempoTimeEnvelope,
    bounds: &ExportBounds,
    song_start_qn: f64,
    summary: &mut ExportSummary,
) -> Vec<AbsoluteEvent> {
    let mut result = Vec::new();
    for marker in project.markers_regions.markers_sorted() {
        if marker.name.is_empty() || is_structural_marker(&marker.name) {
            continue;
        }
        if marker.position < bounds.start_seconds || marker.position > bounds.end_seconds {
            continue;
        }
        let tick = qn_to_tick(tempo_envelope.beats_at_time(marker.position) - song_start_qn);
        summary.marker_count += 1;
        result.push(AbsoluteEvent {
            tick,
            priority: 5,
            kind: TrackEventKind::Meta(MetaMessage::Marker(leak_bytes(marker.name.clone()))),
        });
    }
    result
}

fn gather_region_events(
    project: &ReaperProject,
    tempo_envelope: &TempoTimeEnvelope,
    bounds: &ExportBounds,
    song_start_qn: f64,
    summary: &mut ExportSummary,
) -> Vec<AbsoluteEvent> {
    let mut result = Vec::new();
    let project_measure_offset = project
        .properties
        .proj_offs
        .map(|(_, measure, _)| measure)
        .unwrap_or(0);
    for region in normalized_regions(project) {
        if region.name.is_empty()
            || region.start_seconds < bounds.start_seconds
            || region.start_seconds > bounds.end_seconds
        {
            continue;
        }
        let tick = qn_to_tick(tempo_envelope.beats_at_time(region.start_seconds) - song_start_qn);
        summary.region_count += 1;
        result.push(AbsoluteEvent {
            tick,
            priority: 6,
            kind: TrackEventKind::Meta(MetaMessage::Marker(leak_bytes(region.name.clone()))),
        });
        if let Some(metadata) =
            encode_keyflow_section_metadata(project, &region, project_measure_offset)
        {
            result.push(AbsoluteEvent {
                tick,
                priority: 7,
                kind: TrackEventKind::Meta(MetaMessage::Text(leak_bytes(metadata))),
            });
        }
    }
    result
}

fn gather_tempo_events(
    tempo_envelope: &TempoTimeEnvelope,
    bounds: &ExportBounds,
    song_start_qn: f64,
    summary: &mut ExportSummary,
) -> Vec<AbsoluteEvent> {
    let mut result = Vec::new();
    let (tempo, (num, denom)) = tempo_envelope.get_at_time(bounds.start_seconds);
    result.push(AbsoluteEvent {
        tick: 0,
        priority: 0,
        kind: TrackEventKind::Meta(MetaMessage::Tempo(bpm_to_tempo(tempo))),
    });
    result.push(AbsoluteEvent {
        tick: 0,
        priority: 1,
        kind: TrackEventKind::Meta(MetaMessage::TimeSignature(
            num.clamp(1, 255) as u8,
            (denom.max(1) as u32).trailing_zeros().min(7) as u8,
            24,
            8,
        )),
    });
    summary.tempo_event_count += 2;

    for point in &tempo_envelope.points {
        if point.position <= bounds.start_seconds || point.position > bounds.end_seconds {
            continue;
        }
        let tick = qn_to_tick(tempo_envelope.beats_at_time(point.position) - song_start_qn);
        result.push(AbsoluteEvent {
            tick,
            priority: 0,
            kind: TrackEventKind::Meta(MetaMessage::Tempo(bpm_to_tempo(point.tempo))),
        });
        summary.tempo_event_count += 1;

        if let Some((num, denom)) = point.time_signature() {
            result.push(AbsoluteEvent {
                tick,
                priority: 1,
                kind: TrackEventKind::Meta(MetaMessage::TimeSignature(
                    num.clamp(1, 255) as u8,
                    (denom.max(1) as u32).trailing_zeros().min(7) as u8,
                    24,
                    8,
                )),
            });
            summary.tempo_event_count += 1;
        }
    }

    result
}

fn detect_export_bounds(project: &ReaperProject, parent: &Track) -> Option<ExportBounds> {
    let analysis = analyze_keyflow_project(project, &parent.name).ok()?;
    let start_seconds = analysis.resolved_start_seconds?;
    let end_seconds = analysis.resolved_end_seconds?;
    (end_seconds > start_seconds).then_some(ExportBounds {
        start_seconds,
        end_seconds,
    })
}

fn detect_keyflow_item_bounds(project: &ReaperProject, parent: &Track) -> Option<(f64, f64)> {
    let mut earliest = None;
    let mut latest = None;

    for receive in &parent.receives {
        let Ok(track_index) = usize::try_from(receive.source_track_index) else {
            continue;
        };
        let Some(track) = project.tracks.get(track_index) else {
            continue;
        };

        for item in &track.items {
            earliest =
                Some(earliest.map_or(item.position, |current: f64| current.min(item.position)));
            let item_end = item.position + item.length;
            latest = Some(latest.map_or(item_end, |current: f64| current.max(item_end)));
        }
    }

    earliest.zip(latest)
}

fn describe_source_channel(channel: MidiSourceChannel) -> String {
    match channel {
        MidiSourceChannel::All => "All".to_string(),
        MidiSourceChannel::Channel(channel) => format!("Ch {channel}"),
    }
}

fn describe_destination_channel(channel: MidiDestinationChannel) -> String {
    match channel {
        MidiDestinationChannel::Original => "Original".to_string(),
        MidiDestinationChannel::Channel(channel) => format!("Ch {channel}"),
    }
}

fn format_resolved_bound(seconds: Option<f64>, source: Option<&str>) -> String {
    match (seconds, source) {
        (Some(seconds), Some(source)) => format!("{seconds:.3}s ({source})"),
        (Some(seconds), None) => format!("{seconds:.3}s"),
        _ => "none".to_string(),
    }
}

fn format_optional_seconds(seconds: Option<f64>) -> String {
    seconds
        .map(|seconds| format!("{seconds:.3}s"))
        .unwrap_or_else(|| "none".to_string())
}

fn format_named_point(point: Option<&NamedPoint>) -> String {
    point
        .map(|point| format!("{} @ {:.3}s", point.name, point.seconds))
        .unwrap_or_else(|| "none".to_string())
}

fn format_named_range(range: Option<&NamedRange>) -> String {
    range
        .map(|range| match range.end_seconds {
            Some(end_seconds) => {
                format!(
                    "{} @ {:.3}s -> {:.3}s",
                    range.name, range.start_seconds, end_seconds
                )
            }
            None => format!("{} @ {:.3}s", range.name, range.start_seconds),
        })
        .unwrap_or_else(|| "none".to_string())
}

fn extract_note_spans(midi: &MidiSource) -> Vec<NoteSpan> {
    let mut result = Vec::new();
    let mut active: HashMap<(u8, u8), Vec<(u32, u8)>> = HashMap::new();
    let mut absolute_tick = 0u32;

    for event in &midi.event_stream {
        absolute_tick = absolute_tick.saturating_add(event.delta_ticks());
        let MidiSourceEvent::Midi(event) = event else {
            continue;
        };
        match event.event_type() {
            MidiEventType::NoteOn => {
                if let Some((channel, pitch, velocity)) = parse_note_on(event) {
                    active
                        .entry((channel, pitch))
                        .or_default()
                        .push((absolute_tick, velocity));
                }
            }
            MidiEventType::NoteOff => {
                if let Some((channel, pitch)) = parse_note_off(event) {
                    let Some(stack) = active.get_mut(&(channel, pitch)) else {
                        continue;
                    };
                    let Some((start_tick, velocity)) = stack.pop() else {
                        continue;
                    };
                    if absolute_tick > start_tick {
                        result.push(NoteSpan {
                            start_tick,
                            end_tick: absolute_tick,
                            channel,
                            pitch,
                            velocity,
                        });
                    }
                }
            }
            _ => {}
        }
    }

    result
}

fn parse_note_on(event: &MidiEvent) -> Option<(u8, u8, u8)> {
    let channel = event.channel();
    let pitch = *event.bytes.get(1)?;
    let velocity = *event.bytes.get(2)?;
    (velocity > 0).then_some((channel, pitch, velocity.clamp(1, 127)))
}

fn parse_note_off(event: &MidiEvent) -> Option<(u8, u8)> {
    Some((event.channel(), *event.bytes.get(1)?))
}

fn active_take(takes: &[Take]) -> Option<&Take> {
    takes
        .iter()
        .find(|take| take.is_selected)
        .or_else(|| takes.first())
}

fn scale_tick(source_tick: u32, source_ppq: u32) -> u32 {
    if source_ppq == 0 || source_ppq == u32::from(TICKS_PER_QUARTER) {
        return source_tick;
    }
    ((u64::from(source_tick) * u64::from(TICKS_PER_QUARTER)) / u64::from(source_ppq)) as u32
}

fn bpm_to_tempo(bpm: f64) -> u24 {
    let micros = ((60_000_000.0 / bpm.max(0.001)).round() as u32).clamp(1, 0x00ff_ffff);
    u24::new(micros)
}

fn qn_to_tick(qn: f64) -> u32 {
    (qn * f64::from(TICKS_PER_QUARTER)).round().max(0.0) as u32
}

fn encode_keyflow_section_metadata(
    project: &ReaperProject,
    region: &NamedRange,
    project_measure_offset: i32,
) -> Option<String> {
    let end_seconds = region.end_seconds?;
    if end_seconds <= region.start_seconds {
        return None;
    }

    let start_measure = seconds_to_measure_index(project, region.start_seconds) + project_measure_offset;
    let end_measure = seconds_to_measure_index(project, end_seconds) + project_measure_offset;
    let length = (end_measure - start_measure).max(1);
    Some(format!(
        "KFSECTION\t{}\t{}\t{}",
        region.name, start_measure, length
    ))
}

fn default_export_path(rpp_path: &Path) -> PathBuf {
    let stem = rpp_path
        .file_stem()
        .and_then(|s| s.to_str())
        .unwrap_or("keyflow");
    rpp_path.with_file_name(format!("{stem}.keyflow.mid"))
}

fn mapping_accepts_channel(mapping: &MidiChannelMapping, source_channel: u8) -> bool {
    match mapping.source {
        MidiSourceChannel::All => true,
        MidiSourceChannel::Channel(channel) => channel == source_channel,
    }
}

fn mapping_output_channel(mapping: &MidiChannelMapping, source_channel: u8) -> u8 {
    match mapping.destination {
        MidiDestinationChannel::Original => source_channel,
        MidiDestinationChannel::Channel(channel) => channel,
    }
}

fn leak_bytes(text: String) -> &'static [u8] {
    Box::leak(text.into_bytes().into_boxed_slice())
}

fn is_songstart_marker(name: &str) -> bool {
    let upper = name.to_uppercase();
    upper == "SONGSTART"
        || upper.starts_with("SONGSTART ")
        || upper == "SONG START"
        || upper.starts_with("SONG START ")
}

fn is_songend_marker(name: &str) -> bool {
    let upper = name.to_uppercase();
    upper == "SONGEND"
        || upper.starts_with("SONGEND ")
        || upper == "SONG END"
        || upper.starts_with("SONG END ")
}

fn is_count_in_marker(name: &str) -> bool {
    name.to_uppercase().replace(['-', ' ', '_'], "") == "COUNTIN"
}

fn is_structural_marker(name: &str) -> bool {
    is_count_in_marker(name)
        || is_songstart_marker(name)
        || is_songend_marker(name)
        || name == "=START"
        || name == "=END"
        || name.starts_with("=SONGSTART")
        || name.starts_with("=SONGEND")
}
