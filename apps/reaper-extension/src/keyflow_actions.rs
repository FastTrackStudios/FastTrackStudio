use actions_proto::ActionResult;
use daw::reaper::safe_wrappers::{
    item as item_sw, markers as markers_sw, midi as midi_sw, routing as routing_sw,
    tempo as tempo_sw, time_map as time_map_sw, ReaperLow,
};
use daw::reaper::track::{add_track_on_main_thread, set_folder_depth_on_main_thread};
use daw::service::routing::{MidiChannelMapping, MidiDestinationChannel, MidiSourceChannel};
use midly::{
    num::{u15, u24, u28, u4, u7},
    Format, Header, MetaMessage, MidiMessage, Smf, Timing, TrackEvent, TrackEventKind,
};
use reaper_high::{Project, Reaper, SendPartnerType, Track, TrackRoutePartner};
use reaper_medium::{
    ItemAttributeKey, ProjectContext as ReaperProjectContext, TrackAttributeKey,
    TrackSendAttributeKey, TrackSendCategory,
};
use std::cmp::Ordering;
use std::path::PathBuf;

const KEYFLOW_FOLDER_NAME: &str = "Keyflow";
const TICKS_PER_QUARTER: u16 = 960;
const KEYFLOW_TRACKS: [(&str, u8); 4] = [("CHORDS", 1), ("LINES", 2), ("HITS", 3), ("STOPS", 4)];

#[derive(Clone)]
struct ExportBounds {
    start_seconds: f64,
    end_seconds: f64,
}

#[derive(Clone)]
struct AbsoluteEvent {
    tick: u32,
    priority: u8,
    kind: TrackEventKind<'static>,
}

pub fn generate_keyflow_tracks() -> ActionResult {
    let project = Reaper::get().current_project();

    let result: Option<String> = project.undoable("Generate Keyflow Tracks", || {
        let parent = ensure_keyflow_folder(&project)?;
        let mut configured = 0usize;

        for (name, channel) in KEYFLOW_TRACKS {
            let child = ensure_keyflow_child_track(&project, &parent, name)?;
            configure_keyflow_child_send(&child, &parent, channel).ok()?;
            configured += 1;
        }

        Some(format!(
            "Configured Keyflow folder with {configured} child tracks"
        ))
    });

    match result {
        Some(message) => ActionResult::success_with_message(message),
        None => ActionResult::failure("Failed to generate Keyflow tracks"),
    }
}

pub fn export_keyflow_chart_to_midi() -> ActionResult {
    let project = Reaper::get().current_project();
    let Some(parent) = find_track_by_name(&project, KEYFLOW_FOLDER_NAME) else {
        return ActionResult::failure("Keyflow folder track not found");
    };
    let Some(bounds) = detect_export_bounds(&project, &parent) else {
        return ActionResult::failure("Could not determine song bounds from markers/regions");
    };
    let Some(path) = prompt_export_path(default_export_path(&project)) else {
        return ActionResult::success_with_message("Export cancelled");
    };

    match build_keyflow_midi_bytes(&project, &parent, &bounds)
        .and_then(|bytes| std::fs::write(&path, bytes).map_err(|e| e.to_string()))
    {
        Ok(()) => ActionResult::success_with_message(format!(
            "Exported Keyflow MIDI to {}",
            path.display()
        )),
        Err(error) => ActionResult::failure(format!("Keyflow MIDI export failed: {error}")),
    }
}

fn ensure_keyflow_folder(project: &Project) -> Option<Track> {
    if let Some(track) = find_track_by_name(project, KEYFLOW_FOLDER_NAME) {
        if track.folder_depth_change() <= 0 {
            set_folder_depth_on_main_thread(&track.guid().to_string_without_braces(), 1).ok()?;
        }
        return Some(track);
    }

    let guid = add_track_on_main_thread(KEYFLOW_FOLDER_NAME, None)?;
    let track = find_track_by_guid(project, &guid)?;
    set_folder_depth_on_main_thread(&guid, 1).ok()?;
    Some(track)
}

fn ensure_keyflow_child_track(project: &Project, parent: &Track, name: &str) -> Option<Track> {
    if let Some(track) = find_child_track_by_name(project, parent, name) {
        return Some(track);
    }

    let span = folder_span(project, parent);
    let insert_index = span.map(|(_, end)| end + 1).unwrap_or(parent.index()? + 1);
    let guid = add_track_on_main_thread(name, Some(insert_index))?;
    let new_track = find_track_by_guid(project, &guid)?;

    if let Some((_, previous_last_idx)) = span {
        if let Some(previous_last) = project.track_by_index(previous_last_idx) {
            let old_depth = previous_last.folder_depth_change();
            if old_depth < 0 {
                set_folder_depth_on_main_thread(
                    &previous_last.guid().to_string_without_braces(),
                    old_depth + 1,
                )
                .ok()?;
            }
        }
    }

    set_folder_depth_on_main_thread(&guid, -1).ok()?;
    Some(new_track)
}

fn configure_keyflow_child_send(
    child: &Track,
    parent: &Track,
    midi_channel: u8,
) -> Result<(), String> {
    let mapping = MidiChannelMapping {
        source: MidiSourceChannel::All,
        destination: MidiDestinationChannel::Channel(midi_channel),
    };
    let medium = Reaper::get().medium_reaper();
    let raw_track = child.raw().map_err(|e| e.to_string())?;

    routing_sw::set_media_track_info_value(medium, raw_track, TrackAttributeKey::MainSend, 0.0);

    let send_index = match find_send_index_to_track(child, parent) {
        Some(index) => index,
        None => child
            .add_send_to(parent)
            .track_route_index()
            .ok_or_else(|| {
                "Created Keyflow send but could not resolve its route index".to_string()
            })?,
    };
    routing_sw::set_track_send_info_value(
        medium,
        raw_track,
        TrackSendCategory::Send,
        send_index,
        TrackSendAttributeKey::MidiFlags,
        encode_midi_flags(&mapping) as f64,
    );
    Ok(())
}

fn build_keyflow_midi_bytes(
    project: &Project,
    parent: &Track,
    bounds: &ExportBounds,
) -> Result<Vec<u8>, String> {
    let song_start_tick = qn_to_tick(daw::reaper::tempo_map::time_to_qn_on_main_thread(
        bounds.start_seconds,
    ));
    let song_end_tick = qn_to_tick(daw::reaper::tempo_map::time_to_qn_on_main_thread(
        bounds.end_seconds,
    ));

    let mut events = gather_tempo_events(bounds, song_start_tick);
    events.extend(gather_marker_events(bounds, song_start_tick));
    events.extend(gather_region_events(bounds, song_start_tick));
    events.extend(gather_note_events(
        project,
        parent,
        song_start_tick,
        song_end_tick,
    )?);

    if events.is_empty() {
        return Err("No Keyflow MIDI events found inside the song bounds".to_string());
    }

    events.sort_by(|a, b| {
        a.tick
            .cmp(&b.tick)
            .then_with(|| a.priority.cmp(&b.priority))
            .then(Ordering::Equal)
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
    smf.write_std(&mut bytes).map_err(|e| e.to_string())?;
    Ok(bytes)
}

fn gather_note_events(
    project: &Project,
    parent: &Track,
    song_start_tick: u32,
    song_end_tick: u32,
) -> Result<Vec<AbsoluteEvent>, String> {
    let mut events = Vec::new();
    let low = Reaper::get().medium_reaper().low();
    let medium = Reaper::get().medium_reaper();

    for child in keyflow_child_tracks(project, parent) {
        let Some(mapping) = track_mapping_to_parent(&child, parent) else {
            continue;
        };

        for item in child.items() {
            let raw_item = item.raw();
            let item_start =
                item_sw::get_item_info_value(medium, raw_item, ItemAttributeKey::Position);
            let item_len = item_sw::get_item_info_value(medium, raw_item, ItemAttributeKey::Length);
            let Some(take) = item_sw::get_active_take(medium, raw_item) else {
                continue;
            };
            if !midi_sw::take_is_midi(low, take) {
                continue;
            }

            let item_start_tick = qn_to_tick(daw::reaper::tempo_map::time_to_qn_on_main_thread(
                item_start,
            ));
            let item_end_tick = qn_to_tick(daw::reaper::tempo_map::time_to_qn_on_main_thread(
                item_start + item_len,
            ));
            if item_end_tick <= song_start_tick || item_start_tick >= song_end_tick {
                continue;
            }

            let counts = midi_sw::count_events(low, take);
            for note_idx in 0..counts.notes {
                let Some(note) = midi_sw::get_note(low, take, note_idx) else {
                    continue;
                };
                if note.muted {
                    continue;
                }

                let source_channel = (note.channel.clamp(0, 15) + 1) as u8;
                if !mapping_accepts_channel(&mapping, source_channel) {
                    continue;
                }

                let output_channel =
                    mapping_output_channel(&mapping, source_channel).saturating_sub(1);
                let abs_start =
                    item_start_tick.saturating_add(note.start_ppq.max(0.0).round() as u32);
                let abs_end = item_start_tick.saturating_add(note.end_ppq.max(0.0).round() as u32);
                if abs_end <= song_start_tick || abs_start >= song_end_tick {
                    continue;
                }

                let clipped_start = abs_start
                    .max(song_start_tick)
                    .saturating_sub(song_start_tick);
                let clipped_end = abs_end.min(song_end_tick).saturating_sub(song_start_tick);
                if clipped_end <= clipped_start {
                    continue;
                }

                let pitch = note.pitch.clamp(0, 127) as u8;
                let velocity = note.velocity.clamp(1, 127) as u8;
                events.push(AbsoluteEvent {
                    tick: clipped_start,
                    priority: 20,
                    kind: TrackEventKind::Midi {
                        channel: u4::new(output_channel),
                        message: MidiMessage::NoteOn {
                            key: u7::new(pitch),
                            vel: u7::new(velocity),
                        },
                    },
                });
                events.push(AbsoluteEvent {
                    tick: clipped_end,
                    priority: 10,
                    kind: TrackEventKind::Midi {
                        channel: u4::new(output_channel),
                        message: MidiMessage::NoteOff {
                            key: u7::new(pitch),
                            vel: u7::new(0),
                        },
                    },
                });
            }
        }
    }

    Ok(events)
}

fn gather_marker_events(bounds: &ExportBounds, song_start_tick: u32) -> Vec<AbsoluteEvent> {
    let low = Reaper::get().medium_reaper().low();
    let mut result = Vec::new();
    let mut idx = 0i32;
    loop {
        let Some(marker) = markers_sw::enum_project_markers(low, idx) else {
            break;
        };
        idx += 1;
        if marker.is_region || marker.name.is_empty() || is_structural_marker(&marker.name) {
            continue;
        }
        if marker.pos < bounds.start_seconds || marker.pos > bounds.end_seconds {
            continue;
        }
        let tick = qn_to_tick(daw::reaper::tempo_map::time_to_qn_on_main_thread(
            marker.pos,
        ))
        .saturating_sub(song_start_tick);
        result.push(AbsoluteEvent {
            tick,
            priority: 5,
            kind: TrackEventKind::Meta(MetaMessage::Marker(leak_bytes(marker.name))),
        });
    }
    result
}

fn gather_region_events(bounds: &ExportBounds, song_start_tick: u32) -> Vec<AbsoluteEvent> {
    let reaper = Reaper::get();
    let low = reaper.medium_reaper().low();
    let project = std::ptr::null_mut();
    let mut result = Vec::new();
    for region in daw::reaper::region::get_regions_on_main_thread() {
        if region.name.is_empty()
            || region.start_seconds() < bounds.start_seconds
            || region.start_seconds() > bounds.end_seconds
        {
            continue;
        }
        let tick = qn_to_tick(daw::reaper::tempo_map::time_to_qn_on_main_thread(
            region.start_seconds(),
        ))
        .saturating_sub(song_start_tick);
        result.push(AbsoluteEvent {
            tick,
            priority: 6,
            kind: TrackEventKind::Meta(MetaMessage::Marker(leak_bytes(region.name.clone()))),
        });
        if let Some(metadata) = encode_keyflow_section_metadata(
            low,
            project,
            &region.name,
            region.start_seconds(),
            region.end_seconds(),
        ) {
            result.push(AbsoluteEvent {
                tick,
                priority: 7,
                kind: TrackEventKind::Meta(MetaMessage::Text(leak_bytes(metadata))),
            });
        }
    }
    result
}

fn gather_tempo_events(bounds: &ExportBounds, song_start_tick: u32) -> Vec<AbsoluteEvent> {
    let reaper = Reaper::get();
    let medium = reaper.medium_reaper();
    let low = medium.low();
    let count = medium.count_tempo_time_sig_markers(ReaperProjectContext::CurrentProject);
    let mut result = Vec::new();

    for i in 0..count {
        let Some(point) = tempo_sw::get_tempo_marker(
            low,
            reaper_medium::ProjectContext::CurrentProject,
            i as i32,
        ) else {
            continue;
        };
        if point.timepos < bounds.start_seconds || point.timepos > bounds.end_seconds {
            continue;
        }

        let tick = qn_to_tick(daw::reaper::tempo_map::time_to_qn_on_main_thread(
            point.timepos,
        ))
        .saturating_sub(song_start_tick);
        let micros = ((60_000_000.0 / point.bpm).round() as u32).clamp(1, 0x00ff_ffff);
        result.push(AbsoluteEvent {
            tick,
            priority: 0,
            kind: TrackEventKind::Meta(MetaMessage::Tempo(u24::new(micros))),
        });

        if point.timesig_num > 0 && point.timesig_denom > 0 {
            let denom_power = (point.timesig_denom as u32).trailing_zeros().min(7) as u8;
            result.push(AbsoluteEvent {
                tick,
                priority: 1,
                kind: TrackEventKind::Meta(MetaMessage::TimeSignature(
                    point.timesig_num.clamp(1, 255) as u8,
                    denom_power,
                    24,
                    8,
                )),
            });
        }
    }

    if result.is_empty() {
        let (tempo, num, denom) =
            daw::reaper::tempo_map::get_tempo_and_time_sig_at_on_main_thread(bounds.start_seconds);
        let micros = ((60_000_000.0 / tempo).round() as u32).clamp(1, 0x00ff_ffff);
        result.push(AbsoluteEvent {
            tick: 0,
            priority: 0,
            kind: TrackEventKind::Meta(MetaMessage::Tempo(u24::new(micros))),
        });
        let denom_power = (denom.max(1) as u32).trailing_zeros().min(7) as u8;
        result.push(AbsoluteEvent {
            tick: 0,
            priority: 1,
            kind: TrackEventKind::Meta(MetaMessage::TimeSignature(
                num.clamp(1, 255) as u8,
                denom_power,
                24,
                8,
            )),
        });
    }

    result
}

fn detect_export_bounds(project: &Project, parent: &Track) -> Option<ExportBounds> {
    let low = Reaper::get().medium_reaper().low();
    let mut song_start = None;
    let mut count_in_start = None;
    let mut section_region_start = None;
    let mut song_end = None;
    let mut fallback_end = None;
    let item_bounds = detect_keyflow_item_bounds(project, parent);
    let mut idx = 0i32;

    loop {
        let Some(marker) = markers_sw::enum_project_markers(low, idx) else {
            break;
        };
        idx += 1;

        if count_in_start.is_none() && is_count_in_marker(&marker.name) {
            count_in_start = Some(marker.pos);
        }

        if marker.is_region {
            if section_region_start.is_none()
                && !marker.name.is_empty()
                && !is_structural_marker(&marker.name)
            {
                section_region_start = Some(marker.pos);
            }
            fallback_end =
                Some(fallback_end.map_or(marker.end, |current: f64| current.max(marker.end)));
            continue;
        }

        if is_songstart_marker(&marker.name) || marker.name.starts_with("=SONGSTART") {
            song_start = Some(marker.pos);
        }
        if is_songend_marker(&marker.name)
            || marker.name.starts_with("=SONGEND")
            || marker.name == "=END"
        {
            song_end = Some(marker.pos);
        }
    }

    let start_seconds = song_start
        .or(count_in_start)
        .or(section_region_start)
        .or(item_bounds.map(|bounds| bounds.0))?;
    let end_seconds = song_end
        .or(fallback_end)
        .or(item_bounds.map(|bounds| bounds.1))?;
    (end_seconds > start_seconds).then_some(ExportBounds {
        start_seconds,
        end_seconds,
    })
}

fn detect_keyflow_item_bounds(project: &Project, parent: &Track) -> Option<(f64, f64)> {
    let medium = Reaper::get().medium_reaper();
    let mut earliest = None;
    let mut latest = None;

    for child in keyflow_child_tracks(project, parent) {
        for item in child.items() {
            let raw_item = item.raw();
            let start = item_sw::get_item_info_value(medium, raw_item, ItemAttributeKey::Position);
            let len = item_sw::get_item_info_value(medium, raw_item, ItemAttributeKey::Length);
            earliest = Some(earliest.map_or(start, |current: f64| current.min(start)));
            let end = start + len;
            latest = Some(latest.map_or(end, |current: f64| current.max(end)));
        }
    }

    earliest.zip(latest)
}

fn default_export_path(project: &Project) -> PathBuf {
    if let Some(project_file) = project.file() {
        let mut path = PathBuf::from(project_file.to_string());
        let stem = path
            .file_stem()
            .and_then(|s| s.to_str())
            .unwrap_or("keyflow");
        path.set_file_name(format!("{stem}.keyflow.mid"));
        return path;
    }

    std::env::current_dir()
        .unwrap_or_else(|_| PathBuf::from("."))
        .join("keyflow_export.mid")
}

fn prompt_export_path(default_path: PathBuf) -> Option<PathBuf> {
    #[cfg(target_os = "macos")]
    {
        return prompt_export_path_macos(&default_path);
    }

    #[cfg(all(target_family = "unix", not(target_os = "macos")))]
    {
        return prompt_export_path_swell(default_path);
    }

    #[cfg(not(target_family = "unix"))]
    {
        let _ = default_path;
        None
    }
}

#[cfg(target_os = "macos")]
fn prompt_export_path_macos(default_path: &std::path::Path) -> Option<PathBuf> {
    use cocoa::base::{id, nil};
    use cocoa::foundation::NSString;
    use objc::{class, msg_send, sel, sel_impl};
    use std::ffi::CStr;
    use std::os::raw::c_char;

    unsafe {
        let panel: id = msg_send![class!(NSSavePanel), savePanel];
        if panel == nil {
            return None;
        }

        let title = NSString::alloc(nil).init_str("Export Keyflow Chart to MIDI");
        let _: () = msg_send![panel, setTitle: title];

        let file_types: id = msg_send![class!(NSMutableArray), array];
        let mid = NSString::alloc(nil).init_str("mid");
        let midi = NSString::alloc(nil).init_str("midi");
        let _: () = msg_send![file_types, addObject: mid];
        let _: () = msg_send![file_types, addObject: midi];
        let _: () = msg_send![panel, setAllowedFileTypes: file_types];

        if let Some(parent) = default_path.parent() {
            let dir = NSString::alloc(nil).init_str(&parent.to_string_lossy());
            let dir_url: id = msg_send![class!(NSURL), fileURLWithPath: dir];
            let _: () = msg_send![panel, setDirectoryURL: dir_url];
        }

        if let Some(name) = default_path.file_name().and_then(|name| name.to_str()) {
            let default_name = NSString::alloc(nil).init_str(name);
            let _: () = msg_send![panel, setNameFieldStringValue: default_name];
        }

        let response: isize = msg_send![panel, runModal];
        if response != 1 {
            return None;
        }

        let url: id = msg_send![panel, URL];
        if url == nil {
            return None;
        }

        let path: id = msg_send![url, path];
        if path == nil {
            return None;
        }

        let utf8: *const c_char = msg_send![path, UTF8String];
        if utf8.is_null() {
            return None;
        }

        Some(PathBuf::from(
            CStr::from_ptr(utf8).to_string_lossy().into_owned(),
        ))
    }
}

#[cfg(all(target_family = "unix", not(target_os = "macos")))]
fn prompt_export_path_swell(default_path: PathBuf) -> Option<PathBuf> {
    use reaper_low::Swell;
    use std::ffi::CString;

    let swell = Swell::get();
    let mut file_buf = vec![0u8; 4096];
    let title = CString::new("Export Keyflow Chart to MIDI").ok()?;
    let initial_dir = default_path
        .parent()
        .and_then(|dir| CString::new(dir.to_string_lossy().to_string()).ok());
    let initial_file = default_path
        .file_name()
        .and_then(|name| CString::new(name.to_string_lossy().to_string()).ok());

    #[cfg(target_family = "unix")]
    let ok = unsafe {
        swell.BrowseForSaveFile(
            title.as_ptr(),
            initial_dir
                .as_ref()
                .map_or(std::ptr::null(), |value| value.as_ptr()),
            initial_file
                .as_ref()
                .map_or(std::ptr::null(), |value| value.as_ptr()),
            std::ptr::null(),
            file_buf.as_mut_ptr() as *mut i8,
            file_buf.len() as i32,
        )
    };

    #[cfg(not(target_family = "unix"))]
    let ok = false;

    if ok {
        let path = unsafe { std::ffi::CStr::from_ptr(file_buf.as_ptr() as *const i8) }
            .to_string_lossy()
            .trim()
            .to_string();
        if !path.is_empty() {
            return Some(PathBuf::from(path));
        }
    }

    None
}

fn find_track_by_name(project: &Project, name: &str) -> Option<Track> {
    project
        .tracks()
        .find(|track| track_name(track).eq_ignore_ascii_case(name))
}

fn find_track_by_guid(project: &Project, guid: &str) -> Option<Track> {
    project
        .tracks()
        .find(|track| track.guid().to_string_without_braces() == guid)
}

fn find_child_track_by_name(project: &Project, parent: &Track, name: &str) -> Option<Track> {
    keyflow_child_tracks(project, parent)
        .into_iter()
        .find(|track| track_name(track).eq_ignore_ascii_case(name))
}

fn keyflow_child_tracks(project: &Project, parent: &Track) -> Vec<Track> {
    folder_span(project, parent)
        .map(|(start, end)| {
            ((start + 1)..=end)
                .filter_map(|idx| project.track_by_index(idx))
                .filter(|track| {
                    KEYFLOW_TRACKS
                        .iter()
                        .any(|(name, _)| track_name(track).eq_ignore_ascii_case(name))
                })
                .collect()
        })
        .unwrap_or_default()
}

fn folder_span(project: &Project, parent: &Track) -> Option<(u32, u32)> {
    let start = parent.index()?;
    let mut remaining = parent.folder_depth_change();
    if remaining <= 0 {
        return None;
    }

    for idx in (start + 1)..project.track_count() {
        let track = project.track_by_index(idx)?;
        remaining += track.folder_depth_change();
        if remaining <= 0 {
            return Some((start, idx));
        }
    }

    Some((start, project.track_count().saturating_sub(1)))
}

fn find_send_index_to_track(source: &Track, destination: &Track) -> Option<u32> {
    let send_count = source.typed_send_count(SendPartnerType::Track);
    for idx in 0..send_count {
        let route = source.typed_send_by_index(SendPartnerType::Track, idx)?;
        if let Some(TrackRoutePartner::Track(track)) = route.partner() {
            if track.guid() == destination.guid() {
                return Some(idx);
            }
        }
    }
    None
}

fn track_mapping_to_parent(source: &Track, parent: &Track) -> Option<MidiChannelMapping> {
    let send_index = find_send_index_to_track(source, parent)?;
    let raw_track = source.raw().ok()?;
    let raw_flags = routing_sw::get_track_send_info_value(
        Reaper::get().medium_reaper(),
        raw_track,
        TrackSendCategory::Send,
        send_index,
        TrackSendAttributeKey::MidiFlags,
    ) as i32;
    Some(parse_midi_flags(raw_flags))
}

fn encode_midi_flags(mapping: &MidiChannelMapping) -> i32 {
    let src = match mapping.source {
        MidiSourceChannel::All => 0,
        MidiSourceChannel::Channel(channel) => channel.clamp(1, 16) as i32,
    };
    let dst = match mapping.destination {
        MidiDestinationChannel::Original => 0,
        MidiDestinationChannel::Channel(channel) => channel.clamp(1, 16) as i32,
    };
    src | (dst << 5)
}

fn parse_midi_flags(raw_flags: i32) -> MidiChannelMapping {
    let src_bits = raw_flags & 0x1f;
    let dst_bits = (raw_flags >> 5) & 0x1f;

    let source = match src_bits {
        1..=16 => MidiSourceChannel::Channel(src_bits as u8),
        _ => MidiSourceChannel::All,
    };
    let destination = match dst_bits {
        1..=16 => MidiDestinationChannel::Channel(dst_bits as u8),
        _ => MidiDestinationChannel::Original,
    };

    MidiChannelMapping {
        source,
        destination,
    }
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

fn qn_to_tick(qn: f64) -> u32 {
    (qn * f64::from(TICKS_PER_QUARTER)).round().max(0.0) as u32
}

fn encode_keyflow_section_metadata(
    low: &ReaperLow,
    project: *mut reaper_low::raw::ReaProject,
    name: &str,
    start_seconds: f64,
    end_seconds: f64,
) -> Option<String> {
    if end_seconds <= start_seconds {
        return None;
    }

    let proj_ctx = reaper_medium::ReaProject::new(project)
        .map(reaper_medium::ProjectContext::Proj)
        .unwrap_or(reaper_medium::ProjectContext::CurrentProject);
    let start_qn = daw::reaper::tempo_map::time_to_qn_on_main_thread(start_seconds);
    let end_qn = daw::reaper::tempo_map::time_to_qn_on_main_thread(end_seconds);
    let start_measure = time_map_sw::qn_to_measures(low, proj_ctx, start_qn).measure_index + 1;
    let end_measure = time_map_sw::qn_to_measures(low, proj_ctx, end_qn).measure_index + 1;
    let length = (end_measure - start_measure).max(1);

    Some(format!(
        "KFSECTION\t{}\t{}\t{}",
        name, start_measure, length
    ))
}

fn track_name(track: &Track) -> String {
    track
        .name()
        .map(|name| name.to_str().to_string())
        .unwrap_or_default()
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn midi_mapping_round_trips() {
        let mapping = MidiChannelMapping {
            source: MidiSourceChannel::All,
            destination: MidiDestinationChannel::Channel(3),
        };
        assert_eq!(parse_midi_flags(encode_midi_flags(&mapping)), mapping);
    }
}
