//! Top-level parse orchestration.
//!
//! The main parse flow:
//! 1. Decrypt the file
//! 2. Detect the Pro Tools version
//! 3. Parse the block tree
//! 4. Extract session metadata (sample rate)
//! 5. Parse tempo and meter maps
//! 6. Extract audio file references
//! 7. Extract regions and region-to-track assignments
//! 8. Extract MIDI data

pub mod audio;
pub mod io;
pub mod meter;
pub mod midi;
pub mod plugins;
pub mod regions;
pub mod tempo;
pub mod tracks;
pub mod version;

use crate::block::{self, Block};
use crate::content_type::ContentType;
use crate::cursor::Cursor;
use crate::decrypt;
use crate::error::{PtError, PtResult};
use crate::types::{ProToolsSession, TempoEvent};

/// Parse a Pro Tools session from raw file bytes.
///
/// The `target_sample_rate` is the rate to convert positions to. If it matches
/// the session rate, no conversion is applied.
pub fn parse_session(data: &mut [u8], target_sample_rate: u32) -> PtResult<ProToolsSession> {
    // Step 1: Decrypt
    let xor_type = decrypt::decrypt(data)?;

    // Step 2: Detect endianness and version
    let is_bigendian = data.get(0x11).copied().unwrap_or(0) != 0;
    let cursor = Cursor::new(data, is_bigendian);

    // Step 3: Parse block tree
    let blocks = block::parse_blocks(data, is_bigendian);

    // Step 4: Detect version
    let version = version::parse_version(&cursor, &blocks, xor_type)?;
    if !(5..=12).contains(&version) {
        return Err(PtError::UnsupportedVersion(version));
    }

    // Step 5: Extract sample rate
    let session_sample_rate = parse_sample_rate(&blocks, &cursor).unwrap_or(48000);
    let rate_factor = if session_sample_rate > 0 && target_sample_rate > 0 {
        target_sample_rate as f64 / session_sample_rate as f64
    } else {
        1.0
    };

    // Step 6: Parse tempo map (needed for all tick→sample conversions below)
    let tempo_segments = tempo::parse_tempo_map(&blocks, &cursor, target_sample_rate);

    // Build the public TempoEvent list from the internal segments.
    let bpm = tempo_segments.first().map(|s| s.bpm).unwrap_or(120.0);
    let tempo_events: Vec<TempoEvent> = tempo_segments
        .iter()
        .map(|s| TempoEvent {
            tick_start: s.tick_start,
            sample_start: s.sample_start,
            bpm: s.bpm,
            ticks_per_beat: s.ticks_per_beat,
        })
        .collect();

    // Step 7: Parse meter map and markers
    let meter_events =
        meter::parse_meter_events(&blocks, &cursor, &tempo_segments, target_sample_rate);
    let markers = meter::parse_markers(&blocks, &cursor, &tempo_segments, target_sample_rate);

    // Step 8: Parse audio files
    let audio_files = audio::parse_audio_files(&blocks, &cursor, version);

    // Step 9: Parse regions
    let audio_regions = regions::parse_audio_regions(&blocks, &cursor, version, rate_factor);

    // Step 10: Parse tracks and region-to-track assignments
    let mut audio_tracks = tracks::parse_audio_tracks(
        &blocks,
        &cursor,
        &audio_regions,
        version,
        rate_factor,
        &tempo_segments,
        target_sample_rate,
    );

    // Step 11: Parse MIDI
    let (midi_regions, mut midi_tracks) = midi::parse_midi(
        &blocks,
        &cursor,
        version,
        rate_factor,
        &tempo_segments,
        target_sample_rate,
    );

    // The PT MIDI track list (block 0x2519) contains entries for every track in
    // the session, not just MIDI tracks; audio tracks that received event
    // chunks via the MIDI region map (0x1058) get pulled in too. Drop any
    // "MIDI" track whose name (with its playlist suffix stripped) matches an
    // audio track. Audio tracks store the base name (e.g., "Vocal Split");
    // MIDI list stores the active-playlist name (e.g., "Vocal Split.01").
    fn strip_playlist_suffix(name: &str) -> &str {
        if let Some(idx) = name.rfind('.') {
            let suffix = &name[idx + 1..];
            if !suffix.is_empty() && suffix.chars().all(|c| c.is_ascii_digit()) {
                return &name[..idx];
            }
        }
        name
    }
    let audio_names: std::collections::HashSet<&str> =
        audio_tracks.iter().map(|t| t.name.as_str()).collect();
    midi_tracks.retain(|t| {
        !audio_names.contains(t.name.as_str())
            && !audio_names.contains(strip_playlist_suffix(t.name.as_str()))
    });

    // Step 12: Decode per-track mix state (volume / pan / mute) from 0x1029
    // blocks. Empirically the blocks are emitted in audio-tracks order (per
    // the 0x1015 list) followed by mixable MIDI/aux tracks (per 0x2519,
    // skipping Master). Stereo PT tracks share one mix state across their
    // L/R channel siblings (consecutive same-name entries).
    //
    // See `docs/pt-track-properties.md` for the byte layout.
    {
        let mix_blocks = collect_blocks_recursive(&blocks, ContentType::TrackMixSettings);
        let data = cursor.data();
        let mut mix_iter = mix_blocks.iter().filter_map(|b| {
            let p = b.offset;
            let payload = p + 2;
            if payload + 17 > data.len() {
                return None;
            }
            let vol = i32::from_le_bytes(data[payload + 1..payload + 5].try_into().unwrap());
            let mute = data[payload + 5] != 0;
            let pan = i32::from_le_bytes(data[payload + 13..payload + 17].try_into().unwrap());
            Some((vol, mute, pan))
        });

        // Audio tracks first, in 0x1015 order. Stereo channel siblings share
        // a single mix state.
        let mut last_name = String::new();
        let mut last_state: Option<(i32, bool, i32)> = None;
        for t in audio_tracks.iter_mut() {
            if t.name != last_name {
                last_state = mix_iter.next();
                last_name = t.name.clone();
            }
            if let Some((v, m, p)) = last_state {
                t.volume_centibel = v;
                t.mute = m;
                t.pan = p;
            }
        }

        // Then MIDI tracks (skip Master, no 0x1029 for it).
        let mut last_name = String::new();
        let mut last_state: Option<(i32, bool, i32)> = None;
        for t in midi_tracks.iter_mut() {
            if t.name == "Master 1" || t.name.starts_with("Master") {
                continue;
            }
            if t.name != last_name {
                last_state = mix_iter.next();
                last_name = t.name.clone();
            }
            if let Some((v, m, p)) = last_state {
                t.volume_centibel = v;
                t.mute = m;
                t.pan = p;
            }
        }
    }

    // Step 13: Parse plugins and I/O channels
    let plugins = plugins::parse_plugins(&blocks, &cursor);
    let io_channels = io::parse_io_channels(&blocks, &cursor);

    Ok(ProToolsSession {
        version,
        session_sample_rate,
        bpm,
        tempo_events,
        meter_events,
        markers,
        audio_files,
        audio_regions,
        audio_tracks,
        midi_regions,
        midi_tracks,
        plugins,
        io_channels,
    })
}

/// Collect every block (and nested child) of the given content type.
fn collect_blocks_recursive<'a>(blocks: &'a [Block], ct: ContentType) -> Vec<&'a Block> {
    let mut out = Vec::new();
    fn walk<'a>(blocks: &'a [Block], ct: ContentType, out: &mut Vec<&'a Block>) {
        for b in blocks {
            if b.content_type == Some(ct) {
                out.push(b);
            }
            walk(&b.children, ct, out);
        }
    }
    walk(blocks, ct, &mut out);
    out
}

/// Find the session sample rate from block type 0x1028.
fn parse_sample_rate(blocks: &[Block], cursor: &Cursor<'_>) -> Option<u32> {
    fn find_recursive(blocks: &[Block], ct: ContentType) -> Option<&Block> {
        for block in blocks {
            if block.content_type == Some(ct) {
                return Some(block);
            }
            if let Some(found) = find_recursive(&block.children, ct) {
                return Some(found);
            }
        }
        None
    }

    let block = find_recursive(blocks, ContentType::SessionSampleRate)?;
    // Sample rate is a u32 at offset + 4
    // (offset+0,1 = content_type, offset+2,3 = flags, offset+4 = sample rate)
    Some(cursor.u32_at(block.offset + 4))
}
