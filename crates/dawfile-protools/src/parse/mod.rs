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

    // Step 12: Decode per-track mix state (volume / pan / mute).
    //
    // Pro Tools emits one `0x1029` (TrackMixSettings) block for each track
    // in `0x251a` (the master MIDI-track list under `0x2519`), in document
    // order, **including** the Master and Click tracks. Verified on the
    // user's "Lord of the Fight" session: a straight 1:1 zip puts
    // `ClickPrint` (0x251a[02]) on `0x1029[02]` (vol=-310 = -31 dB), which
    // matches PT's mixer reading. The "skip Master" rule previously written
    // in `docs/pt-track-properties.md` was incorrect and has been removed.
    //
    // We build a `name → mix_state` map keyed by the 0x251a name and look
    // up each parsed audio/MIDI track by its own name. PT-only tracks
    // (Master, Click) live in `midi_tracks` already, so the lookup is
    // symmetric for them.
    {
        let data = cursor.data();
        let track_list = collect_blocks_recursive(&blocks, ContentType::MidiTrackList)
            .into_iter()
            .next();
        let mix_blocks = collect_blocks_recursive(&blocks, ContentType::TrackMixSettings);

        // Iterate 0x251a entries that have a mix block.
        let mut mix_by_name: std::collections::HashMap<String, (i32, bool, i32)> =
            std::collections::HashMap::new();
        if let Some(list) = track_list {
            let mut mix_idx = 0usize;
            // 0x251a entries are duplicated in the file (2× the 30 logical
            // tracks); the second copy mirrors the first, so we only take
            // the first run by stopping once names start to repeat.
            let mut seen: std::collections::HashSet<String> = std::collections::HashSet::new();
            for child in list.find_children(ContentType::MidiTrackInfo) {
                // name @ child.offset + 4 (length-prefixed string)
                let name_off = child.offset + 4;
                if name_off + 4 > data.len() {
                    continue;
                }
                let (name, _) = cursor.length_prefixed_string(name_off);
                if name.is_empty() {
                    continue;
                }
                if !seen.insert(name.clone()) {
                    // Hit the second copy of the list — stop.
                    break;
                }
                let Some(b) = mix_blocks.get(mix_idx) else {
                    break;
                };
                mix_idx += 1;
                let payload = b.offset + 2;
                if payload + 17 > data.len() {
                    continue;
                }
                let vol = i32::from_le_bytes(data[payload + 1..payload + 5].try_into().unwrap());
                // Mute discriminator is NOT a per-track byte. Frida-traced the
                // PT Reaper Converter v1.5.4 on LotF: the converter checks for
                // a Swift `PTXMutePoint`-class object (Optional<...>) per
                // track. Only ~2 tracks per session have explicit mute
                // records; folder children inherit mute via tree walk. The
                // `0x1029 +5` byte we previously read here was a different PT
                // flag (likely `inactive`/`bouncedSource`) and produced false
                // positives on 12/30 LotF tracks. Default mute=false until
                // the mute-record block ID is located. See
                // docs/pt-reaper-converter-re.md "2026-05-17 round 2".
                let mute = false;
                let pan = i32::from_le_bytes(data[payload + 13..payload + 17].try_into().unwrap());
                mix_by_name.insert(name, (vol, mute, pan));
            }
        }

        let lookup = |name: &str| -> Option<(i32, bool, i32)> {
            if let Some(s) = mix_by_name.get(name) {
                return Some(*s);
            }
            // Audio tracks store the base name ("Vocal Split"); 0x251a uses
            // the active-playlist name ("Vocal Split.01"). Try common
            // playlist suffixes.
            for suffix in [".01", ".02", ".03", ".04", ".05"] {
                let probe = format!("{name}{suffix}");
                if let Some(s) = mix_by_name.get(&probe) {
                    return Some(*s);
                }
            }
            None
        };

        for t in audio_tracks.iter_mut() {
            if let Some((v, m, p)) = lookup(&t.name) {
                t.volume_centibel = v;
                t.mute = m;
                t.pan = p;
            }
        }
        for t in midi_tracks.iter_mut() {
            if let Some((v, m, p)) = lookup(&t.name) {
                t.volume_centibel = v;
                t.mute = m;
                t.pan = p;
            }
        }
    }

    // Step 12b: Decode per-track output routing from 0x260e blocks.
    //
    // Each `0x260d` per-track wrapper holds exactly one `0x260e` routing
    // block as its first non-`0x1029` child. Two payload shapes observed:
    //
    //   - 61 bytes: no destination (payload begins `ff ff 01 01 ...`)
    //   - 66/71 bytes: destination name as a length-prefixed string at
    //     payload offset `+0x24..` (e.g. `0a 00 00 00 "Analog 1-2"`,
    //     `05 00 00 00 "Bus 1"`).
    //
    // The `0x260d` blocks themselves are emitted in `0x251a` document
    // order (1:1 with the same alignment used for `0x1029` above), so we
    // pair `0x260e` with `0x251a` names the same way and build a
    // `name → output` lookup, then resolve each parsed track by name.
    {
        let data = cursor.data();
        let track_list = collect_blocks_recursive(&blocks, ContentType::MidiTrackList)
            .into_iter()
            .next();
        let wrappers = collect_blocks_recursive(&blocks, ContentType::TrackMixWrapper);

        let mut out_by_name: std::collections::HashMap<String, String> =
            std::collections::HashMap::new();
        if let Some(list) = track_list {
            let mut seen: std::collections::HashSet<String> = std::collections::HashSet::new();
            let mut wrap_idx = 0usize;
            for child in list.find_children(ContentType::MidiTrackInfo) {
                let name_off = child.offset + 4;
                if name_off + 4 > data.len() {
                    continue;
                }
                let (name, _) = cursor.length_prefixed_string(name_off);
                if name.is_empty() {
                    continue;
                }
                if !seen.insert(name.clone()) {
                    break;
                }
                let Some(wrapper) = wrappers.get(wrap_idx) else {
                    break;
                };
                wrap_idx += 1;
                // First (and usually only) 0x260e inside the wrapper.
                let Some(route) = wrapper
                    .children
                    .iter()
                    .find(|c| c.content_type == Some(ContentType::TrackRouting))
                else {
                    continue;
                };
                let payload = route.offset + 2;
                let payload_len = route.block_size.saturating_sub(2) as usize;
                // Length-prefixed string at payload +0x24
                let str_off = payload + 0x24;
                if str_off + 4 > data.len() || str_off + 4 - payload > payload_len {
                    continue;
                }
                let str_len =
                    u32::from_le_bytes(data[str_off..str_off + 4].try_into().unwrap()) as usize;
                if str_len == 0 || str_len > 128 {
                    continue;
                }
                let data_start = str_off + 4;
                if data_start + str_len > data.len() {
                    continue;
                }
                let s = String::from_utf8_lossy(&data[data_start..data_start + str_len])
                    .trim_end_matches('\0')
                    .to_string();
                if !s.is_empty() {
                    out_by_name.insert(name, s);
                }
            }
        }

        let lookup_out = |name: &str| -> Option<String> {
            if let Some(s) = out_by_name.get(name) {
                return Some(s.clone());
            }
            for suffix in [".01", ".02", ".03", ".04", ".05"] {
                let probe = format!("{name}{suffix}");
                if let Some(s) = out_by_name.get(&probe) {
                    return Some(s.clone());
                }
            }
            None
        };

        for t in audio_tracks.iter_mut() {
            if let Some(o) = lookup_out(&t.name) {
                t.output = o;
            }
        }
        for t in midi_tracks.iter_mut() {
            if let Some(o) = lookup_out(&t.name) {
                t.output = o;
            }
        }
    }

    // Step 12c: Decode per-track color palette index from 0x200b +106..+107.
    //
    // The color is a 2-byte LE i16: -2 (= 0xfffe) means "default / no
    // color", any non-negative value is a palette index (mapped to RGB
    // via the table in `docs/pt-color-palette-ground-truth.md`).
    //
    // Verified via RPP→PTX probe + plaintext-diff: a track with
    // `color(0xd86e41)` set in RPP produces +106..+107 = `18 00` (=24)
    // in PTX while baseline (no color) shows `fe ff` (=-2). See
    // `docs/pt-field-map.md`.
    //
    // We previously read `+163` which was a different field that
    // happened to correlate with palette index on Color Testing
    // (probably a per-track ordinal counter). The +163 reading was
    // wrong on LotF and other fixtures.
    //
    // To resolve which track each 0x200b belongs to, walk upward from
    // the block through its ancestors and find the nearest `0x2619`
    // (track name) descendant.
    {
        let data = cursor.data();
        let mut parents: std::collections::HashMap<usize, Option<&Block>> =
            std::collections::HashMap::new();
        fn build_parents<'a>(
            blocks: &'a [Block],
            parent: Option<&'a Block>,
            parents: &mut std::collections::HashMap<usize, Option<&'a Block>>,
        ) {
            for b in blocks {
                parents.insert(b.offset, parent);
                build_parents(&b.children, Some(b), parents);
            }
        }
        build_parents(&blocks, None, &mut parents);

        fn find_2619_name(b: &Block, data: &[u8]) -> Option<String> {
            for c in &b.children {
                if c.content_type == Some(ContentType::MarkerEntry) {
                    let p = c.offset + 2;
                    if p + 4 > data.len() {
                        return None;
                    }
                    let len = u32::from_le_bytes(data[p..p + 4].try_into().unwrap()) as usize;
                    if len == 0 || len > 64 || p + 4 + len > data.len() {
                        return None;
                    }
                    return Some(
                        String::from_utf8_lossy(&data[p + 4..p + 4 + len])
                            .trim_end_matches('\0')
                            .to_string(),
                    );
                }
                if let Some(n) = find_2619_name(c, data) {
                    return Some(n);
                }
            }
            None
        }

        let mut color_by_name: std::collections::HashMap<String, u8> =
            std::collections::HashMap::new();
        let aux_blocks = collect_blocks_recursive(&blocks, ContentType::TrackAuxState);
        for b in &aux_blocks {
            // Color is i16 LE at +106..+107 (verified by RPP→PTX diff).
            // We expose it as a u8 (`color_byte`) since palette indices
            // observed so far are all < 256; -2/0xfffe (default) maps to 0.
            let p = b.offset + 2 + 106;
            let color = if p + 2 <= data.len() {
                let val = i16::from_le_bytes([data[p], data[p + 1]]);
                if val < 0 { 0 } else { val as u8 }
            } else {
                0
            };

            // Walk up to 10 ancestors looking for a 0x2619 name anywhere
            // in the subtree.
            let mut name: Option<String> = None;
            let mut anc = parents.get(&b.offset).cloned().flatten();
            let mut depth = 0;
            while let Some(a) = anc {
                if let Some(n) = find_2619_name(a, data) {
                    name = Some(n);
                    break;
                }
                anc = parents.get(&a.offset).cloned().flatten();
                depth += 1;
                if depth > 10 {
                    break;
                }
            }
            if let Some(n) = name {
                // First write wins — preserves the earliest (most-likely-live) entry
                color_by_name.entry(n).or_insert(color);
            }
        }

        for t in audio_tracks.iter_mut() {
            if let Some(c) = color_by_name.get(&t.name) {
                t.color_byte = *c;
            } else {
                // Audio tracks store the base name ("Vocal Split"); 0x2619
                // may carry the active-playlist name ("Vocal Split.01").
                for suffix in [".01", ".02", ".03", ".04", ".05"] {
                    if let Some(c) = color_by_name.get(&format!("{}{suffix}", t.name)) {
                        t.color_byte = *c;
                        break;
                    }
                }
            }
        }
        for t in midi_tracks.iter_mut() {
            if let Some(c) = color_by_name.get(&t.name) {
                t.color_byte = *c;
            }
        }
    }

    // Step 12d: Decode per-track `is_folder` flag from 0x251a.
    //
    // The byte at offset (`0x251a` payload + 4 + len(name)) is `0x01` for
    // folder/container tracks (including Master), `0x00` otherwise. PT
    // groups child tracks under their folder parent in a separate block
    // we haven't decoded yet, but we can at least surface the flag so
    // REAPER can model the per-track folder marker.
    {
        let data = cursor.data();
        let track_list = collect_blocks_recursive(&blocks, ContentType::MidiTrackList)
            .into_iter()
            .next();
        let mut folder_by_name: std::collections::HashMap<String, bool> =
            std::collections::HashMap::new();
        if let Some(list) = track_list {
            let mut seen: std::collections::HashSet<String> = std::collections::HashSet::new();
            for child in list.find_children(ContentType::MidiTrackInfo) {
                let name_off = child.offset + 4;
                if name_off + 4 > data.len() {
                    continue;
                }
                let (name, str_consumed) = cursor.length_prefixed_string(name_off);
                if name.is_empty() {
                    continue;
                }
                if !seen.insert(name.clone()) {
                    break;
                }
                // is_folder flag is the first byte AFTER the length-
                // prefixed name within the 0x251a payload.
                let flag_off = name_off + str_consumed;
                if flag_off < data.len() {
                    folder_by_name.insert(name, data[flag_off] != 0);
                }
            }
        }
        for t in audio_tracks.iter_mut() {
            if let Some(f) = folder_by_name.get(&t.name) {
                t.is_folder = *f;
            } else {
                for suffix in [".01", ".02", ".03", ".04", ".05"] {
                    if let Some(f) = folder_by_name.get(&format!("{}{suffix}", t.name)) {
                        t.is_folder = *f;
                        break;
                    }
                }
            }
        }
        for t in midi_tracks.iter_mut() {
            if let Some(f) = folder_by_name.get(&t.name) {
                t.is_folder = *f;
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

/// Walk a 0x261c block looking for the embedded track name. Path:
/// `0x261c → 0x261b → 0x102d → 0x2619` (length-prefixed string at
/// payload +0).
fn find_2619_track_name(b: &Block, data: &[u8]) -> Option<String> {
    for c in &b.children {
        if c.content_type == Some(ContentType::MarkerEntry) {
            // 0x2619 payload begins at offset + 2 in Block convention
            let p = c.offset + 2;
            if p + 4 > data.len() {
                return None;
            }
            let len = u32::from_le_bytes(data[p..p + 4].try_into().unwrap()) as usize;
            if len == 0 || len > 64 || p + 4 + len > data.len() {
                return None;
            }
            return Some(
                String::from_utf8_lossy(&data[p + 4..p + 4 + len])
                    .trim_end_matches('\0')
                    .to_string(),
            );
        }
        if let Some(n) = find_2619_track_name(c, data) {
            return Some(n);
        }
    }
    None
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
