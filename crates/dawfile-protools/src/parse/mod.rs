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
pub mod mix_aux;
pub mod mute_automation;
pub mod mute_resolver;
pub mod plugins;
pub mod regions;
pub mod solo;
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
        // Detect format: converter-authored PTX nests 11× 0x1029 per
        // 0x261c (TrackContainer). PT-authored sessions have a flatter
        // structure with ≈1 0x1029 per logical track. If the ratio
        // (mix-blocks / containers) is ≥ 8, use per-container scoping
        // so each track maps to its OWN first 0x1029.
        let containers = collect_blocks_recursive(&blocks, ContentType::TrackContainer);
        let all_mix_blocks = collect_blocks_recursive(&blocks, ContentType::TrackMixSettings);
        let mix_blocks: Vec<&crate::block::Block> =
            if !containers.is_empty() && all_mix_blocks.len() >= containers.len() * 8 {
                containers
                    .iter()
                    .filter_map(|c| c.find_all(ContentType::TrackMixSettings).first().copied())
                    .collect()
            } else {
                all_mix_blocks
            };

        // Iterate 0x251a entries that have a mix block.
        let mut mix_by_name: std::collections::HashMap<String, (i32, bool, i32)> =
            std::collections::HashMap::new();
        if let Some(list) = track_list {
            let mut mix_idx = 0usize;
            // 0x251a entries may be duplicated in the file:
            // - PT-authored sessions interleave a 2× full-list copy
            //   AT THE END (block-by-block), so seen-duplicates appear
            //   only after the first complete pass.
            // - Converter-authored multi-track PTX interleaves
            //   [active_t0, alt_t0, active_t1, alt_t1, ...] so seen
            //   duplicates appear EARLIER, mid-list.
            // To handle both: track the LAST mix_idx we advanced. If
            // we've already used the same number of mix blocks as we
            // have unique names AND we hit a duplicate, treat that as
            // the start of the 2× copy and stop. Otherwise skip the
            // duplicate and keep walking.
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
                    // Already saw this name. If we've consumed all
                    // mix blocks already, the 2× copy has started —
                    // stop. Otherwise it's a multi-track interleave;
                    // skip and continue.
                    if mix_idx >= mix_blocks.len() {
                        break;
                    }
                    continue;
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
                // Mute = `0x1029 +5` AND `0x260a[0] +8 == 0`.
                //
                // The `+5` byte alone is the STORED mute-mix bit, but
                // it's also set for inactive/bounced-source tracks. The
                // converter discriminates by ALSO checking
                // `0x260a[0] +8` — the "send routing enabled" flag:
                //
                // - +5=1 AND +8=0 → user clicked mute → effective mute
                // - +5=1 AND +8=1 → bounced/inactive (send routes,
                //                   stored mute is incidental) → NOT effective mute
                // - +5=0 → not muted regardless of +8
                //
                // Verified on LotF (2026-05-17): this rule correctly
                // identifies exactly the 8 tracks the converter emits
                // as MUTESOLO 1 (ClickPrint + 7 LORD family stems),
                // excluding the 12 over-muted tracks (SYZ, AC GTR x2,
                // El Gtr 1, Bass Demo, MIDI 1, Inst*).
                //
                // We compute the effective mute below in the second
                // pass once the 0x260a sibling block is reachable.
                // For now, store the stored bit.
                //
                // Definitively verified by the 10-track probe diff
                // (2026-05-17): generating an RPP with 10 muted tracks
                // and another with 10 plain tracks produces PTX files
                // with IDENTICAL block structure; the diff is purely
                // byte-level flips at +5 (and mirror locations:
                // 0x260a[i] +26, 0x260d +14/+447, 0x261b +407/+840,
                // 0x261c +416/+849, 0x2624 +429/+862).
                //
                // **No separate explicit-mute marker block exists.**
                // Earlier Frida traces showed the converter constructs
                // a Swift `Optional<PTXMutePoint>` for ~2 tracks per
                // session — that object is a RUNTIME construct built
                // from the stored bits + folder-tree walk +
                // automation-envelope state. It is NOT serialized as a
                // distinct PTX block.
                //
                // The "over-mutes" on LotF (SYZ/AC GTR/El Gtr/Bass
                // Demo/Inst* tracks where +5=1 but the converter
                // outputs MUTESOLO 0) reflect the difference between:
                //   - STORED state: this byte (what we read)
                //   - EFFECTIVE playback state: the converter computes
                //     this by combining stored + automation envelope +
                //     active/inactive flag
                //
                // Our parser exposes the STORED state, which is the
                // correct representation of what's in the file.
                // Callers wanting effective-playback semantics need to
                // ALSO read mute automation + active flag (TBD).
                let mute = data[payload + 5] != 0;
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

    // Step 12a2: Assign PT display order from the 0x251a track list.
    //
    // The 0x2519/0x251a list enumerates EVERY track (audio, MIDI, master,
    // folder/divider) in PT's on-screen Edit-window order. That order is what
    // the user sees and what the official converter preserves — unlike the
    // channel-map `index`, which is internal voice assignment and buries
    // Master/Click/Shake at the bottom. We build a `name → sequence` map from
    // the first occurrence of each name, then stamp every parsed track with
    // its position so the emitter can sort audio + MIDI into one merged order.
    {
        let data = cursor.data();
        let track_list = collect_blocks_recursive(&blocks, ContentType::MidiTrackList)
            .into_iter()
            .next();
        let mut order_by_name: std::collections::HashMap<String, u32> =
            std::collections::HashMap::new();
        if let Some(list) = track_list {
            let mut seq = 0u32;
            for child in list.find_children(ContentType::MidiTrackInfo) {
                let name_off = child.offset + 4;
                if name_off + 4 > data.len() {
                    continue;
                }
                let (name, _) = cursor.length_prefixed_string(name_off);
                if name.is_empty() {
                    continue;
                }
                // First occurrence wins (the active entry); later duplicates
                // are alternate-playlist or the 2× tail copy — ignore them.
                let full = order_by_name.entry(name.clone()).or_insert(seq);
                if *full != seq {
                    // Name already seen earlier; don't advance the counter.
                    continue;
                }
                // Also index by the suffix-stripped base name so audio tracks
                // (which store "ClickPrint", not "ClickPrint.01") can match.
                order_by_name
                    .entry(strip_playlist_suffix(&name).to_string())
                    .or_insert(seq);
                seq += 1;
            }
        }
        let order_lookup = |name: &str| -> Option<u32> {
            if let Some(o) = order_by_name.get(name) {
                return Some(*o);
            }
            order_by_name
                .get(strip_playlist_suffix(name))
                .copied()
                .or_else(|| {
                    [".01", ".02", ".03", ".04", ".05"]
                        .iter()
                        .find_map(|s| order_by_name.get(&format!("{name}{s}")).copied())
                })
        };
        for t in audio_tracks.iter_mut() {
            if let Some(o) = order_lookup(&t.name) {
                t.display_order = o;
            }
        }
        for t in midi_tracks.iter_mut() {
            if let Some(o) = order_lookup(&t.name) {
                t.display_order = o;
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

    // Step 12c2: Decode per-track solo flag from 0x102d +162.
    solo::apply_solo_state(&blocks, &cursor, &mut audio_tracks, &mut midi_tracks);

    // Step 12c2.5: Resolve effective mute. The +5 byte we read above
    // is the stored mix-bit, but PT also sets it for inactive/
    // bounced-source tracks. The converter discriminates via the
    // `0x260a[0] +8` "send routing enabled" flag — if non-zero, the
    // track isn't user-muted, just inactive. See
    // docs/pt-field-map.md "Effective mute" + parse/mute_resolver.rs.
    mute_resolver::resolve_effective_mute(&blocks, &cursor, &mut audio_tracks, &mut midi_tracks);

    // Step 12c2.6: Decode per-track mute-automation envelope from the
    // second 0x260a child under each 0x260d wrapper.
    mute_automation::apply_mute_automation(&blocks, &cursor, &mut audio_tracks, &mut midi_tracks);

    // Step 12c3: Fall back to 0x2624 vol/pan mirrors for converter-
    // generated PTX where 0x1029 isn't populated.
    mix_aux::fill_vol_pan_from_2624(&blocks, &cursor, &mut audio_tracks, &mut midi_tracks);

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

    // Step 14: Parse `0x2602` routing entries. Each entry is a u8-flag
    // record. The active flag at +10 distinguishes live entries from
    // template/unused ones. See `docs/converter-frida-discovered-offsets.md`.
    let routing_entries = {
        fn collect_recursive<'a>(blocks: &'a [Block], ct_raw: u16, out: &mut Vec<&'a Block>) {
            for b in blocks {
                if b.content_type_raw == ct_raw {
                    out.push(b);
                }
                collect_recursive(&b.children, ct_raw, out);
            }
        }
        let mut entries = Vec::new();
        collect_recursive(&blocks, 0x2602, &mut entries);
        let data = cursor.data();
        let mut out = Vec::with_capacity(entries.len());
        for b in entries {
            let magic = b.offset.saturating_sub(7);
            if magic + 53 > data.len() {
                continue;
            }
            let active = data[magic + 10] != 0;
            let flag_33 = data[magic + 33];
            let flag_36 = data[magic + 36];
            let mut destination_uid = [0u8; 6];
            destination_uid.copy_from_slice(&data[magic + 47..magic + 53]);
            out.push(crate::types::RoutingEntry {
                block_start: magic,
                active,
                flag_33,
                flag_36,
                destination_uid,
            });
        }
        out
    };

    let edit_groups = parse_edit_groups(&blocks, data);
    let stem_mappings = parse_stem_mappings(&blocks, data);
    let internal_tracks = parse_internal_tracks(&blocks, data);

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
        routing_entries,
        edit_groups,
        stem_mappings,
        internal_tracks,
    })
}

/// Decode the session's internal/aux/bus track list from `0x261e` blocks.
///
/// Each block carries a length-prefixed track name at payload `+0x1d`
/// (= magic + `0x24`) and a 6-byte routing UID at payload `+0x29..+0x2e`
/// (= magic + `0x32..+0x37`). The kind (Aux / Internal Bus / Master
/// Fader / Click) is not yet decoded.
fn parse_internal_tracks(blocks: &[Block], data: &[u8]) -> Vec<crate::types::InternalTrack> {
    let mut out = Vec::new();
    for b in collect_blocks_recursive(blocks, ContentType::InternalTrackEntry) {
        let magic = b.offset.saturating_sub(7);
        let block_end = magic + 9 + b.block_size as usize;
        if magic + 9 >= data.len() || block_end > data.len() {
            continue;
        }
        // Scan forward from payload start for the first `[u32 namelen]
        // [printable ASCII name of that length]` triple. The block has a
        // nested header chain (0x261b → 0x102d → 0x2619) whose exact byte
        // count varies; the name itself is reliably the first sane
        // length-prefixed string in the payload.
        let mut name: Option<(usize, String)> = None;
        let mut p = magic + 9;
        while p + 4 < block_end && p + 4 < data.len() {
            let nlen = u32::from_le_bytes(data[p..p + 4].try_into().unwrap()) as usize;
            if (2..=64).contains(&nlen) && p + 4 + nlen <= block_end && p + 4 + nlen <= data.len() {
                let candidate = &data[p + 4..p + 4 + nlen];
                if candidate.iter().all(|c| (0x20..0x7f).contains(c)) {
                    name = Some((p, String::from_utf8_lossy(candidate).into_owned()));
                    break;
                }
            }
            p += 1;
        }
        let Some((name_pos, name)) = name else {
            continue;
        };
        // Routing UID: the 6 bytes appear ~14 bytes after the name end,
        // preceded by `2a 00 00 00` (the same `0x2a` marker seen in source
        // file UID encoding). Scan for that signature.
        let uid_search_start = name_pos + 4 + name.len();
        let mut routing_uid = [0u8; 6];
        let mut q = uid_search_start;
        while q + 10 < block_end && q + 10 < data.len() {
            if data[q] == 0x2a && data[q + 1] == 0 && data[q + 2] == 0 && data[q + 3] == 0 {
                routing_uid.copy_from_slice(&data[q + 4..q + 10]);
                break;
            }
            q += 1;
        }
        out.push(crate::types::InternalTrack { name, routing_uid });
    }
    out
}

/// Walk a `0x4501` payload tail-region and decode the flat group name list.
///
/// Each entry is `[u32 LE namelen][utf-8 name][i16 LE color]`. The
/// per-track membership table preceding the name list is not yet decoded;
/// we locate the start of the name list by scanning for the first sane
/// `[len][ASCII-printable name]` pair.
///
/// **Preliminary** — the parser is heuristic and may over-read when the
/// preceding per-track membership table happens to contain byte patterns
/// matching the `[len][ASCII]` shape. Caller should treat the list as a
/// best-effort union of stem-types + edit-groups until the membership
/// table is decoded and we can bound the name list precisely.
fn parse_edit_groups(blocks: &[Block], data: &[u8]) -> Vec<crate::types::EditGroup> {
    let mut out = Vec::new();
    for b in collect_blocks_recursive(blocks, ContentType::EditGroupList) {
        // Block payload starts at offset = magic + 9.
        let payload_start = b.offset.saturating_sub(7) + 9;
        let block_end = b.offset.saturating_sub(7) + b.block_size as usize;
        if payload_start >= data.len() || block_end > data.len() {
            continue;
        }
        // Scan from payload_start for the first plausible `[u32 namelen]
        // [printable ASCII name of that length][i16 color]` triple.
        let mut p = payload_start;
        let mut found_start = None;
        while p + 6 < block_end {
            let nlen = u32::from_le_bytes(data[p..p + 4].try_into().unwrap()) as usize;
            if (2..=64).contains(&nlen) && p + 4 + nlen + 2 <= block_end {
                let name = &data[p + 4..p + 4 + nlen];
                if name.iter().all(|c| (0x20..0x7f).contains(c)) {
                    found_start = Some(p);
                    break;
                }
            }
            p += 1;
        }
        let Some(mut p) = found_start else { continue };
        // Read entries until the layout no longer matches.
        while p + 6 < block_end {
            let nlen = u32::from_le_bytes(data[p..p + 4].try_into().unwrap()) as usize;
            if !(1..=64).contains(&nlen) || p + 4 + nlen + 2 > block_end {
                break;
            }
            let name_bytes = &data[p + 4..p + 4 + nlen];
            if !name_bytes.iter().all(|c| (0x20..0x7f).contains(c)) {
                break;
            }
            let name = String::from_utf8_lossy(name_bytes).into_owned();
            let color_raw =
                i16::from_le_bytes(data[p + 4 + nlen..p + 4 + nlen + 2].try_into().unwrap());
            let color = if color_raw == -2 {
                None
            } else {
                Some(color_raw)
            };
            out.push(crate::types::EditGroup { name, color });
            p += 4 + nlen + 2;
        }
    }
    out
}

/// Decode the flat stem-mapping list inside a `0x4702` block. Same layout
/// as edit groups but without the trailing `i16` color.
fn parse_stem_mappings(blocks: &[Block], data: &[u8]) -> Vec<String> {
    let mut out = Vec::new();
    for b in collect_blocks_recursive(blocks, ContentType::StemMappingList) {
        let payload_start = b.offset.saturating_sub(7) + 9;
        let block_end = b.offset.saturating_sub(7) + b.block_size as usize;
        if payload_start >= data.len() || block_end > data.len() {
            continue;
        }
        // The list starts almost immediately — first u32 namelen sits a
        // few bytes into the payload. Scan for the first valid entry.
        let mut p = payload_start;
        let mut found_start = None;
        while p + 4 < block_end {
            let nlen = u32::from_le_bytes(data[p..p + 4].try_into().unwrap()) as usize;
            if (2..=64).contains(&nlen) && p + 4 + nlen <= block_end {
                let name = &data[p + 4..p + 4 + nlen];
                if name.iter().all(|c| (0x20..0x7f).contains(c)) {
                    found_start = Some(p);
                    break;
                }
            }
            p += 1;
        }
        let Some(mut p) = found_start else { continue };
        while p + 4 < block_end {
            let nlen = u32::from_le_bytes(data[p..p + 4].try_into().unwrap()) as usize;
            if !(1..=64).contains(&nlen) || p + 4 + nlen > block_end {
                break;
            }
            let name_bytes = &data[p + 4..p + 4 + nlen];
            if !name_bytes.iter().all(|c| (0x20..0x7f).contains(c)) {
                break;
            }
            out.push(String::from_utf8_lossy(name_bytes).into_owned());
            p += 4 + nlen;
        }
    }
    out
}

/// Collect every block (and nested child) of the given content type.
fn collect_blocks_recursive(blocks: &[Block], ct: ContentType) -> Vec<&Block> {
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
