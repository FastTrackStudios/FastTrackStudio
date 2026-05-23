//! MIDI parsing: events, regions, and track assignments.
//!
//! MIDI data is stored as event chunks (block 0x2000), which are then
//! mapped to regions (0x2002/0x2634) and finally to tracks (0x1058).

use crate::block::Block;
use crate::content_type::ContentType;
use crate::cursor::{self, Cursor};
use crate::parse::tempo::{TempoSegment, tick_to_sample};
use crate::types::{MidiEvent, MidiRegion, NO_REGION, Track, TrackKind, TrackRegion, ZERO_TICKS};

/// Magic marker that precedes MIDI event data within a 0x2000 block.
const MIDI_MAGIC: &[u8] = b"MdNLB";

/// A raw MIDI event chunk before region assignment.
#[derive(Debug, Clone)]
struct MidiChunk {
    events: Vec<MidiEvent>,
    zero_ticks: u64,
    max_pos: u64,
}

/// Parse all MIDI data: events, regions, and tracks.
pub fn parse_midi(
    blocks: &[Block],
    cursor: &Cursor<'_>,
    version: u16,
    rate_factor: f64,
    tempo_segments: &[TempoSegment],
    target_sample_rate: u32,
) -> (Vec<MidiRegion>, Vec<Track>) {
    // Pass 1: Parse raw MIDI event chunks
    let chunks = parse_midi_chunks(blocks, cursor);

    // Pass 2: Map chunks to MIDI regions
    let regions = parse_midi_regions(
        blocks,
        cursor,
        &chunks,
        version,
        rate_factor,
        tempo_segments,
        target_sample_rate,
    );

    // Pass 3: Parse MIDI tracks and assign regions
    let tracks = parse_midi_tracks(
        blocks,
        cursor,
        &regions,
        rate_factor,
        tempo_segments,
        target_sample_rate,
    );

    (regions, tracks)
}

/// Parse raw MIDI event chunks from 0x2000 blocks.
///
/// ## Chunk layout (PT 10+)
///
/// ```text
/// [+0..+5]   magic "MdNLB"
/// [+11..+15] u32 n_events
/// [+15..+23] u64 zero_ticks (chunk position baseline, LE)
/// [+23..]    n_events × 35-byte event records
/// ```
///
/// ## Event record (35 bytes, relative to record start)
///
/// ```text
/// [+0]       u8     extra/source note (often equal to +9; semantics unclear)
/// [+1..+8]   bytes  flags / extra metadata
/// [+9]       u8     MIDI note number (0..127)
/// [+10]      u8     velocity (0..127)
/// [+11..+19] i64 LE duration in ticks (negative = paired note-off; skip)
/// [+19..+27] bytes  sign-extension / padding
/// [+27..+35] u64 LE absolute tick position (same encoding as chunk zero_ticks)
/// ```
///
/// Position relative to the chunk start = `position_u64 - zero_ticks_u64`.
/// Both share the same `0x4000_00e8_...` upper-byte pattern that PT uses for
/// timestamps; subtracting cancels the constant prefix.
fn parse_midi_chunks(blocks: &[Block], cursor: &Cursor<'_>) -> Vec<MidiChunk> {
    let mut chunks = Vec::new();
    let data = cursor.data();

    let midi_blocks = find_all_recursive(blocks, ContentType::MidiEventsBlock);

    const EVENTS_START_OFFSET: usize = 23;
    const EVENT_STRIDE: usize = 35;
    const POS_OFFSET: usize = 27;
    const NOTE_OFFSET: usize = 9;
    const VEL_OFFSET: usize = 10;
    const DUR_OFFSET: usize = 11;

    for block in midi_blocks {
        // Scan for ALL MdNLB magic markers within the block.
        let block_end = (block.offset + block.block_size as usize).min(data.len());
        let mut search_start = block.offset;

        while let Some(magic_pos) = find_magic(data, search_start, block_end) {
            search_start = magic_pos + MIDI_MAGIC.len();

            let n_events_offset = magic_pos + 11;
            if n_events_offset + 4 > data.len() {
                break;
            }
            let n_events = cursor.u32_at(n_events_offset) as usize;

            // zero_ticks is an 8-byte field (u64 LE) — same encoding as the
            // per-event position field, so subtracting yields relative ticks.
            let zt_offset = magic_pos + 15;
            if zt_offset + 8 > data.len() {
                break;
            }
            let zero_ticks_u64 =
                u64::from_le_bytes(data[zt_offset..zt_offset + 8].try_into().unwrap());

            let events_start = magic_pos + EVENTS_START_OFFSET;
            let mut events = Vec::with_capacity(n_events);
            let mut max_pos: u64 = 0;

            for i in 0..n_events {
                let ev_offset = events_start + i * EVENT_STRIDE;
                if ev_offset + EVENT_STRIDE > data.len() {
                    break;
                }

                let note = data[ev_offset + NOTE_OFFSET];
                let velocity = data[ev_offset + VEL_OFFSET];

                // Duration field at +11 (8 bytes). PT stores this in two forms
                // depending on the source track / event type:
                //   * top byte 0x40 → `2^62 + ticks` (same baseline as the
                //     position field; the actual duration is `value - 2^62`)
                //   * top byte 0x00 → small positive u64, ticks directly
                //   * top byte 0xff → negative i64 = paired note-off record,
                //     skip to avoid duplicating notes
                //   * anything else → unknown, skip
                let dur_bytes: [u8; 8] = data[ev_offset + DUR_OFFSET..ev_offset + DUR_OFFSET + 8]
                    .try_into()
                    .unwrap();
                let dur_raw = u64::from_le_bytes(dur_bytes);
                const BASELINE: u64 = 0x4000_0000_0000_0000;
                let duration = match dur_bytes[7] {
                    0x00 => dur_raw,
                    0x40 => dur_raw.saturating_sub(BASELINE),
                    0xff => continue, // paired note-off
                    _ => continue,
                };
                // duration == 0 is legitimate (instantaneous click/drum hit)

                // Position is a u64 LE in absolute PT ticks. Subtract the
                // chunk's zero_ticks baseline (same encoding) to get the
                // tick offset from the chunk's reference point.
                let pos_bytes: [u8; 8] = data[ev_offset + POS_OFFSET..ev_offset + POS_OFFSET + 8]
                    .try_into()
                    .unwrap();
                let pos_abs = u64::from_le_bytes(pos_bytes);
                let relative_pos = pos_abs.saturating_sub(zero_ticks_u64);

                // Sanity check: drop events with implausible note numbers
                // (the dump tool showed a few records with byte values outside
                // 0..127 — likely format glitches or unused entries).
                if note > 127 || velocity > 127 {
                    continue;
                }

                if relative_pos > max_pos {
                    max_pos = relative_pos;
                }

                events.push(MidiEvent {
                    position: relative_pos,
                    duration,
                    note,
                    velocity,
                });
            }

            chunks.push(MidiChunk {
                events,
                // The legacy u40 form of zero_ticks is no longer used for
                // arithmetic, but downstream may inspect it for debugging.
                zero_ticks: zero_ticks_u64 & 0x0000_0000_ffff_ffff_ffff,
                max_pos,
            });
        }
    }

    chunks
}

/// Parse MIDI regions from region map blocks.
fn parse_midi_regions(
    blocks: &[Block],
    cursor: &Cursor<'_>,
    chunks: &[MidiChunk],
    version: u16,
    _rate_factor: f64,
    tempo_segments: &[TempoSegment],
    target_sample_rate: u32,
) -> Vec<MidiRegion> {
    let mut regions = Vec::new();

    // Choose block types based on version
    let (map_ct, region_ct) = if version < 10 {
        (ContentType::MidiRegionMapOld, ContentType::MidiRegionOld)
    } else {
        (ContentType::MidiRegionMapNew, ContentType::MidiRegionNew)
    };

    let region_map = match find_block_recursive(blocks, map_ct) {
        Some(b) => b,
        None => return regions,
    };

    let region_blocks = region_map.find_all(region_ct);

    for (idx, block) in region_blocks.iter().enumerate() {
        let data = cursor.data();

        // For PT 10+, the region data is inside a CompoundRegionGroup (0x2628) child.
        // For PT 5-9, the data is directly in the region block.
        let data_block = if version >= 10 {
            match block.find_child(ContentType::CompoundRegionGroup) {
                Some(child) => child,
                None => continue,
            }
        } else {
            block
        };

        // Region name at data_block.offset + 2
        let name_offset = data_block.offset + 2;
        if name_offset + 4 >= data.len() {
            continue;
        }
        let (name, str_consumed) = cursor.length_prefixed_string(name_offset);

        // The chunk index is a u32 right after the end of the data block
        // (at data_block.offset + data_block.block_size)
        let chunk_idx_offset = data_block.offset + data_block.block_size as usize;
        let chunk_idx = if chunk_idx_offset + 4 <= data.len() {
            cursor.u32_at(chunk_idx_offset) as usize
        } else {
            idx
        };

        // Look up the MIDI chunk
        let events = if chunk_idx < chunks.len() {
            chunks[chunk_idx].events.clone()
        } else {
            Vec::new()
        };

        // Region length: max_pos is in PT ticks (relative to the chunk's zero).
        // Convert to samples via the tempo map so downstream consumers can treat
        // `length` as samples like audio regions.
        let region_length_samples = if chunk_idx < chunks.len() {
            tick_to_sample(
                chunks[chunk_idx].max_pos,
                tempo_segments,
                target_sample_rate,
            )
        } else {
            0
        };

        regions.push(MidiRegion {
            name,
            index: idx as u16,
            start_pos: 0,
            sample_offset: 0,
            length: region_length_samples,
            events,
        });
    }

    regions
}

/// Strip a region name's trailing `-NN` placement suffix to recover its
/// playlist name. PT names MIDI regions `"<playlist>-<NN>"`, so
/// `"Inst 1.02-01"` → `"Inst 1.02"` and `"MIDI 1-01"` → `"MIDI 1"`. The
/// playlist name (WITH any `.01`/`.dup1` suffix) is exactly the active track's
/// name in the 0x251a list, so it's what we match tracks against.
fn region_playlist_name(region_name: &str) -> &str {
    if let Some(idx) = region_name.rfind('-') {
        let suffix = &region_name[idx + 1..];
        if !suffix.is_empty() && suffix.chars().all(|c| c.is_ascii_digit()) {
            return &region_name[..idx];
        }
    }
    region_name
}

/// Parse the non-audio tracks (MIDI / instrument / master / click / folder
/// dividers) from the 0x2519 track list and assign each its ACTIVE-playlist
/// regions.
///
/// The 0x2519 list enumerates EVERY track in the session. Audio tracks are
/// parsed separately from 0x1015 and dropped from this set by the caller (by
/// name); what remains is the non-audio tracks PT shows in the Edit window —
/// master, click, dividers and instrument/MIDI tracks — all of which must be
/// emitted to match the source layout even when they hold no regions.
///
/// Regions are matched to tracks BY NAME (not the old positional 0x1058 walk,
/// which scattered every region onto the wrong track): a region
/// `"Inst 1.02-01"` belongs to the track named `"Inst 1.02"`. Regions whose
/// playlist is an inactive alternate (e.g. `"Inst 1.01-*"` when `.02` is the
/// active track) match no track name and are left out, mirroring the audio
/// active/alternate-playlist model.
fn parse_midi_tracks(
    blocks: &[Block],
    cursor: &Cursor<'_>,
    regions: &[MidiRegion],
    rate_factor: f64,
    tempo_segments: &[TempoSegment],
    target_sample_rate: u32,
) -> Vec<Track> {
    let _ = (rate_factor, tempo_segments, target_sample_rate);
    let mut tracks = Vec::new();

    // Parse track definitions from 0x2519
    let track_list = match find_block_recursive(blocks, ContentType::MidiTrackList) {
        Some(b) => b,
        None => return tracks,
    };

    let data = cursor.data();
    let mut seen = std::collections::HashSet::new();
    for child in track_list.find_children(ContentType::MidiTrackInfo) {
        let name_offset = child.offset + 4;
        if name_offset + 4 >= data.len() {
            continue;
        }

        let (name, _str_consumed) = cursor.length_prefixed_string(name_offset);
        if name.is_empty() {
            continue;
        }
        // 0x2519 may carry a 2× tail copy of the whole list (and interleaved
        // alternates in converter-authored PTX); keep the first occurrence of
        // each name only.
        if !seen.insert(name.clone()) {
            continue;
        }

        // Active-playlist regions: those whose playlist part equals this
        // track's name. `region_index` is the array position so the emitter
        // can index `session.midi_regions` directly; `start_pos` comes from
        // the region's own decoded timeline position.
        let track_regions: Vec<TrackRegion> = regions
            .iter()
            .enumerate()
            .filter(|(_, r)| region_playlist_name(&r.name) == name)
            .map(|(ri, r)| TrackRegion {
                region_index: ri as u16,
                start_pos: r.start_pos,
                clip_flag_53: false,
                clip_muted: false,
                clip_color: None,
            })
            .collect();

        tracks.push(Track {
            name,
            kind: TrackKind::Midi,
            index: tracks.len() as u16,
            playlist_name: String::new(),
            fades: Vec::new(),
            regions: track_regions,
            volume_centibel: 0,
            mute: false,
            solo: false,
            solo_defeat: false,
            inactive: false,
            pan: 0,
            alternate_playlists: Vec::new(),
            output: String::new(),
            color_byte: 0,
            volume_automation: Vec::new(),
            mute_automation: Vec::new(),
            is_folder: false,
            display_order: u32::MAX,
        });
    }

    tracks
}

/// Find the MdNLB magic bytes within a range.
fn find_magic(data: &[u8], start: usize, end: usize) -> Option<usize> {
    let end = end.min(data.len());
    if end < start + MIDI_MAGIC.len() {
        return None;
    }
    data[start..end - MIDI_MAGIC.len() + 1]
        .windows(MIDI_MAGIC.len())
        .position(|w| w == MIDI_MAGIC)
        .map(|p| start + p)
}

fn find_block_recursive(blocks: &[Block], ct: ContentType) -> Option<&Block> {
    for block in blocks {
        if block.content_type == Some(ct) {
            return Some(block);
        }
        if let Some(found) = find_block_recursive(&block.children, ct) {
            return Some(found);
        }
    }
    None
}

fn find_all_recursive(blocks: &[Block], ct: ContentType) -> Vec<&Block> {
    let mut result = Vec::new();
    for block in blocks {
        if block.content_type == Some(ct) {
            result.push(block);
        }
        result.extend(find_all_recursive(&block.children, ct));
    }
    result
}
