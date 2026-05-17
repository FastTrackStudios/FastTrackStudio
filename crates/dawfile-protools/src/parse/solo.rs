//! Solo state decoding. Each track has a `0x102d` (per-track state) block
//! whose payload byte at `+162` is the solo flag (u8 0/1).
//!
//! Verified via RPP→PTX probe + plaintext-diff (2026-05-17): a track
//! with `.soloed()` in RPP produces `0x102d +162 = 0x01` in PTX; baseline
//! reads `0x00`. See `docs/pt-field-map.md`.

use crate::block::Block;
use crate::cursor::Cursor;
use crate::types::Track;
use std::collections::HashMap;

/// Walk every `0x102d` (raw u16, no named ContentType yet) in the block
/// tree, pair each with its child `0x2619` (name), and produce a map
/// `name → solo`.
fn collect_solo_by_name(blocks: &[Block], data: &[u8]) -> HashMap<String, bool> {
    let mut out: HashMap<String, bool> = HashMap::new();

    fn walk(blocks: &[Block], data: &[u8], out: &mut HashMap<String, bool>) {
        for b in blocks {
            if b.content_type_raw == 0x102d {
                let p162 = b.offset + 2 + 162;
                let solo = p162 < data.len() && data[p162] != 0;
                let name = b.children.iter().find_map(|c| {
                    if c.content_type_raw != 0x2619 {
                        return None;
                    }
                    let p = c.offset + 2;
                    if p + 4 > data.len() {
                        return None;
                    }
                    let len = u32::from_le_bytes(data[p..p + 4].try_into().ok()?) as usize;
                    if len == 0 || len > 64 || p + 4 + len > data.len() {
                        return None;
                    }
                    Some(
                        String::from_utf8_lossy(&data[p + 4..p + 4 + len])
                            .trim_end_matches('\0')
                            .to_string(),
                    )
                });
                if let Some(name) = name {
                    out.entry(name).or_insert(solo);
                }
            }
            walk(&b.children, data, out);
        }
    }

    walk(blocks, data, &mut out);
    out
}

/// Apply solo state to every audio + MIDI track in `tracks`. Tries the
/// raw name first, then with common playlist suffixes (`.01`, etc.).
pub fn apply_solo_state(
    blocks: &[Block],
    cursor: &Cursor<'_>,
    audio_tracks: &mut [Track],
    midi_tracks: &mut [Track],
) {
    let data = cursor.data();
    let solo_by_name = collect_solo_by_name(blocks, data);

    for t in audio_tracks.iter_mut() {
        if let Some(s) = solo_by_name.get(&t.name).copied() {
            t.solo = s;
        } else {
            for suffix in [".01", ".02", ".03", ".04", ".05"] {
                if let Some(s) = solo_by_name.get(&format!("{}{suffix}", t.name)).copied() {
                    t.solo = s;
                    break;
                }
            }
        }
    }
    for t in midi_tracks.iter_mut() {
        if let Some(s) = solo_by_name.get(&t.name).copied() {
            t.solo = s;
        }
    }
}
