//! Solo-state decoder. Each track has a `0x102d` (per-track state) block
//! whose payload byte at offset `+162` is the solo flag (u8, 0/1).
//!
//! Verified via RPP→PTX probe + plaintext-diff (2026-05-17): a track
//! with `.soloed()` set in RPP produces `0x102d +162 = 0x01` in PTX;
//! baseline reads `0x00`. See `docs/pt-field-map.md`.

use crate::block::Block;
use crate::cursor::Cursor;
use crate::types::Track;
use std::collections::HashMap;

/// Build a `name → solo` map by walking every `0x102d` (raw u16 — not
/// yet a named `ContentType` variant), reading the child `0x2619`
/// (track name), and the byte at payload `+162`.
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

/// Apply solo state to every audio + MIDI track in the lists.
///
/// PT writes one `0x102d` per track in document order (including Master,
/// Click, etc.). We look up each parsed track by name first, then with
/// the common active-playlist suffixes (`.01`..`.05`) since audio
/// tracks may store the base name while the `0x102d`'s 0x2619 child
/// uses the playlist name.
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
            continue;
        }
        for suffix in [".01", ".02", ".03", ".04", ".05"] {
            if let Some(s) = solo_by_name.get(&format!("{}{suffix}", t.name)).copied() {
                t.solo = s;
                break;
            }
        }
    }
    for t in midi_tracks.iter_mut() {
        if let Some(s) = solo_by_name.get(&t.name).copied() {
            t.solo = s;
        }
    }
}
