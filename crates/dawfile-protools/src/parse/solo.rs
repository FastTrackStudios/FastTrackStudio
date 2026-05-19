//! Solo + solo-defeat decoder.
//!
//! - Solo: `0x102d +162` (u8 0/1). Verified by RPP→PTX probe.
//! - Solo-defeat: `0x200b +268` (u8 0/1), mirror at `0x200a +259`.
//!   Verified by RPP→PTX probe (`.solo_defeated()`).
//!
//! See `docs/pt-field-map.md`.

use crate::block::Block;
use crate::content_type::ContentType;
use crate::cursor::Cursor;
use crate::types::Track;
use std::collections::HashMap;

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

/// Collect `name → solo_defeat` from `0x200b +268`. Uses the same
/// ancestor-walking 0x2619 name resolution as the color decoder
/// (handles both flat and deeply-nested track-name structures).
fn collect_solo_defeat_by_name(blocks: &[Block], data: &[u8]) -> HashMap<String, bool> {
    let mut parents: HashMap<usize, Option<&Block>> = HashMap::new();
    fn build_parents<'a>(
        blocks: &'a [Block],
        parent: Option<&'a Block>,
        out: &mut HashMap<usize, Option<&'a Block>>,
    ) {
        for b in blocks {
            out.insert(b.offset, parent);
            build_parents(&b.children, Some(b), out);
        }
    }
    build_parents(blocks, None, &mut parents);

    fn find_2619(b: &Block, data: &[u8]) -> Option<String> {
        for c in &b.children {
            if c.content_type == Some(ContentType::MarkerEntry) {
                let p = c.offset + 2;
                if p + 4 > data.len() {
                    return None;
                }
                let len = u32::from_le_bytes(data[p..p + 4].try_into().ok()?) as usize;
                if len == 0 || len > 64 || p + 4 + len > data.len() {
                    return None;
                }
                return Some(
                    String::from_utf8_lossy(&data[p + 4..p + 4 + len])
                        .trim_end_matches('\0')
                        .to_string(),
                );
            }
            if let Some(n) = find_2619(c, data) {
                return Some(n);
            }
        }
        None
    }

    let mut out: HashMap<String, bool> = HashMap::new();
    let aux_blocks = crate::parse::collect_blocks_recursive(blocks, ContentType::TrackAuxState);
    for b in &aux_blocks {
        let p = b.offset + 2 + 268;
        if p >= data.len() {
            continue;
        }
        let defeat = data[p] != 0;

        let mut anc = parents.get(&b.offset).copied().flatten();
        let mut depth = 0;
        let mut name: Option<String> = None;
        while let Some(a) = anc {
            if let Some(n) = find_2619(a, data) {
                name = Some(n);
                break;
            }
            anc = parents.get(&a.offset).copied().flatten();
            depth += 1;
            if depth > 10 {
                break;
            }
        }
        if let Some(name) = name {
            out.entry(name).or_insert(defeat);
        }
    }
    out
}

/// Apply solo + solo-defeat to every audio + MIDI track.
pub fn apply_solo_state(
    blocks: &[Block],
    cursor: &Cursor<'_>,
    audio_tracks: &mut [Track],
    midi_tracks: &mut [Track],
) {
    let data = cursor.data();
    let solo_by_name = collect_solo_by_name(blocks, data);
    let defeat_by_name = collect_solo_defeat_by_name(blocks, data);

    let lookup = |map: &HashMap<String, bool>, name: &str| -> Option<bool> {
        if let Some(v) = map.get(name).copied() {
            return Some(v);
        }
        for suffix in [".01", ".02", ".03", ".04", ".05"] {
            if let Some(v) = map.get(&format!("{}{suffix}", name)).copied() {
                return Some(v);
            }
        }
        None
    };

    for t in audio_tracks.iter_mut() {
        if let Some(s) = lookup(&solo_by_name, &t.name) {
            t.solo = s;
        }
        if let Some(d) = lookup(&defeat_by_name, &t.name) {
            t.solo_defeat = d;
        }
    }
    for t in midi_tracks.iter_mut() {
        if let Some(s) = solo_by_name.get(&t.name).copied() {
            t.solo = s;
        }
        if let Some(d) = defeat_by_name.get(&t.name).copied() {
            t.solo_defeat = d;
        }
    }
}
