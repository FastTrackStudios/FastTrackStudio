//! Dump raw byte-level structure of every fade track entry in a session
//! file, plus the audio region it references. Used for reverse-engineering
//! the fade-length encoding.
//!
//! Usage: cargo run --example dump_fades -- <path-to-.ptx>

use dawfile_protools::{block, content_type::ContentType, cursor::Cursor, decrypt};
use std::env;

fn main() {
    let path = env::args().nth(1).expect("usage: dump_fades <file>");
    let mut data = std::fs::read(&path).expect("read");
    let _ = decrypt::decrypt(&mut data).expect("decrypt");

    let is_be = data.get(0x11).copied().unwrap_or(0) != 0;
    let cursor = Cursor::new(&data, is_be);
    let blocks = block::parse_blocks(&data, is_be);

    println!("is_bigendian: {is_be}");

    let map_block = find_top_level(&blocks, ContentType::AudioRegionTrackMapNew)
        .expect("AudioRegionTrackMapNew");

    let map_entries = map_block.find_all(ContentType::AudioRegionTrackMapEntriesNew);
    println!("playlist count: {}", map_entries.len());

    let mut fade_count = 0usize;

    for (pi, map_entry) in map_entries.iter().enumerate() {
        let playlist_name = {
            let no = map_entry.offset + 2;
            let (n, _) = cursor.length_prefixed_string(no);
            n
        };

        for track_entry in &map_entry.find_all(ContentType::AudioRegionTrackEntryNew) {
            let te_off = track_entry.offset;
            let te_size = track_entry.block_size as usize;
            let te_end = te_off + te_size;
            let is_fade = te_off + 47 <= data.len() && data[te_off + 46] == 0x01;

            if !is_fade {
                continue;
            }

            fade_count += 1;
            println!(
                "\n=== FADE #{} on playlist[{}] {:?} ===",
                fade_count, pi, playlist_name
            );
            println!(
                "  TrackEntry @0x{:x}  block_size={}  end=0x{:x}",
                te_off, te_size, te_end
            );
            // Dump the FULL track entry payload as hex
            let end_show = te_end.min(data.len());
            println!("  TrackEntry bytes ({} bytes total):", end_show - te_off);
            print_hex_with_offsets(&data[te_off..end_show], te_off, 0);

            // Dump each sub-entry
            for sub in &track_entry.find_all(ContentType::AudioRegionTrackSubEntryNew) {
                let so = sub.offset;
                let ss = sub.block_size as usize;
                let se = (so + ss).min(data.len());
                let raw_index = cursor.u32_at(so + 4) as u16;
                println!(
                    "    SubEntry @0x{:x} size={} region_index={} (0x{:04x})",
                    so, ss, raw_index, raw_index
                );
                println!("    SubEntry bytes:");
                print_hex_with_offsets(&data[so..se], so, 4);
            }
        }
    }

    println!("\nTotal fade entries: {}", fade_count);
}

fn find_top_level(blocks: &[block::Block], ct: ContentType) -> Option<&block::Block> {
    blocks.iter().find(|b| b.content_type == Some(ct))
}

fn print_hex_with_offsets(bytes: &[u8], abs_base: usize, indent_extra: usize) {
    let indent = " ".repeat(2 + indent_extra);
    for (i, chunk) in bytes.chunks(16).enumerate() {
        let off = i * 16;
        let hex: Vec<String> = chunk.iter().map(|b| format!("{:02x}", b)).collect();
        let ascii: String = chunk
            .iter()
            .map(|&b| {
                if (0x20..0x7f).contains(&b) {
                    b as char
                } else {
                    '.'
                }
            })
            .collect();
        println!(
            "{}+{:04x} (abs 0x{:06x}): {:48}  |{}|",
            indent,
            off,
            abs_base + off,
            hex.join(" "),
            ascii
        );
    }
}
