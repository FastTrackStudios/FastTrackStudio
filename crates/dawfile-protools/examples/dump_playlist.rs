//! Dump the full bytes of a single 0x1052 (playlist) block by index.

use dawfile_protools::{block, content_type::ContentType, cursor::Cursor, decrypt};
use std::env;

fn main() {
    let path = env::args()
        .nth(1)
        .expect("usage: dump_playlist <file> <playlist_idx>");
    let pi: usize = env::args().nth(2).expect("idx").parse().unwrap();

    let mut data = std::fs::read(&path).expect("read");
    let _ = decrypt::decrypt(&mut data).expect("decrypt");

    let is_be = data.get(0x11).copied().unwrap_or(0) != 0;
    let cursor = Cursor::new(&data, is_be);
    let blocks = block::parse_blocks(&data, is_be);

    fn find_top<'a>(blocks: &'a [block::Block], ct: ContentType) -> Option<&'a block::Block> {
        blocks.iter().find(|b| b.content_type == Some(ct))
    }

    let map = find_top(&blocks, ContentType::AudioRegionTrackMapNew).expect("map");
    let entries = map.find_all(ContentType::AudioRegionTrackMapEntriesNew);
    let pl = entries[pi];
    let pl_start = pl.offset;
    let pl_end = pl_start + pl.block_size as usize;
    let name_off = pl.offset + 2;
    let (name, _) = cursor.length_prefixed_string(name_off);
    println!(
        "Playlist[{}] {:?}  @0x{:x}  size={}",
        pi, name, pl_start, pl.block_size
    );

    for (row, chunk) in data[pl_start..pl_end].chunks(16).enumerate() {
        let off = row * 16;
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
            "  +{:04x} (abs 0x{:06x}): {:48}  |{}|",
            off,
            pl_start + off,
            hex.join(" "),
            ascii
        );
    }
}
