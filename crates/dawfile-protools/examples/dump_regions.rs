//! Dump all audio regions with their lengths so we can correlate fade
//! references to region durations.

use dawfile_protools::{
    block,
    content_type::ContentType,
    cursor::{self, Cursor},
    decrypt,
};
use std::env;

fn main() {
    let path = env::args().nth(1).expect("usage: dump_regions <file>");
    let mut data = std::fs::read(&path).expect("read");
    let _ = decrypt::decrypt(&mut data).expect("decrypt");

    let is_be = data.get(0x11).copied().unwrap_or(0) != 0;
    let cursor = Cursor::new(&data, is_be);
    let blocks = block::parse_blocks(&data, is_be);

    // Find AudioRegionListNew
    fn find_recursive<'a>(blocks: &'a [block::Block], ct: ContentType) -> Option<&'a block::Block> {
        for b in blocks {
            if b.content_type == Some(ct) {
                return Some(b);
            }
            if let Some(f) = find_recursive(&b.children, ct) {
                return Some(f);
            }
        }
        None
    }

    let list =
        find_recursive(&blocks, ContentType::AudioRegionListNew).expect("AudioRegionListNew");
    let regions = list.find_all(ContentType::AudioRegionNew);

    println!("idx | name                                    | start         length         offset");
    for (idx, b) in regions.iter().enumerate() {
        let name_off = b.offset + 11;
        if name_off + 4 >= data.len() {
            continue;
        }
        let (name, consumed) = cursor.length_prefixed_string(name_off);
        let tp = name_off + consumed;
        if tp + 20 >= data.len() {
            continue;
        }
        let (start, soff, length) = cursor::parse_three_point(&cursor, tp);
        println!(
            "{:3} | {:40} | {:13} {:13} {:13}",
            idx, &name, start, length, soff
        );
    }
}
