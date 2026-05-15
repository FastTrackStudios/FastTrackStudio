//! Dump every block content_type seen in the file with counts and sample sizes.

use dawfile_protools::{block, decrypt};
use std::collections::BTreeMap;
use std::env;

fn main() {
    let path = env::args().nth(1).expect("usage: dump_block_types <file>");
    let mut data = std::fs::read(&path).expect("read");
    let _ = decrypt::decrypt(&mut data).expect("decrypt");

    let is_be = data.get(0x11).copied().unwrap_or(0) != 0;
    let blocks = block::parse_blocks(&data, is_be);

    let mut stats: BTreeMap<u16, (usize, usize, usize)> = BTreeMap::new(); // ct -> (count, min_size, max_size)

    fn walk(b: &block::Block, stats: &mut BTreeMap<u16, (usize, usize, usize)>) {
        let e = stats
            .entry(b.content_type_raw)
            .or_insert((0, usize::MAX, 0));
        e.0 += 1;
        e.1 = e.1.min(b.block_size as usize);
        e.2 = e.2.max(b.block_size as usize);
        for c in &b.children {
            walk(c, stats);
        }
    }
    for b in &blocks {
        walk(b, &mut stats);
    }

    println!("ct      count   min      max");
    for (ct, (c, mn, mx)) in &stats {
        println!("0x{:04x}  {:5}   {:8} {:8}", ct, c, mn, mx);
    }
}
