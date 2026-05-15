//! Dump every block of a given content type as hex.

use dawfile_protools::{block, decrypt};
use std::env;

fn main() {
    let path = env::args().nth(1).expect("usage: dump_ct <file> <ct_hex>");
    let ct_hex = env::args().nth(2).expect("missing ct hex");
    let target_ct = u16::from_str_radix(ct_hex.trim_start_matches("0x"), 16).expect("bad hex");

    let mut data = std::fs::read(&path).expect("read");
    let _ = decrypt::decrypt(&mut data).expect("decrypt");

    let is_be = data.get(0x11).copied().unwrap_or(0) != 0;
    let blocks = block::parse_blocks(&data, is_be);

    let mut found: Vec<&block::Block> = Vec::new();
    fn walk<'a>(b: &'a block::Block, target: u16, out: &mut Vec<&'a block::Block>) {
        if b.content_type_raw == target {
            out.push(b);
        }
        for c in &b.children {
            walk(c, target, out);
        }
    }
    for b in &blocks {
        walk(b, target_ct, &mut found);
    }

    println!("Found {} blocks of type 0x{:04x}\n", found.len(), target_ct);
    for (i, b) in found.iter().enumerate() {
        let start = b.offset;
        let end = (b.offset + b.block_size as usize).min(data.len());
        let len = end - start;
        println!(
            "[{}] @0x{:x}  block_size={}  bytes={}",
            i, start, b.block_size, len
        );
        for (row, chunk) in data[start..end].chunks(16).enumerate() {
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
                start + off,
                hex.join(" "),
                ascii
            );
        }
        println!();
    }
}
