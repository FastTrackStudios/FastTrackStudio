//! Dump bytes of one MIDI chunk and find which columns vary across events,
//! to locate note/velocity/duration fields.

use dawfile_protools::block::{self, Block};
use dawfile_protools::content_type::ContentType;
use dawfile_protools::decrypt;

const MAGIC: &[u8] = b"MdNLB";

fn find_blocks_recursive<'a>(blocks: &'a [Block], ct: ContentType, out: &mut Vec<&'a Block>) {
    for b in blocks {
        if b.content_type == Some(ct) {
            out.push(b);
        }
        find_blocks_recursive(&b.children, ct, out);
    }
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let path = std::env::args().nth(1).expect("path");
    let chunk_idx: usize = std::env::args()
        .nth(2)
        .map(|s| s.parse().unwrap())
        .unwrap_or(0);

    let mut data = std::fs::read(&path)?;
    let _ = decrypt::decrypt(&mut data)?;
    let is_be = data.get(0x11).copied().unwrap_or(0) != 0;
    let blocks = block::parse_blocks(&data, is_be);
    let mut midi_blocks = Vec::new();
    find_blocks_recursive(&blocks, ContentType::MidiEventsBlock, &mut midi_blocks);

    let mut all_magics = Vec::new();
    for b in &midi_blocks {
        let start = b.offset;
        let end = (b.offset + b.block_size as usize).min(data.len());
        let mut p = start;
        while let Some(idx) = data[p..end].windows(MAGIC.len()).position(|w| w == MAGIC) {
            all_magics.push(p + idx);
            p = p + idx + MAGIC.len();
        }
    }

    let mp = all_magics[chunk_idx];
    let n_events = u32::from_le_bytes(data[mp + 11..mp + 15].try_into().unwrap()) as usize;
    println!("chunk #{} at 0x{:x}, n_events={}", chunk_idx, mp, n_events);

    // Try stride = 35, events_start = +23
    let es = 23;
    let stride = 35;
    println!("\nEvent bytes (stride=35, events_start=+23):");

    // Collect first 12 events as byte arrays
    let mut events: Vec<&[u8]> = Vec::new();
    for i in 0..n_events.min(12) {
        let ev = mp + es + i * stride;
        if ev + stride > data.len() {
            break;
        }
        events.push(&data[ev..ev + stride]);
    }

    // Diff map: which columns vary across all events (not just first 12)
    let mut full_events: Vec<&[u8]> = Vec::new();
    for i in 0..n_events {
        let ev = mp + es + i * stride;
        if ev + stride > data.len() {
            break;
        }
        full_events.push(&data[ev..ev + stride]);
    }
    print!("var map (X=varies, .=constant): ");
    for col in 0..stride {
        let first = full_events[0][col];
        let varies = full_events.iter().any(|e| e[col] != first);
        print!("{}", if varies { 'X' } else { '.' });
    }
    println!();
    print!("col idx (hex):                  ");
    for col in 0..stride {
        print!("{:x}", col % 16);
    }
    println!();

    // Dump first 8 events with column markers
    for (i, e) in events.iter().enumerate().take(8) {
        let hex: String = e.iter().map(|b| format!("{:02x} ", b)).collect();
        // u8 candidate fields at common byte positions, u40 at +0, +9, etc.
        let pos_at_27 =
            u64::from_le_bytes([e[27], e[28], e[29], e[30], e[31], e[32], e[33], e[34]]);
        println!("ev[{:3}] {}  pos@+27={}", i, hex, pos_at_27);
    }

    // For each column, show distribution of values (helps identify note byte: 0-127 range)
    println!("\nPer-column histogram (values that appear):");
    for col in 0..stride {
        let mut vals: Vec<u8> = full_events.iter().map(|e| e[col]).collect();
        vals.sort_unstable();
        let unique: std::collections::BTreeSet<u8> = vals.iter().copied().collect();
        if unique.len() > 1 {
            let mn = *unique.iter().min().unwrap();
            let mx = *unique.iter().max().unwrap();
            let sample: Vec<u8> = unique.iter().take(8).copied().collect();
            println!(
                "  col +{:2}: {} unique, min=0x{:02x}({}), max=0x{:02x}({}), sample={:?}",
                col,
                unique.len(),
                mn,
                mn,
                mx,
                mx,
                sample
            );
        }
    }

    Ok(())
}
