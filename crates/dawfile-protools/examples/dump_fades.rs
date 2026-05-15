//! For each fade track-entry, print the referenced region's length so we can
//! verify fade-length comes from the audio_regions table.

use dawfile_protools::block::{self, Block};
use dawfile_protools::content_type::ContentType;
use dawfile_protools::cursor::Cursor;
use dawfile_protools::decrypt;

fn find_all<'a>(blocks: &'a [Block], ct: ContentType, out: &mut Vec<&'a Block>) {
    for b in blocks {
        if b.content_type == Some(ct) {
            out.push(b);
        }
        find_all(&b.children, ct, out);
    }
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let path = std::env::args().nth(1).expect("path");
    let s = dawfile_protools::read_session(&path, 48000)?;
    // Re-parse raw to inspect raw bytes
    let mut data = std::fs::read(&path)?;
    let _ = decrypt::decrypt(&mut data)?;
    let is_be = data.get(0x11).copied().unwrap_or(0) != 0;
    let blocks = block::parse_blocks(&data, is_be);
    let cursor = Cursor::new(&data, is_be);

    let mut entries = Vec::new();
    find_all(&blocks, ContentType::AudioRegionTrackEntryNew, &mut entries);

    println!("audio_regions table has {} entries", s.audio_regions.len());

    for (i, e) in entries.iter().enumerate() {
        if e.offset + 47 > data.len() {
            continue;
        }
        let flag = data[e.offset + 46];
        if flag != 0x01 {
            continue;
        }
        // Find the child sub-entry
        let sub = e
            .children
            .iter()
            .find(|c| c.content_type == Some(ContentType::AudioRegionTrackSubEntryNew));
        let Some(sub) = sub else { continue };

        // Region index = u32 at sub.offset + 4
        let raw_offset = sub.offset + 4;
        if raw_offset + 4 > data.len() {
            continue;
        }
        let raw_index = cursor.u32_at(raw_offset) as u16;
        // Start position = u32 at sub.offset + 9 (samples) OR u40 if +16 == 0x40
        let start_offset = sub.offset + 9;
        let is_tick = sub.offset + 17 <= data.len() && cursor.u8_at(sub.offset + 16) == 0x40;
        let start_samples = if !is_tick && start_offset + 4 <= data.len() {
            cursor.u32_at(start_offset) as u64
        } else {
            0
        };

        // Look up region
        let region = s.audio_regions.iter().find(|r| r.index == raw_index);
        let (rname, rlen, rstart, rfile) = region
            .map(|r| (r.name.clone(), r.length, r.start_pos, r.audio_file_index))
            .unwrap_or((String::from("?"), 0, 0, 0));

        println!(
            "fade #{:2}: entry_off=0x{:x} sub_off=0x{:x} reg_idx={} start_samples={} reg={:?} reg_len={}",
            i, e.offset, sub.offset, raw_index, start_samples, rname, rlen
        );
        let _ = (rstart, rfile);
        // Dump sub-entry bytes (37 bytes after content_type)
        let sub_end = (sub.offset + sub.block_size as usize).min(data.len());
        let bytes: Vec<String> = data[sub.offset..sub_end]
            .iter()
            .map(|b| format!("{:02x}", b))
            .collect();
        println!(
            "    sub_entry [{} bytes]: {}",
            sub_end - sub.offset,
            bytes.join(" ")
        );
    }
    Ok(())
}
