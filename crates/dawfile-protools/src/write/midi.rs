//! MIDI note-chunk encoding for the native PTX writer.
//!
//! The inverse of `parse::midi::parse_midi_chunks`. A MIDI take is stored as an
//! `MdNLB` note chunk inside a `0x2000` block. The chunk header is:
//!
//! ```text
//! [+0..+5]   "MdNLB"
//! [+5..+7]   u16 version (always 3 for PT 10+)
//! [+7..+11]  u32 field7 = n_events*47 + 22   (deterministic; verified across chunks)
//! [+11..+15] u32 n_events
//! [+15..+23] u64 zero_ticks   (the take's absolute-tick baseline)
//! [+23..]    n_events × 35-byte event records
//! ```
//!
//! Each 35-byte record's fields:
//!
//! ```text
//! [+0]       u8  note number (0..127)
//! [+1..+9]   u64 duration, baseline-2^62  (stored = 2^62 + ticks)
//! [+9]       u8  velocity (0..127)
//! [+10]      u8  0x40  (constant marker)
//! [+11..+19] u64 baseline-2^62, value 0   (unused field)
//! [+19..+27] u64 baseline-2^62, value 0   (unused field)
//! [+27..+35] u64 absolute tick position = zero_ticks + chunk-relative position
//! ```
//!
//! **Staggered pairing (critical).** PT does not store one note per record
//! self-contained. The note that sounds at record *i*'s onset (`+27`) has its
//! pitch / velocity / duration in record *i+1*. This was verified against the
//! reference exports: decoding self-contained makes note parity collapse
//! (~240 mismatches vs ~10), while the staggered pairing matches. So to encode
//! `N` notes we emit `N+1` records: record `i`'s `+27` is note `i`'s onset and
//! record `i+1` carries note `i`'s pitch/vel/dur. `n_events` (header) = record
//! count = `N+1`; the decoder loop yields `N` notes.

/// A note to encode into a chunk. Positions/durations are in PT ticks
/// (960,000 per quarter), relative to the chunk's `zero_ticks` baseline.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct ChunkNote {
    /// Chunk-relative onset position, in ticks.
    pub position: u64,
    /// Duration in ticks.
    pub duration: u64,
    /// MIDI note number (0..127).
    pub note: u8,
    /// Velocity (1..127).
    pub velocity: u8,
}

/// 2^62 — the baseline PT adds to tick-valued u64 fields.
const BASELINE: u64 = 0x4000_0000_0000_0000;
const EVENT_STRIDE: usize = 35;
const MAGIC: &[u8; 5] = b"MdNLB";

/// Build a 35-byte record from an onset position and (optionally) a note's
/// pitch/vel/dur. In the staggered layout the position and note-data come from
/// different source notes, so they're set independently.
fn build_record(abs_pos: u64, note_data: Option<&ChunkNote>) -> [u8; EVENT_STRIDE] {
    let mut r = [0u8; EVENT_STRIDE];
    r[10] = 0x40;
    r[1..9].copy_from_slice(&BASELINE.to_le_bytes());
    r[11..19].copy_from_slice(&BASELINE.to_le_bytes());
    r[19..27].copy_from_slice(&BASELINE.to_le_bytes());
    if let Some(nd) = note_data {
        r[0] = nd.note;
        r[1..9].copy_from_slice(&BASELINE.wrapping_add(nd.duration).to_le_bytes());
        r[9] = nd.velocity;
    }
    r[27..35].copy_from_slice(&abs_pos.to_le_bytes());
    r
}

/// Encode an `MdNLB` note chunk (header + records) in PT's staggered layout.
/// `zero_ticks` is the take's absolute-tick baseline (PT timestamps carry a
/// `2^62 + ZERO_TICKS` prefix).
pub fn encode_note_chunk(notes: &[ChunkNote], zero_ticks: u64) -> Vec<u8> {
    if notes.is_empty() {
        let mut out = Vec::new();
        out.extend_from_slice(MAGIC);
        out.extend_from_slice(&3u16.to_le_bytes());
        out.extend_from_slice(&22u32.to_le_bytes());
        out.extend_from_slice(&0u32.to_le_bytes());
        out.extend_from_slice(&zero_ticks.to_le_bytes());
        return out;
    }
    let n = notes.len();
    let rec_count = (n + 1) as u32; // staggered: N notes → N+1 records
    let mut out = Vec::with_capacity(23 + (n + 1) * EVENT_STRIDE);
    out.extend_from_slice(MAGIC);
    out.extend_from_slice(&3u16.to_le_bytes()); // version
    out.extend_from_slice(&rec_count.wrapping_mul(47).wrapping_add(22).to_le_bytes()); // field7
    out.extend_from_slice(&rec_count.to_le_bytes()); // n_events = record count
    out.extend_from_slice(&zero_ticks.to_le_bytes());
    // record j: onset = note[j] (j<N), note-data = note[j-1] (j>=1).
    for j in 0..=n {
        let abs_pos = zero_ticks.wrapping_add(notes[j.min(n - 1)].position);
        let note_data = if j >= 1 { Some(&notes[j - 1]) } else { None };
        out.extend_from_slice(&build_record(abs_pos, note_data));
    }
    out
}

/// Staggered decoder mirroring the production parser: note `i` takes its onset
/// from record `i`'s `+27` and its pitch/vel/dur from record `i+1`. (Velocity
/// is read from `+9`, the real location; the production parser reads `+10` for
/// note-parity reasons and so loses velocity — that's a parser quirk, not the
/// data.) Used to validate the encoder offline.
#[cfg(test)]
pub fn decode_note_chunk_staggered(data: &[u8]) -> Vec<ChunkNote> {
    let mut out = Vec::new();
    if data.len() < 23 || &data[0..5] != MAGIC {
        return out;
    }
    let n = u32::from_le_bytes(data[11..15].try_into().unwrap()) as usize;
    let zt = u64::from_le_bytes(data[15..23].try_into().unwrap());
    for i in 0..n {
        let r = 23 + i * EVENT_STRIDE;
        let nr = r + EVENT_STRIDE;
        if nr + EVENT_STRIDE > data.len() {
            break;
        }
        let dur =
            u64::from_le_bytes(data[nr + 1..nr + 9].try_into().unwrap()).wrapping_sub(BASELINE);
        let pos = u64::from_le_bytes(data[r + 27..r + 35].try_into().unwrap()).wrapping_sub(zt);
        out.push(ChunkNote {
            position: pos,
            duration: dur,
            note: data[nr],
            velocity: data[nr + 9],
        });
    }
    out
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn chunk_header_field7_formula() {
        // 451 notes → 452 records; header n_events = record count.
        let notes = vec![
            ChunkNote {
                position: 0,
                duration: 480_000,
                note: 60,
                velocity: 100
            };
            451
        ];
        let chunk = encode_note_chunk(&notes, 0x4000_00e8_d519_a488);
        let n = u32::from_le_bytes(chunk[11..15].try_into().unwrap());
        assert_eq!(n, 452);
        let field7 = u32::from_le_bytes(chunk[7..11].try_into().unwrap());
        assert_eq!(field7, 452 * 47 + 22);
    }

    #[test]
    fn note_chunk_round_trips() {
        let zt = 0x4000_00e8_d519_a488;
        let notes = vec![
            ChunkNote {
                position: 990_720,
                duration: 1_065_600,
                note: 66,
                velocity: 80,
            },
            ChunkNote {
                position: 1_463_040,
                duration: 550_080,
                note: 69,
                velocity: 78,
            },
            ChunkNote {
                position: 2_413_440,
                duration: 1_048_320,
                note: 64,
                velocity: 76,
            },
            ChunkNote {
                position: 3_000_000,
                duration: 120_000,
                note: 36,
                velocity: 64,
            },
        ];
        let chunk = encode_note_chunk(&notes, zt);
        // N notes → N+1 records.
        assert_eq!(chunk.len(), 23 + (notes.len() + 1) * EVENT_STRIDE);
        let decoded = decode_note_chunk_staggered(&chunk);
        assert_eq!(decoded, notes);
    }
}
