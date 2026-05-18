//! Native (template-patch) PTX writer.
//!
//! Strategy: start from a baseline PTX template (one default-state
//! track, generated once via the converter and embedded in this crate),
//! patch known field offsets to match the input, re-encrypt.
//!
//! Covers (single-track only for now):
//! - Track name (variable-length splice)
//! - Track color (`0x200a +97`, `0x200b +106`, `0x2015 +88` — i16 LE)
//! - Track mute (`0x1029 +5`, `0x260a[1] +26`, ...)
//! - Track solo (`0x102d +162`)
//! - Track volume (`0x260a[0] +26` i16 LE centibel)
//! - Track pan (`0x260a[2] +26` i16 LE)
//!
//! Multi-track is NOT yet supported — needs per-track block expansion
//! (the converter's "Phase 3b: reassemble" step that copies the 8
//! per-track block templates N times).
//!
//! Validation: a test compares native output against converter
//! shell-out output, ignoring the per-file UID region at `0x261b
//! +7433..+7581` (148-byte run of randomized bytes that differs
//! between any two converter invocations even with identical input).
//!
//! See `docs/pt-field-map.md` for the byte-level field map.

use crate::raw_block::RawSession;
use crate::write::splice::replace_string;

/// Single-track PTX writer parameters.
#[derive(Debug, Clone)]
pub struct NativeTrackSpec {
    pub name: String,
    /// PT palette index, or 0 for default.
    pub color: u8,
    pub mute: bool,
    pub solo: bool,
    /// Track volume in 0.1 dB units. 0 = unity.
    pub volume_centibel: i16,
    /// Pan -100..+100. -100 = full L (the default).
    pub pan: i16,
}

impl Default for NativeTrackSpec {
    fn default() -> Self {
        Self {
            name: "ProbeTrack".to_string(),
            color: 0,
            mute: false,
            solo: false,
            volume_centibel: 0,
            pan: -100,
        }
    }
}

/// Embedded single-track baseline template.
///
/// Generated 2026-05-17 by running:
/// `cargo run -p daw-reaper --example rpp_to_ptx_probe -- baseline`
/// then copying `/tmp/probe_baseline.ptx`.
///
/// This is an encrypted PTX. We decrypt → patch → re-encrypt.
const BASELINE_PTX: &[u8] =
    include_bytes!("../../tests/fixtures/native-writer/baseline-single-track.ptx");

/// Generate a single-track PTX file from `spec`. Returns the encrypted
/// PTX bytes ready to write to disk.
pub fn write_single_track_ptx(spec: &NativeTrackSpec) -> crate::PtResult<Vec<u8>> {
    // Decrypt baseline
    let mut session = crate::parse_raw(BASELINE_PTX.to_vec())?;

    // Step 1: rename the track from "ProbeTrack" to spec.name. After
    // splicing, the block tree offsets are stale; we walk the data
    // directly when patching numeric fields below, so the stale tree
    // is fine for fixed-offset writes inside the SAME block. But
    // rename SHIFTS later blocks, so we need to re-build the tree
    // without going through the decryption step again.
    let did_rename = if spec.name != "ProbeTrack" {
        rename_all_track_name_occurrences(&mut session, "ProbeTrack", &spec.name);
        true
    } else {
        false
    };
    if did_rename {
        // Re-parse the RAW block tree from the already-decrypted data
        // (do NOT go through parse_raw which would XOR-decrypt again).
        let is_be = session.is_bigendian;
        session.blocks = crate::raw_block::parse_raw_blocks_pub(&session.data, is_be);
    }

    // Step 2: patch numeric fields. These are all fixed-size so no splicing.
    patch_color(&mut session, spec.color);
    patch_mute(&mut session, spec.mute);
    patch_solo(&mut session, spec.solo);
    patch_volume(&mut session, spec.volume_centibel);
    patch_pan(&mut session, spec.pan);

    // Step 3: re-encrypt
    Ok(session.encrypt())
}

/// Splice a new track name everywhere "ProbeTrack" appears. Multiple
/// occurrences exist (0x1014 audio entry, 0x251a MIDI list entry).
fn rename_all_track_name_occurrences(session: &mut RawSession, old_name: &str, new_name: &str) {
    if old_name == new_name {
        return;
    }
    // Find all length-prefixed strings equal to old_name and splice.
    // We do this iteratively because each splice shifts all subsequent
    // offsets — re-walk the data each time.
    loop {
        let data = &session.data;
        let needle = old_name.as_bytes();
        // Build pattern: u32 LE length + bytes
        let mut pattern = (needle.len() as u32).to_le_bytes().to_vec();
        pattern.extend_from_slice(needle);

        let pos = data.windows(pattern.len()).position(|w| w == pattern);
        let Some(p) = pos else {
            break;
        };
        replace_string(session, p, new_name);
    }
}

fn patch_color(session: &mut RawSession, color: u8) {
    let color_i16 = if color == 0 { -2i16 } else { color as i16 };
    let bytes = color_i16.to_le_bytes();
    // 0x200b +106..+107
    for ct in [0x200b, 0x200a, 0x2015] {
        let offset_in_payload = match ct {
            0x200b => 106,
            0x200a => 97,
            0x2015 => 88,
            _ => continue,
        };
        for b in collect_by_raw_ct(&session.blocks, ct) {
            let p = b.start + 9 + offset_in_payload;
            if p + 2 <= session.data.len() {
                session.data[p..p + 2].copy_from_slice(&bytes);
            }
        }
    }
}

fn patch_mute(session: &mut RawSession, mute: bool) {
    let v: u8 = if mute { 1 } else { 0 };
    // 0x1029 +5
    for b in collect_by_raw_ct(&session.blocks, 0x1029) {
        let p = b.start + 9 + 5;
        if p < session.data.len() {
            session.data[p] = v;
        }
    }
    // 0x260d +14 and +447 (per-track wrapper mirrors)
    for b in collect_by_raw_ct(&session.blocks, 0x260d) {
        for off in [14usize, 447] {
            let p = b.start + 9 + off;
            if p < session.data.len() {
                session.data[p] = v;
            }
        }
    }
    // 0x261b +414 and +847
    for b in collect_by_raw_ct(&session.blocks, 0x261b) {
        for off in [414usize, 847] {
            let p = b.start + 9 + off;
            if p < session.data.len() {
                session.data[p] = v;
            }
        }
    }
    // 0x261c +423 and +856
    for b in collect_by_raw_ct(&session.blocks, 0x261c) {
        for off in [423usize, 856] {
            let p = b.start + 9 + off;
            if p < session.data.len() {
                session.data[p] = v;
            }
        }
    }
    // 0x2624 +436 and +869
    for b in collect_by_raw_ct(&session.blocks, 0x2624) {
        for off in [436usize, 869] {
            let p = b.start + 9 + off;
            if p < session.data.len() {
                session.data[p] = v;
            }
        }
    }
}

fn patch_solo(session: &mut RawSession, solo: bool) {
    let v: u8 = if solo { 1 } else { 0 };
    // 0x102d +162
    for b in collect_by_raw_ct(&session.blocks, 0x102d) {
        let p = b.start + 9 + 162;
        if p < session.data.len() {
            session.data[p] = v;
        }
    }
    // 0x261b +171, 0x261c +180, 0x2624 +193 (mirrors)
    for (ct, off) in [(0x261bu16, 171usize), (0x261c, 180), (0x2624, 193)] {
        for b in collect_by_raw_ct(&session.blocks, ct) {
            let p = b.start + 9 + off;
            if p < session.data.len() {
                session.data[p] = v;
            }
        }
    }
}

fn patch_volume(session: &mut RawSession, vol: i16) {
    let bytes = vol.to_le_bytes();
    // 0x260a[0] +26..+27 (master-send volume — only the FIRST 0x260a).
    // 0x260d +407..+408 mirror.
    // 0x261b +807..+808, 0x261c +816..+817, 0x2624 +829..+830 mirrors.
    let blocks_260a = collect_by_raw_ct(&session.blocks, 0x260a);
    if let Some(b) = blocks_260a.first() {
        let p = b.start + 9 + 26;
        if p + 2 <= session.data.len() {
            session.data[p..p + 2].copy_from_slice(&bytes);
        }
    }
    let mirror = [
        (0x260du16, 407usize),
        (0x261b, 807),
        (0x261c, 816),
        (0x2624, 829),
    ];
    for (ct, off) in mirror {
        for b in collect_by_raw_ct(&session.blocks, ct) {
            let p = b.start + 9 + off;
            if p + 2 <= session.data.len() {
                session.data[p..p + 2].copy_from_slice(&bytes);
            }
        }
    }
}

fn patch_pan(session: &mut RawSession, pan: i16) {
    let bytes = pan.to_le_bytes();
    // 0x260a[2] +26..+27 (left-channel pan — the THIRD 0x260a).
    let blocks_260a = collect_by_raw_ct(&session.blocks, 0x260a);
    if let Some(b) = blocks_260a.get(2) {
        let p = b.start + 9 + 26;
        if p + 2 <= session.data.len() {
            session.data[p..p + 2].copy_from_slice(&bytes);
        }
    }
    // 0x260c[0] +36..+37 mirror
    let blocks_260c = collect_by_raw_ct(&session.blocks, 0x260c);
    if let Some(b) = blocks_260c.first() {
        let p = b.start + 9 + 36;
        if p + 2 <= session.data.len() {
            session.data[p..p + 2].copy_from_slice(&bytes);
        }
    }
    let mirror = [
        (0x260du16, 497usize),
        (0x261b, 897),
        (0x261c, 906),
        (0x2624, 919),
    ];
    for (ct, off) in mirror {
        for b in collect_by_raw_ct(&session.blocks, ct) {
            let p = b.start + 9 + off;
            if p + 2 <= session.data.len() {
                session.data[p..p + 2].copy_from_slice(&bytes);
            }
        }
    }
}

fn collect_by_raw_ct(
    blocks: &[crate::raw_block::RawBlock],
    ct: u16,
) -> Vec<&crate::raw_block::RawBlock> {
    let mut out = Vec::new();
    fn walk<'a>(
        blocks: &'a [crate::raw_block::RawBlock],
        ct: u16,
        out: &mut Vec<&'a crate::raw_block::RawBlock>,
    ) {
        for b in blocks {
            if b.content_type_raw == ct {
                out.push(b);
            }
            walk(&b.children, ct, out);
        }
    }
    walk(blocks, ct, &mut out);
    out
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn round_trip_baseline_identity() {
        // Writing default spec should produce a file that round-trips through
        // our parser to the same baseline values.
        let spec = NativeTrackSpec::default();
        let bytes = write_single_track_ptx(&spec).unwrap();

        // Write to a temp file and re-parse via the full read_session
        // (which gives us track-level fields).
        let tmp = std::env::temp_dir().join("native_roundtrip_baseline.ptx");
        std::fs::write(&tmp, &bytes).unwrap();
        let session = crate::read_session(tmp.to_str().unwrap(), 48000).unwrap();
        let all: Vec<_> = session.all_tracks().collect();
        assert!(
            all.iter().any(|t| t.name == "ProbeTrack"),
            "track ProbeTrack should be present after round-trip"
        );
    }

    fn parse_native(spec: &NativeTrackSpec) -> crate::types::ProToolsSession {
        let bytes = write_single_track_ptx(spec).unwrap();
        let tmp = std::env::temp_dir().join(format!("native_{}.ptx", spec.name));
        std::fs::write(&tmp, &bytes).unwrap();
        crate::read_session(tmp.to_str().unwrap(), 48000).unwrap()
    }

    #[test]
    fn write_with_color() {
        let spec = NativeTrackSpec {
            color: 0x18,
            ..NativeTrackSpec::default()
        };
        let session = parse_native(&spec);
        let tracks: Vec<_> = session.all_tracks().collect();
        let t = tracks
            .iter()
            .find(|t| t.name == spec.name)
            .expect("track present");
        assert_eq!(t.color_byte, 0x18);
    }

    #[test]
    fn write_with_solo() {
        let spec = NativeTrackSpec {
            solo: true,
            name: "SoloTrack".to_string(),
            ..NativeTrackSpec::default()
        };
        let session = parse_native(&spec);
        let tracks: Vec<_> = session.all_tracks().collect();
        let t = tracks
            .iter()
            .find(|t| t.name == spec.name)
            .expect("track present");
        assert!(t.solo, "solo should round-trip");
    }

    #[test]
    fn write_with_rename() {
        let spec = NativeTrackSpec {
            name: "MyTrack".to_string(),
            ..NativeTrackSpec::default()
        };
        let session = parse_native(&spec);
        let names: Vec<&str> = session.all_tracks().map(|t| t.name.as_str()).collect();
        assert!(
            names.iter().any(|n| *n == "MyTrack"),
            "expected MyTrack in {names:?}"
        );
    }
}
