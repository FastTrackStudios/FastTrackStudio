// Compare per-track MIDI note counts: our PT parse vs reference SMF export.
use std::fs;
// ── Minimal Standard MIDI File reader ───────────────────────────────────────

struct Smf {
    division: u16,
    tracks: Vec<Vec<SmfEvent>>,
}

#[derive(Debug)]
enum SmfEvent {
    Tempo { tick: u64, us_per_qn: u32 },
    TimeSig { tick: u64, num: u8, den: u8 },
    Marker { tick: u64, text: String },
    TrackName { text: String },
    NoteOn { tick: u64, note: u8, vel: u8 },
}

struct Reader<'a> {
    d: &'a [u8],
    p: usize,
}
impl<'a> Reader<'a> {
    fn u8(&mut self) -> u8 {
        let v = self.d[self.p];
        self.p += 1;
        v
    }
    fn u16(&mut self) -> u16 {
        let v = u16::from_be_bytes([self.d[self.p], self.d[self.p + 1]]);
        self.p += 2;
        v
    }
    fn u32(&mut self) -> u32 {
        let v = u32::from_be_bytes(self.d[self.p..self.p + 4].try_into().unwrap());
        self.p += 4;
        v
    }
    fn varlen(&mut self) -> u64 {
        let mut val = 0u64;
        loop {
            let b = self.u8();
            val = (val << 7) | (b & 0x7f) as u64;
            if b & 0x80 == 0 {
                break;
            }
        }
        val
    }
}

fn parse_smf(data: &[u8]) -> Smf {
    let mut r = Reader { d: data, p: 0 };
    assert_eq!(&data[0..4], b"MThd");
    r.p = 4;
    let _len = r.u32();
    let _format = r.u16();
    let ntrks = r.u16();
    let division = r.u16();
    r.p = 8 + 6;

    let mut tracks = Vec::new();
    for _ in 0..ntrks {
        assert_eq!(&data[r.p..r.p + 4], b"MTrk");
        r.p += 4;
        let len = r.u32() as usize;
        let end = r.p + len;
        let mut tick = 0u64;
        let mut running_status = 0u8;
        let mut events = Vec::new();
        while r.p < end {
            tick += r.varlen();
            let mut status = r.d[r.p];
            if status & 0x80 != 0 {
                r.p += 1;
                running_status = status;
            } else {
                status = running_status; // running status: reuse last
            }
            match status {
                0xff => {
                    let meta = r.u8();
                    let mlen = r.varlen() as usize;
                    let bytes = &r.d[r.p..r.p + mlen];
                    match meta {
                        0x51 => {
                            let us = ((bytes[0] as u32) << 16)
                                | ((bytes[1] as u32) << 8)
                                | bytes[2] as u32;
                            events.push(SmfEvent::Tempo {
                                tick,
                                us_per_qn: us,
                            });
                        }
                        0x58 => events.push(SmfEvent::TimeSig {
                            tick,
                            num: bytes[0],
                            den: 1 << bytes[1],
                        }),
                        0x06 => events.push(SmfEvent::Marker {
                            tick,
                            text: String::from_utf8_lossy(bytes).to_string(),
                        }),
                        0x03 => events.push(SmfEvent::TrackName {
                            text: String::from_utf8_lossy(bytes).to_string(),
                        }),
                        _ => {}
                    }
                    r.p += mlen;
                }
                0xf0 | 0xf7 => {
                    let slen = r.varlen() as usize;
                    r.p += slen;
                }
                _ => {
                    let hi = status & 0xf0;
                    let data_bytes = if matches!(hi, 0xc0 | 0xd0) { 1 } else { 2 };
                    let d0 = r.u8();
                    let d1 = if data_bytes == 2 { r.u8() } else { 0 };
                    if hi == 0x90 && d1 > 0 {
                        events.push(SmfEvent::NoteOn {
                            tick,
                            note: d0,
                            vel: d1,
                        });
                    }
                }
            }
        }
        r.p = end;
        tracks.push(events);
    }
    Smf { division, tracks }
}

fn main() {
    let ptx = std::env::args().nth(1).unwrap();
    let mid = std::env::args().nth(2).unwrap();
    let smf = parse_smf(&fs::read(&mid).unwrap());
    let mut refs: Vec<(String, usize)> = Vec::new();
    for tr in &smf.tracks {
        let name = tr.iter().find_map(|e| {
            if let SmfEvent::TrackName { text } = e {
                Some(text.clone())
            } else {
                None
            }
        });
        let n = tr
            .iter()
            .filter(|e| matches!(e, SmfEvent::NoteOn { .. }))
            .count();
        if n > 0 {
            refs.push((name.unwrap_or_default(), n));
        }
    }
    let s = dawfile_protools::read_session(&ptx, 0).unwrap();
    let mut ours: Vec<(String, usize)> = Vec::new();
    for t in &s.midi_tracks {
        let n: usize = t
            .regions
            .iter()
            .map(|r| {
                let (lo, hi) = (r.clip_lo_ticks, r.note_trim_ticks);
                s.midi_regions
                    .get(r.region_index as usize)
                    .map(|reg| {
                        reg.events
                            .iter()
                            .filter(|e| e.velocity > 0 && e.position >= lo && e.position < hi)
                            .count()
                    })
                    .unwrap_or(0)
            })
            .sum();
        ours.push((t.name.clone(), n));
    }
    let mut ok = 0;
    let mut tot = 0;
    for (rn, rc) in &refs {
        tot += 1;
        let oc = ours
            .iter()
            .find(|(on, _)| on == rn)
            .map(|(_, c)| *c)
            .unwrap_or(0);
        let mark = if oc == *rc { "OK  " } else { "DIFF" };
        if oc == *rc {
            ok += 1;
        }
        println!("  [{mark}] {:<26} ref={:<5} ours={}", rn, rc, oc);
    }
    println!("=> {ok}/{tot} tracks exact");
}
