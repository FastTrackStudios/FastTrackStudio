fn main() {
    let path = std::env::args().nth(1).unwrap();
    let s = dawfile_protools::read_session(&path, 0).unwrap();
    println!("bpm={} tempo_events={}", s.bpm, s.tempo_events.len());
    for (i, t) in s.tempo_events.iter().take(3).enumerate() {
        println!("  [{i}] bpm={} tpb={}", t.bpm, t.ticks_per_beat);
    }
    for m in s.meter_events.iter().take(3) {
        println!("  meter {}/{}", m.numerator, m.denominator);
    }
}
