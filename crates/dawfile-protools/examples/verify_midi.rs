fn main() -> Result<(), Box<dyn std::error::Error>> {
    let path = std::env::args().nth(1).unwrap();
    let s = dawfile_protools::read_session(&path, 48000)?;
    println!("audio_tracks ({}):", s.audio_tracks.len());
    for t in &s.audio_tracks {
        println!("  {:?}", t.name);
    }
    println!();
    println!("midi_tracks ({}):", s.midi_tracks.len());
    for t in &s.midi_tracks {
        println!("  {:?}", t.name);
    }
    Ok(())
}
