//! Print per-track mute / volume / pan from the parsed session.

fn main() {
    let path = std::env::args().nth(1).expect("path");
    let session = dawfile_protools::read_session(&path, 0).unwrap();
    println!("audio tracks:");
    for (i, t) in session.audio_tracks.iter().enumerate() {
        println!(
            "  [{i:02}] mute={} vol={:>5} pan={:>4} out={:<14}  {}",
            t.mute as u8, t.volume_centibel, t.pan, t.output, t.name
        );
    }
    println!("midi tracks:");
    for (i, t) in session.midi_tracks.iter().enumerate() {
        println!(
            "  [{i:02}] mute={} vol={:>5} pan={:>4} out={:<14}  {}",
            t.mute as u8, t.volume_centibel, t.pan, t.output, t.name
        );
    }
}
