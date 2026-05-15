fn main() -> Result<(), Box<dyn std::error::Error>> {
    let path = std::env::args().nth(1).unwrap();
    let s = dawfile_protools::read_session(&path, 48000)?;
    let sr = 48000.0;
    let want: Vec<&str> = vec![
        "El Gtr 1",
        "AC GTR Strum Demo 1",
        "Intro SFX 1",
        "Intro SFX 2",
        "SYZ",
        "02 LORD OF THE FIGHT-05",
        "02 LORD OF THE FIGHT", // catch-all
    ];
    for t in &s.audio_tracks {
        if t.fades.is_empty() && !want.iter().any(|w| t.name.contains(w)) {
            continue;
        }
        if t.fades.is_empty() {
            continue;
        }
        println!(
            "track {:?} ({} items, {} fades):",
            t.name,
            t.regions.len(),
            t.fades.len()
        );
        // Items
        for (i, tr) in t.regions.iter().enumerate() {
            let r = &s.audio_regions[tr.region_index as usize];
            let start = tr.start_pos;
            let end = start + r.length;
            println!(
                "  item[{}] start={:.3}s end={:.3}s name={:?}",
                i,
                start as f64 / sr,
                end as f64 / sr,
                r.name
            );
        }
        for (i, f) in t.fades.iter().enumerate() {
            println!(
                "  fade[{}] start={:.3}s len={:.3}s curve={} reg_idx={}",
                i,
                f.start_pos as f64 / sr,
                f.length as f64 / sr,
                f.curve,
                f.region_index
            );
        }
        println!();
    }
    Ok(())
}
