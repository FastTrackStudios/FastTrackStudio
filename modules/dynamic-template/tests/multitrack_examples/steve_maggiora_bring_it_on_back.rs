use dynamic_template::*;

#[test]
fn steve_maggiora_bring_it_on_back() {
    // Track list from "Steve Maggiora - Bring It On Back"
    let items = vec![
        "01 Trumpet - Bring It On Back.wav",
        "02 Trombone - Bring It On Back.wav",
        "03 Tenor sax - Bring It On Back.wav",
        "04 Bari sax - Bring It On Back.wav",
        "05Sax fills - Bring It On  Back Multis.wav",
        "06 Sax FX only - Bring It On  Back Multis.wav",
        "07 MG_s Bridge BG_s - Bring It On  Back Multis.wav",
        "08 MG_s Bridge BG_s FX only - Bring It On  Back Multis.wav",
        "Bass Amp.wav",
        "Bass DI.wav",
        "Bass_SUM.wav",
        "BGV1_SUM.wav",
        "BGV1A.wav",
        "BGV1B.wav",
        "BGV1C.wav",
        "BGV1D.wav",
        "Boho Synth Pluck.wav",
        "Cymbal Swell.wav",
        "Floor Tom Bottom.wav",
        "Floor Tom Top.wav",
        "Floor Tom_SUM.wav",
        "Guitar Amp 1A.wav",
        "Guitar Amp 1B.wav",
        "Guitar DI.wav",
        "Guitar_SUM.wav",
        "HH.wav",
        "Kick In.wav",
        "Kick Out.wav",
        "Kick_SUM.wav",
        "Kim VOX 1A.wav",
        "Kim VOX 1B.wav",
        "Kim VOX_SUM.wav",
        "Lead Guitar Amp.wav",
        "Lead Guitar DI.wav",
        "Lead Guitar_SUM.wav",
        "Lead VOX Double 1.wav",
        "Lead VOX Double 2.wav",
        "Lead VOX Double_SUM.wav",
        "Lead VOX.wav",
        "OH_SUM.wav",
        "OHL.wav",
        "OHR.wav",
        "Piano L.wav",
        "Piano R.wav",
        "Piano_SUM.wav",
        "Rack Tom Bottom.wav",
        "Rack Tom Top.wav",
        "Rack Tom_SUM.wav",
        "Room C.wav",
        "Room L.wav",
        "Room Mono.wav",
        "Room R.wav",
        "Rooms_SUM.wav",
        "Snare Bottom.wav",
        "Snare Top.wav",
        "Snare_SUM.wav",
        "Synth Rise Run 1.wav",
        "Synth Rise Run 2.wav",
        "Wurlitzer L.wav",
        "Wurlitzer R.wav",
        "Wurlitzer_SUM.wav",
        "Xavier VOX.wav",
    ];

    // Organize into tracks using monarchy sort
    let config = default_config();
    let tracks = items.organize_into_tracks(&config, None).unwrap();

    // Display the track list
    println!("\nTrack list:");
    daw::tracks::display_tracklist(&tracks);

    // TODO: Add expected structure once provided
}
