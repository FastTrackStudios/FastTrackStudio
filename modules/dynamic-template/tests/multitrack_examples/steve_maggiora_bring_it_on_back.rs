use daw::tracks::{TrackStructureBuilder, assert_tracks_equal};
use dynamic_template::*;

type Result<T> = core::result::Result<T, Box<dyn std::error::Error>>;

#[test]
fn steve_maggiora_bring_it_on_back() -> Result<()> {
    // -- Setup & Fixtures
    // Steve Maggiora - Bring It On Back: 71-track soul/R&B session with horn section and layered BGVs
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
    let config = default_config();

    // -- Exec
    let tracks = items.organize_into_tracks(&config, None)?;

    // -- Check
    println!("\nTrack list:");
    daw::tracks::display_tracklist(&tracks);

    // Expected structure:
    // Drums/
    //   ├─ Kick/                    ← SUM (In/Out) and Kick
    //   ├─ Snare/                   ← Top, Bottom, Snare
    //   ├─ Toms/                    ← Floor, Bottom, Top, Tom
    //   ├─ Cymbals/
    //   │   ├─ Hi Hat               ← HH.wav
    //   │   ├─ OH/                  ← OH_SUM, OHL, OHR
    //   │   └─ Cymbals              ← Cymbal Swell
    //   └─ Rooms/                   ← L, R, Mono, C, Rooms_SUM
    // Bass/                         ← Amp, Bass 1 (DI), Bass 2 (SUM)
    // Guitars/
    //   ├─ Lead/                    ← Lead Guitar Amp/DI/SUM
    //   ├─ Guitars/                 ← Guitar DI/SUM
    //   └─ Guitars 1/               ← Guitar Amp 1A/1B
    // Keys/
    //   ├─ Piano/                   ← Piano L/R/SUM
    //   └─ Wurlitzer/               ← Wurlitzer L/R/SUM
    // Synths/                       ← Boho Synth Pluck, Synth Rise Run 1/2
    // Vocals/
    //   ├─ Lead/                    ← Kim VOX, Lead VOX, Xavier VOX, Doubles
    //   └─ BGVs/
    //       ├─ Bridge/              ← MG's Bridge BG's (now recognized as BG)
    //       └─ BGVs/                ← BGV1_SUM, BGV1A-D
    // Orchestra/
    //   ├─ Trumpets                 ← 01 Trumpet
    //   └─ Trombone                 ← 02 Trombone
    // SFX                           ← 06 Sax FX only
    // Unsorted/                     ← Tenor/Bari Sax, Sax fills (Tenor sax no longer matches Choir)
    let expected = TrackStructureBuilder::new()
        .folder("Drums")
            .folder("Kick")
                .folder("SUM")
                    .track("In", "Kick In.wav")
                    .track("Out", "Kick Out.wav")
                .end()
                .track("Kick", "Kick_SUM.wav")
            .end()
            .folder("Snare")
                .track("Top", "Snare Top.wav")
                .track("Bottom", "Snare Bottom.wav")
                .track("Snare", "Snare_SUM.wav")
            .end()
            .folder("Toms")
                .track("Floor", "Floor Tom_SUM.wav")
                .folder("Bottom")
                    .track("Tom Bottom 1", "Floor Tom Bottom.wav")
                    .track("Tom Bottom 2", "Rack Tom Bottom.wav")
                .end()
                .folder("Top")
                    .track("Tom Top 1", "Floor Tom Top.wav")
                    .track("Tom Top 2", "Rack Tom Top.wav")
                .end()
                .track("Tom", "Rack Tom_SUM.wav")
            .end()
            .folder("Cymbals")
                .track("Hi Hat", "HH.wav")
                .folder("OH")
                    .track("OH 1", "OH_SUM.wav")
                    .track("OH 2", "OHL.wav")
                    .track("OH 3", "OHR.wav")
                .end()
                .track("Cymbals", "Cymbal Swell.wav")
            .end()
            .folder("Rooms")
                .track("L", "Room L.wav")
                .track("R", "Room R.wav")
                .track("Mono", "Room Mono.wav")
                .track("Rooms 1", "Room C.wav")
                .track("Rooms 2", "Rooms_SUM.wav")
            .end()
        .end()
        .folder("Bass")
            .track("Amp", "Bass Amp.wav")
            .track("Bass 1", "Bass DI.wav")
            .track("Bass 2", "Bass_SUM.wav")
        .end()
        .folder("Guitars")
            .folder("Lead")
                .track("Amp", "Lead Guitar Amp.wav")
                .track("DI", "Lead Guitar DI.wav")
                .track("Electric", "Lead Guitar_SUM.wav")
            .end()
            .folder("Guitars")
                .track("DI", "Guitar DI.wav")
                .track("Electric", "Guitar_SUM.wav")
            .end()
            .folder("Guitars 1")
                .track("Electric Amp 1 1", "Guitar Amp 1A.wav")
                .track("Electric Amp 1 2", "Guitar Amp 1B.wav")
            .end()
        .end()
        .folder("Keys")
            .folder("Piano")
                .track("Piano 1", "Piano L.wav")
                .track("Piano 2", "Piano R.wav")
                .track("Piano 3", "Piano_SUM.wav")
            .end()
            .folder("Wurlitzer")
                .track("Wurlitzer 1", "Wurlitzer L.wav")
                .track("Wurlitzer 2", "Wurlitzer R.wav")
                .track("Wurlitzer 3", "Wurlitzer_SUM.wav")
            .end()
        .end()
        .folder("Synths")
            .track("Boho Synth Pluck", "Boho Synth Pluck.wav")
            .track("Synth Rise Run 1", "Synth Rise Run 1.wav")
            .track("Synth Rise Run 2", "Synth Rise Run 2.wav")
        .end()
        .folder("Vocals")
            .folder("Lead")
                .folder("Kim")
                    .track("Kim", "Kim VOX_SUM.wav")
                    .folder("Kim 1")
                        .track("Kim 1", "Kim VOX 1A.wav")
                        .track("Kim 2", "Kim VOX 1B.wav")
                    .end()
                .end()
                .folder("Main")
                    .track("Lead 1", "Lead VOX.wav")
                    .track("Lead 2", "Xavier VOX.wav")
                .end()
                .folder("Double")
                    .track("Double 1", "Lead VOX Double 1.wav")
                    .track("Double 2", "Lead VOX Double 2.wav")
                    .track("Double 3", "Lead VOX Double_SUM.wav")
                .end()
            .end()
            .folder("BGVs")
                .folder("Bridge")
                    .track("Bridge 1", "07 MG_s Bridge BG_s - Bring It On  Back Multis.wav")
                    .track("Bridge 2", "08 MG_s Bridge BG_s FX only - Bring It On  Back Multis.wav")
                .end()
                .folder("BGVs")
                    .track("BGVs 1", "BGV1_SUM.wav")
                    .track("BGVs 2", "BGV1A.wav")
                    .track("BGVs 3", "BGV1B.wav")
                    .track("BGVs 4", "BGV1C.wav")
                    .track("BGVs 5", "BGV1D.wav")
                .end()
            .end()
        .end()
        .folder("Orchestra")
            .track("Trumpets", "01 Trumpet - Bring It On Back.wav")
            .track("Trombone", "02 Trombone - Bring It On Back.wav")
        .end()
        .track("SFX", "06 Sax FX only - Bring It On  Back Multis.wav")
        .folder("Unsorted")
            .track("Tenor Sax - Bring It On Back", "03 Tenor sax - Bring It On Back.wav")
            .track("Bari Sax - Bring It On Back", "04 Bari sax - Bring It On Back.wav")
            .track("05Sax Fills - Bring It On Back Multis", "05Sax fills - Bring It On  Back Multis.wav")
        .end()
        .build();

    assert_tracks_equal(&tracks, &expected)?;

    Ok(())
}
