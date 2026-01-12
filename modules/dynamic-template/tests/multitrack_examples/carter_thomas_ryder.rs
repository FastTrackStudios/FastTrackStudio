use daw::tracks::{TrackStructureBuilder, assert_tracks_equal};
use dynamic_template::*;

type Result<T> = core::result::Result<T, Box<dyn std::error::Error>>;

#[test]
fn carter_thomas_ryder() -> Result<()> {
    // -- Setup & Fixtures
    // Carter Thomas - Ryder: Electronic production with drum loops, arps, and extensive BGV layers
    let items = vec![
        "12 THROW.wav",
        "14TH THROW.wav",
        "AD LIB 1.wav",
        "ARP 1.wav",
        "ARP TRANSITION.wav",
        "BASS.wav",
        "D-50.wav",
        "DBL 1.wav",
        "DBL 2.wav",
        "DBL 3.wav",
        "DBL 4.wav",
        "DBL 5.wav",
        "DBL 6.wav",
        "DBL 7.wav",
        "DBL 8.wav",
        "DRUM LOOP 2.wav",
        "DRUM LOOP.wav",
        "GTR STAB L.wav",
        "GTR STAB R.wav",
        "HK BGV L.wav",
        "HK BGV L2.wav",
        "HK BGV R.wav",
        "HK BGV R2.wav",
        "HK BGV2 L1.wav",
        "HK BGV2 L2.wav",
        "HK BGV2 L3.wav",
        "HK BGV2 L4.wav",
        "HK BGV2 L5.wav",
        "HK BGV2 L6.wav",
        "HK BGV2 R1.wav",
        "HK BGV2 R2.wav",
        "HK BGV2 R3.wav",
        "HK BGV2 R4.wav",
        "HK BGV2 R5.wav",
        "HK BGV2 R6.wav",
        "HK BGV3 L1.wav",
        "HK BGV3 L2.wav",
        "HK BGV3 L3.wav",
        "HK BGV3 L4.wav",
        "HK BGV3 L5.wav",
        "HK BGV3 L6.wav",
        "HK BGV3 R1.wav",
        "HK BGV3 R2.wav",
        "HK BGV3 R3.wav",
        "HK BGV3 R4.wav",
        "HK BGV3 R5.wav",
        "HK BGV3 R6.wav",
        "HK BGV4 L1.wav",
        "HK BGV4 R1.wav",
        "HK BGV5 L1.wav",
        "HK BGV5 R1.wav",
        "HK VLD 1.wav",
        "HK VLD 2.wav",
        "PADS.wav",
        "V1 BGV1 L1.wav",
        "V1 BGV1 L2.wav",
        "V1 BGV1 L3.wav",
        "V1 BGV1 L4.wav",
        "V1 BGV1 R1.wav",
        "V1 BGV1 R2.wav",
        "V1 BGV1 R3.wav",
        "V1 BGV1 R4.wav",
        "V1 BGV2 L1.wav",
        "V1 BGV2 L2.wav",
        "V1 BGV2 L3.wav",
        "V1 BGV2 L4.wav",
        "V1 BGV2 L5.wav",
        "V1 BGV2 L6.wav",
        "V1 BGV2 L7.wav",
        "V1 BGV2 R1.wav",
        "V1 BGV2 R2.wav",
        "V1 BGV2 R3.wav",
        "V1 BGV2 R4.wav",
        "V1 BGV2 R5.wav",
        "V1 BGV2 R6.wav",
        "V1 BGV2 R7.wav",
        "V2 BGV1 L1.wav",
        "V2 BGV1 R2.wav",
        "V2 BGV2 L1.wav",
        "V2 BGV2 L2.wav",
        "V2 BGV2 L3.wav",
        "V2 BGV2 R1.wav",
        "V2 BGV2 R2.wav",
        "V2 BGV2 R3.wav",
        "V2 BGV3 L1.wav",
        "V2 BGV3 L2.wav",
        "V2 BGV3 L3.wav",
        "V2 BGV3 R1.wav",
        "V2 BGV3 R2.wav",
        "V2 BGV3 R3.wav",
        "V2 BGV4 L1.wav",
        "V2 BGV4 L2.wav",
        "V2 BGV4 L3.wav",
        "V2 BGV4 L4.wav",
        "V2 BGV4 R1.wav",
        "V2 BGV4 R2.wav",
        "V2 BGV4 R3.wav",
        "V2 BGV4 R4.wav",
        "V2 BGV5 L1.wav",
        "V2 BGV5 L2.wav",
        "V2 BGV5 L3.wav",
        "V2 BGV5 R1.wav",
        "V2 BGV5 R2.wav",
        "V2 BGV5 R3.wav",
        "VERB THROW.wav",
        "Vocal EFX _12 DLY.wav",
        "Vocal EFX _14TH DLY.wav",
        "Vocal EFX _480L.wav",
        "Vocal EFX _DIM D.wav",
        "Vocal EFX _DOUBLER.wav",
        "Vocal EFX _EMT 140.wav",
        "Vocal EFX _MONO DLY.wav",
        "Vocal EFX _SLAP DLY.wav",
        "VRS LD 1.wav",
        "VRS LD 2.wav",
        "VRS LD 3.wav",
        "WAVES SFX.wav",
    ];
    let config = default_config();

    // -- Exec
    let tracks = items.organize_into_tracks(&config, None)?;

    // -- Check
    println!("\nTrack list:");
    daw::tracks::display_tracklist(&tracks);

    let expected = TrackStructureBuilder::new()
        .folder("Drums")
            .track("Drum Kit 1", "DRUM LOOP 2.wav")
            .track("Drum Kit 2", "DRUM LOOP.wav")
        .end()
        .track("Bass", "BASS.wav")
        .folder("Synths")
            .folder("Arp")
                .track("Arp", "ARP 1.wav")
                .track("Transition", "ARP TRANSITION.wav")
            .end()
            .folder("Chord")
                .track("Chord 1", "GTR STAB L.wav")
                .track("Chord 2", "GTR STAB R.wav")
            .end()
        .end()
        .folder("Vocals")
            .folder("Lead")
                .folder("Lead")
                    .track("Dly 1", "Vocal EFX _12 DLY.wav")
                    .track("Dly 2", "Vocal EFX _14TH DLY.wav")
                    .track("Lead 1", "Vocal EFX _480L.wav")
                    .track("Lead 2", "Vocal EFX _DIM D.wav")
                    .track("Lead 3", "Vocal EFX _DOUBLER.wav")
                    .track("Lead 4", "Vocal EFX _EMT 140.wav")
                    .track("Dly Slap", "Vocal EFX _SLAP DLY.wav")
                .end()
                .track("Mono", "Vocal EFX _MONO DLY.wav")
            .end()
            .folder("BGVs")
                .folder("V1")
                    .folder("L")
                        .track("BGVs 1", "V1 BGV1 L1.wav")
                        .track("BGVs 2", "V1 BGV1 L2.wav")
                        .track("BGVs 3", "V1 BGV1 L3.wav")
                        .track("BGVs 4", "V1 BGV1 L4.wav")
                        .track("BGVs 5", "V1 BGV2 L1.wav")
                        .track("BGVs 6", "V1 BGV2 L2.wav")
                        .track("BGVs 7", "V1 BGV2 L3.wav")
                        .track("BGVs 8", "V1 BGV2 L4.wav")
                        .track("BGVs 9", "V1 BGV2 L5.wav")
                        .track("BGVs 10", "V1 BGV2 L6.wav")
                        .track("BGVs 11", "V1 BGV2 L7.wav")
                    .end()
                    .folder("R")
                        .track("BGVs 1", "V1 BGV1 R1.wav")
                        .track("BGVs 2", "V1 BGV1 R2.wav")
                        .track("BGVs 3", "V1 BGV1 R3.wav")
                        .track("BGVs 4", "V1 BGV1 R4.wav")
                        .track("BGVs 5", "V1 BGV2 R1.wav")
                        .track("BGVs 6", "V1 BGV2 R2.wav")
                        .track("BGVs 7", "V1 BGV2 R3.wav")
                        .track("BGVs 8", "V1 BGV2 R4.wav")
                        .track("BGVs 9", "V1 BGV2 R5.wav")
                        .track("BGVs 10", "V1 BGV2 R6.wav")
                        .track("BGVs 11", "V1 BGV2 R7.wav")
                    .end()
                .end()
                .folder("V2")
                    .folder("L")
                        .track("BGVs 1", "V2 BGV1 L1.wav")
                        .track("BGVs 2", "V2 BGV2 L1.wav")
                        .track("BGVs 3", "V2 BGV2 L2.wav")
                        .track("BGVs 4", "V2 BGV2 L3.wav")
                        .track("BGVs 5", "V2 BGV3 L1.wav")
                        .track("BGVs 6", "V2 BGV3 L2.wav")
                        .track("BGVs 7", "V2 BGV3 L3.wav")
                        .track("BGVs 8", "V2 BGV4 L1.wav")
                        .track("BGVs 9", "V2 BGV4 L2.wav")
                        .track("BGVs 10", "V2 BGV4 L3.wav")
                        .track("BGVs 11", "V2 BGV4 L4.wav")
                        .track("BGVs 12", "V2 BGV5 L1.wav")
                        .track("BGVs 13", "V2 BGV5 L2.wav")
                        .track("BGVs 14", "V2 BGV5 L3.wav")
                    .end()
                    .folder("R")
                        .track("BGVs 1", "V2 BGV1 R2.wav")
                        .track("BGVs 2", "V2 BGV2 R1.wav")
                        .track("BGVs 3", "V2 BGV2 R2.wav")
                        .track("BGVs 4", "V2 BGV2 R3.wav")
                        .track("BGVs 5", "V2 BGV3 R1.wav")
                        .track("BGVs 6", "V2 BGV3 R2.wav")
                        .track("BGVs 7", "V2 BGV3 R3.wav")
                        .track("BGVs 8", "V2 BGV4 R1.wav")
                        .track("BGVs 9", "V2 BGV4 R2.wav")
                        .track("BGVs 10", "V2 BGV4 R3.wav")
                        .track("BGVs 11", "V2 BGV4 R4.wav")
                        .track("BGVs 12", "V2 BGV5 R1.wav")
                        .track("BGVs 13", "V2 BGV5 R2.wav")
                        .track("BGVs 14", "V2 BGV5 R3.wav")
                    .end()
                .end()
                .folder("BGVs")
                    .folder("L")
                        .track("L 1", "HK BGV L.wav")
                        .track("L 2", "HK BGV L2.wav")
                        .track("L 3", "HK BGV2 L1.wav")
                        .track("L 4", "HK BGV2 L2.wav")
                        .track("L 5", "HK BGV2 L3.wav")
                        .track("L 6", "HK BGV2 L4.wav")
                        .track("L 7", "HK BGV2 L5.wav")
                        .track("L 8", "HK BGV2 L6.wav")
                        .track("L 9", "HK BGV3 L1.wav")
                        .track("L 10", "HK BGV3 L2.wav")
                        .track("L 11", "HK BGV3 L3.wav")
                        .track("L 12", "HK BGV3 L4.wav")
                        .track("L 13", "HK BGV3 L5.wav")
                        .track("L 14", "HK BGV3 L6.wav")
                        .track("L 15", "HK BGV4 L1.wav")
                        .track("L 16", "HK BGV5 L1.wav")
                    .end()
                    .folder("R")
                        .track("R 1", "HK BGV R.wav")
                        .track("R 2", "HK BGV R2.wav")
                        .track("R 3", "HK BGV2 R1.wav")
                        .track("R 4", "HK BGV2 R2.wav")
                        .track("R 5", "HK BGV2 R3.wav")
                        .track("R 6", "HK BGV2 R4.wav")
                        .track("R 7", "HK BGV2 R5.wav")
                        .track("R 8", "HK BGV2 R6.wav")
                        .track("R 9", "HK BGV3 R1.wav")
                        .track("R 10", "HK BGV3 R2.wav")
                        .track("R 11", "HK BGV3 R3.wav")
                        .track("R 12", "HK BGV3 R4.wav")
                        .track("R 13", "HK BGV3 R5.wav")
                        .track("R 14", "HK BGV3 R6.wav")
                        .track("R 15", "HK BGV4 R1.wav")
                        .track("R 16", "HK BGV5 R1.wav")
                    .end()
                .end()
            .end()
        .end()
        .track("SFX", "WAVES SFX.wav")
        .folder("Unsorted")
            .track("THROW", "12 THROW.wav")
            .track("14TH THROW", "14TH THROW.wav")
            .track("AD LIB 1", "AD LIB 1.wav")
            .track("D-50", "D-50.wav")
            .track("DBL 1", "DBL 1.wav")
            .track("DBL 2", "DBL 2.wav")
            .track("DBL 3", "DBL 3.wav")
            .track("DBL 4", "DBL 4.wav")
            .track("DBL 5", "DBL 5.wav")
            .track("DBL 6", "DBL 6.wav")
            .track("DBL 7", "DBL 7.wav")
            .track("DBL 8", "DBL 8.wav")
            .track("HK VLD 1", "HK VLD 1.wav")
            .track("HK VLD 2", "HK VLD 2.wav")
            .track("PADS", "PADS.wav")
            .track("VERB THROW", "VERB THROW.wav")
            .track("VRS LD 1", "VRS LD 1.wav")
            .track("VRS LD 2", "VRS LD 2.wav")
            .track("VRS LD 3", "VRS LD 3.wav")
        .end()
        .build();

    assert_tracks_equal(&tracks, &expected)?;

    Ok(())
}
