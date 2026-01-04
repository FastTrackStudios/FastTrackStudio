use dynamic_template::*;
use monarchy::monarchy_sort;

#[test]
fn bon_jovi_you_give_love_a_bad_name() {
    // Track list from "Bon Jovi - You Give Love a Bad Name"
    let items = vec![
        "095 Pop Tamb.L.wav",
        "095 Pop Tamb.R.wav",
        "Acoustic.Right.wav",
        "Acoustic.wav",
        "banjo.One.wav",
        "banjo.Solo.wav",
        "banjo.Two.wav",
        "Bass.wav",
        "Drums.PNT.L.wav",
        "Drums.PNT.R.wav",
        "Fiddle PNT.L.wav",
        "Fiddle PNT.R.wav",
        "Guitar Slide.wav",
        "Guitar Solo.wav",
        "Mando.wav",
        "Vocal.Harmony.One.wav",
        "Vocal.Harmony.Two.wav",
        "Vocal.Tune.Lead.wav",
        "You Give Love A Bad Name.PRINT.L.wav",
        "You Give Love A Bad Name.PRINT.R.wav",
    ];

    // Organize into tracks using monarchy sort
    let config = default_config();
    let tracks = items.clone().organize_into_tracks(&config, None).unwrap();

    // Display the track list
    println!("\nTrack list:");
    daw::tracks::display_tracklist(&tracks);

    // Snapshot test: capture the hierarchical structure
    let structure = monarchy_sort(items, config).unwrap();
    insta::assert_snapshot!(structure.to_tree_string());
}
