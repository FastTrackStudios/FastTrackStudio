//! Test for Steve Maggiora - Hey Lady Multitracks
//!
//! TODO: Add expected track structure and matching verification

use naming_convention::{SimpleParser, create_default_groups, format_track_name_default};

#[test]
fn test_steve_maggiora_hey_lady() {
    let track_names = vec![
        "AD.Big Kit Gretch.83.5BPM.L.wav",
        "AD.Big Kit Gretch.83.5BPM.R.wav",
        "AD.Big Kit Gretch.Grace.83.5BPM.L.wav",
        "AD.Big Kit Gretch.Grace.83.5BPM.R.wav",
        "AD.Bootstraps.Kick.83.5BPM.L.wav",
        "AD.Bootstraps.Kick.83.5BPM.R.wav",
        "AD.Evil Metal.Tama.83.5BPM.L.wav",
        "AD.Evil Metal.Tama.83.5BPM.R.wav",
        "AD.Great.Rock.Sanre.Pearl.83.5BPM.L.wav",
        "AD.Great.Rock.Sanre.Pearl.83.5BPM.R.wav",
        "AD.Great.Rock.Sanre.Pearl.Grace.83.5BPM.L.wav",
        "AD.Great.Rock.Sanre.Pearl.Grace.83.5BPM.R.wav",
        "AD.Hard.Rock.Tama.83.5BPM.L.wav",
        "AD.Hard.Rock.Tama.83.5BPM.R.wav",
        "AD.Hard.Rock.Tama.Grace.83.5BPM.L.wav",
        "AD.Hard.Rock.Tama.Grace.83.5BPM.R.wav",
        "AD.Metal.Kick.Print.83.5BPM.L.wav",
        "AD.Metal.Kick.Print.83.5BPM.R.wav",
        "B3.83.5BPM.C.wav",
        "B3.83.5BPM.L.wav",
        "B3.83.5BPM.R.wav",
        "Bass.Di.83.5BPM.wav",
        "Bass.Mic.83.5BPM.wav",
        "Bass.Mic.dup1.83.5BPM.wav",
        "Echo.Plex.Move.83.5BPM.wav",
        "Flr.27.Trick.83.5BPM.wav",
        "Gtr_57.83.5BPM.wav",
        "Gtr_57.dup1.83.5BPM.wav",
        "Gtr_Di.83.5BPM.wav",
        "H3000.One.New.83.5BPM.L.wav",
        "H3000.One.New.83.5BPM.R.wav",
        "H3000.Three.New.83.5BPM.L.wav",
        "H3000.Three.New.83.5BPM.R.wav",
        "H3000.Two.New.83.5BPM.L.wav",
        "H3000.Two.New.83.5BPM.R.wav",
        "Hammond Hi.dup1.83.5BPM.L.wav",
        "Hammond Hi.dup1.83.5BPM.R.wav",
        "Hey.Lady.83.5.BPM.10.08.15.Mix.Recall.L.wav",
        "Hey.Lady.83.5.BPM.10.08.15.Mix.Recall.R.wav",
        "Hi Hat.83.5BPM.wav",
        "Kick.83.5BPM.wav",
        "OH.83.5BPM.L.wav",
        "OH.83.5BPM.R.wav",
        "Rack.27.Trick.83.5BPM.wav",
        "SN Bot.83.5BPM.wav",
        "SN Top.83.5BPM.wav",
        "Snare D 28.83.5BPM.wav",
        "Snare D 28_Oct.Pitch.83.5BPM.wav",
        "Upright.Forty.Seven.83.5BPM.wav",
        "Upright.Ribbon.83.5BPM.wav",
        "Vocal.Comp.83.5BPM.wav",
        "Vocal.Dub.83.5BPM.wav",
        "Vocal.Eko.Plate.83.5BPM.L.wav",
        "Vocal.Eko.Plate.83.5BPM.R.wav",
        "Vocal.Magic.83.5BPM.L.wav",
        "Vocal.Magic.83.5BPM.R.wav",
        "sax verb.83.5BPM.L.wav",
        "sax verb.83.5BPM.R.wav",
        "sax verb2.83.5BPM.L.wav",
        "sax verb2.83.5BPM.R.wav",
        "sax verb3.83.5BPM.L.wav",
        "sax verb3.83.5BPM.R.wav",
        "sax1.83.5BPM.wav",
        "sax2.83.5BPM.wav",
        "sax3.83.5BPM.wav",
    ];

    let parser = SimpleParser::new(create_default_groups());
    
    println!("\n=== Parsing Steve Maggiora - Hey Lady ===\n");
    
    for name in &track_names {
        let parsed = parser.parse(name);
        let formatted = format_track_name_default(&parsed);
        println!("{} -> {}", name, formatted);
    }
    
    // TODO: Build expected track structure
    // TODO: Create matcher and verify matches
    assert!(!track_names.is_empty(), "Should have track names to test");
}

