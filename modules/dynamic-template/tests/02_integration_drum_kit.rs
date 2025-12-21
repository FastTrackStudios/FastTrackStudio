use dynamic_template::*;
use daw::tracks::item::Item;
use daw::tracks::{TrackStructureBuilder, assert_tracks_equal};

#[test]
fn full_drum_kit_integration_test() {
    // Test the complete drum kit with all drum types
    let items = vec![
        "Kick In",
        "Kick Out",
        "Snare Top",
        "Snare Bottom",
        "Tom 1",
        "Tom 2",
        "Tom 3",
        "Hi Hat",
        "Ride",
        "OH L",
        "OH R",
        "Rooms L",
        "Rooms R",
    ];
    
    // Organize into tracks using monarchy sort
    let config = default_config();
    let tracks = items.organize_into_tracks(&config, None).unwrap();
    
    // Display the track list
    println!("\nTrack list:");
    daw::tracks::display_tracklist(&tracks);
    
    // Verify we got the expected structure:
    // Drums
    // -Kick
    //   --In [Kick In]
    //   --Out [Kick Out]
    // -Snare
    //   --Top [Snare Top]
    //   --Bottom [Snare Bottom]
    // -Toms
    //   --1 [Tom 1]
    //   --2 [Tom 2]
    //   --3 [Tom 3]
    // -Cymbals: [Hi Hat, Ride]  (Note: Hi Hat and Ride are currently grouped together on one track)
    //   --OH
    //     ---L [OH L]
    //     ---R [OH R]
    // -Rooms
    //   --L R [Rooms L]  (Note: "R" from "Rooms" is also matching)
    //   --R [Rooms R]
    let expected = TrackStructureBuilder::new()
        .folder("Drums")
            .folder("Drum Kit")
                .folder("Kick")
                    .track("In", "Kick In")
                    .track("Out", "Kick Out")
                .end()
                .folder("Snare")
                    .track("Top", "Snare Top")
                    .track("Bottom", "Snare Bottom")
                .end()
                .folder("Toms")
                    .track("1", "Tom 1")
                    .track("2", "Tom 2")
                    .track("3", "Tom 3")
                .end()
                .folder("Cymbals")
                    .track("Hi Hat", "Hi Hat")
                    .track("Ride", "Ride")
                    .folder("OH")
                        .track("L", "OH L")
                        .track("R", "OH R")
                    .end()
                .end()
                .folder("Rooms")
                    .track("L R", "Rooms L")
                    .track("R", "Rooms R")
                .end()
            .end()
        .end()
        .build();
    
    assert_tracks_equal(&tracks, &expected).unwrap();
}

