use dynamic_template::*;
use daw::tracks::item::Item;
use daw::tracks::TrackStructureBuilder;

#[test]
fn single_item_creates_track_at_deepest_group_level() {
    // Can pass strings directly - they implement Into<Item>
    let items = vec!["Kick In"];
    
    // Organize into tracks using monarchy sort
    let config = default_config();
    let tracks = items.organize_into_tracks(&config, None).unwrap();


    // Display the track list
    println!("\nTrack list:");
    daw::tracks::display_tracklist(&tracks);
    
    // Verify we got a single track "Kick" with the item "Kick In" on it
    // Philosophy: only create folders when needed to organize multiple things
    let expected = TrackStructureBuilder::new()
        .track("Kick", "Kick In")
        .build();
    
    assert_eq!(tracks, expected);
}

#[test]
fn multiple_items_of_same_subgroup_create_folder_with_subtracks() {
    // Can pass strings directly - they implement Into<Item>
    let items = vec!["Kick In", "Kick Out"];
    
    // Organize into tracks using monarchy sort
    let config = default_config();
    let tracks = items.organize_into_tracks(&config, None).unwrap();
    
    // Display the track list
    println!("\nTrack list:");
    daw::tracks::display_tracklist(&tracks);
    
    // Verify we got a folder structure:
    // Kick (folder)
    // - In: [Kick In]
    // - Out: [Kick Out]
    let expected = TrackStructureBuilder::new()
        .folder("Kick")
            .track("In", "Kick In")
            .track("Out", "Kick Out")
        .end()
        .build();
    
    assert_eq!(tracks, expected);
}

#[test]
fn multiple_subgroups_create_parent_folder_with_nested_structure() {
    // Can pass strings directly - they implement Into<Item>
    let items = vec!["Kick In", "Kick Out", "Snare Top"];
    
    // Organize into tracks using monarchy sort
    let config = default_config();
    let tracks = items.organize_into_tracks(&config, None).unwrap();
    
    // Display the track list
    println!("\nTrack list:");
    daw::tracks::display_tracklist(&tracks);
    
    // Verify we got:
    // Drums
    // -Kick
    //   --In [Kick In]
    //   --Out [Kick Out]
    // -Snare [Snare Top]
    let expected = TrackStructureBuilder::new()
        .folder("Drums")
            .folder("Kick")
                .track("In", "Kick In")
                .track("Out", "Kick Out")
            .end()
            .track("Snare", "Snare Top")
        .end()
        .build();
    
    assert_eq!(tracks, expected);
}

#[test]
fn items_in_nested_groups_create_nested_track_structure() {
    // Can pass strings directly - they implement Into<Item>
    let items = vec!["Kick In", "Kick Out", "Snare Top", "Snare Bottom"];
    
    // Organize into tracks using monarchy sort
    let config = default_config();
    let tracks = items.organize_into_tracks(&config, None).unwrap();
    
    // Display the track list
    println!("\nTrack list:");
    daw::tracks::display_tracklist(&tracks);
    
    // Verify we got:
    // Drums
    // -Kick
    //   --In [Kick In]
    //   --Out [Kick Out]
    // -Snare
    //   --Top [Snare Top]
    //   --Bottom [Snare Bottom]
    let expected = TrackStructureBuilder::new()
        .folder("Drums")
            .folder("Kick")
                .track("In", "Kick In")
                .track("Out", "Kick Out")
            .end()
            .folder("Snare")
                .track("Top", "Snare Top")
                .track("Bottom", "Snare Bottom")
            .end()
        .end()
        .build();
    
    assert_eq!(tracks, expected);
}

#[test]
fn group_separator_only_created_when_multiple_sibling_groups_exist() {
    // Can pass strings directly - they implement Into<Item>
    let items = vec!["Kick In", "Kick Out", "Snare Top", "Snare Bottom", "808 Kick", "Electronic Snare"];
    
    // Organize into tracks using monarchy sort
    let config = default_config();
    let tracks = items.organize_into_tracks(&config, None).unwrap();
    
    // Display the track list
    println!("\nTrack list:");
    daw::tracks::display_tracklist(&tracks);
    
    // Verify we got:
    // Drums
    // -Drum Kit
    //   --Kick
    //     ---In [Kick In]
    //     ---Out [Kick Out]
    //   --Snare
    //     ---Top [Snare Top]
    //     ---Bottom [Snare Bottom]
    // -Electronic Kit
    //   --Kick [808 Kick]
    //   --Snare [Electronic Snare]
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
            .end()
            .folder("Electronic Kit")
                .track("Kick", "808 Kick")
                .track("Snare", "Electronic Snare")
            .end()
        .end()
        .build();
    
    assert_eq!(tracks, expected);
}

#[test]
fn multiple_top_level_groups_create_separate_sections() {
    // Can pass strings directly - they implement Into<Item>
    let items = vec!["Kick In", "Kick Out", "Snare Top", "Snare Bottom", "808 Kick", "Electronic Snare", "Bass Guitar", "Bass Synth"];
    
    // Organize into tracks using monarchy sort
    let config = default_config();
    let tracks = items.organize_into_tracks(&config, None).unwrap();
    
    // Display the track list
    println!("\nTrack list:");
    daw::tracks::display_tracklist(&tracks);
    
    // Verify we got:
    // Drums
    // -Drum Kit
    //   --Kick
    //     ---In [Kick In]
    //     ---Out [Kick Out]
    //   --Snare
    //     ---Top [Snare Top]
    //     ---Bottom [Snare Bottom]
    // -Electronic Kit
    //   --Kick [808 Kick]
    //   --Snare [Electronic Snare]
    // Bass
    // -Guitar [Bass Guitar]
    // -Synth [Bass Synth]
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
            .end()
            .folder("Electronic Kit")
                .track("Kick", "808 Kick")
                .track("Snare", "Electronic Snare")
            .end()
        .end()
        .folder("Bass")
            .track("Guitar", "Bass Guitar")
            .track("Synth", "Bass Synth")
        .end()
        .build();
    
    assert_eq!(tracks, expected);
}

