use dynamic_template::*;
use daw::tracks::item::Item;
use daw::tracks::TrackStructureBuilder;

#[test]
fn single_item_matches_with_deepest_group_parent() {
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
fn multiple_items_creates_folder_with_subtracks() {
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

