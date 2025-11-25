//! Basic example of using the track template engine

use track_template::{TrackMatcher, Track, Template, Take, SendReceive};

fn main() {
    // Create a matcher
    let matcher = TrackMatcher::new();
    
    // Parse some track names
    let kick_in = matcher.parse("D Kick In");
    let kick_out = matcher.parse("D Kick Out");
    let snare_top = matcher.parse("D Snare Top");
    
    println!("Parsed track names:");
    println!("  Kick In: {:?}", kick_in);
    println!("  Kick Out: {:?}", kick_out);
    println!("  Snare Top: {:?}", snare_top);
    
    // Create a template
    let mut template = Template::new("Drums Template");
    
    // Create the parent track (BUS)
    let kick_bus = Track::with_type("Kick", "BUS");
    template.add_track(kick_bus);
    
    // Create child tracks for Kick In and Kick Out
    // These are actual tracks that items will be placed on
    let mut kick_in_track = Track::new("Kick In");
    kick_in_track.set_parent("Kick");
    kick_in_track.set_parent_send(true); // Auto-send to parent Kick (BUS)
    // When items named "Kick In" are matched, they go as takes on this track
    kick_in_track.add_take(Take::new("Kick In"));
    template.add_track(kick_in_track);
    
    let mut kick_out_track = Track::new("Kick Out");
    kick_out_track.set_parent("Kick");
    kick_out_track.set_parent_send(true); // Auto-send to parent Kick (BUS)
    // When items named "Kick Out" are matched, they go as takes on this track
    kick_out_track.add_take(Take::new("Kick Out"));
    template.add_track(kick_out_track);
    
    // Example: Create a track that doesn't send to parent
    let mut kick_sub_track = Track::new("Kick Sub");
    kick_sub_track.set_parent("Kick");
    kick_sub_track.set_parent_send(false); // Disable auto-send to parent
    template.add_track(kick_sub_track);
    
    // Create Snare parent track
    let snare_bus = Track::with_type("Snare", "BUS");
    template.add_track(snare_bus);
    
    // Create child track for Snare Top
    let mut snare_top_track = Track::new("Snare Top");
    snare_top_track.set_parent("Snare");
    snare_top_track.set_parent_send(true); // Auto-send to parent Snare (BUS)
    // When items named "Snare Top" are matched, they go as takes on this track
    snare_top_track.add_take(Take::new("Snare Top"));
    template.add_track(snare_top_track);
    
    // Create a matcher from the template
    let template_matcher = template.create_matcher();
    
    // Try to match parsed names to tracks in the template
    println!("\nMatching results:");
    
    if let Some(result) = template_matcher.find_best_match(&kick_in) {
        println!("  Kick In matched: {} (score: {}, use_takes: {})", 
            result.track.name, result.score, result.use_takes);
    } else {
        println!("  Kick In: No match found");
    }
    
    if let Some(result) = template_matcher.find_best_match(&snare_top) {
        println!("  Snare Top matched: {} (score: {}, use_takes: {})", 
            result.track.name, result.score, result.use_takes);
    } else {
        println!("  Snare Top: No match found");
    }
    
    // Test playlist variation matching
    println!("\nTesting playlist variations:");
    
    // Create a new template for playlist testing
    let mut playlist_template = Template::new("Playlist Test Template");
    
    // Add a track with playlist .1 (the parser should extract this)
    let kick_track_playlist1 = Track::new("D Kick In .1");
    playlist_template.add_track(kick_track_playlist1);
    
    // Create a matcher from the template
    let playlist_matcher = playlist_template.create_matcher();
    
    // Parse a track name with playlist .2
    let kick_in_playlist2 = playlist_matcher.parse("D Kick In .2");
    println!("  Parsed 'D Kick In .2': playlist = {:?}", kick_in_playlist2.playlist);
    
    // Try to match the second playlist variation
    if let Some(result) = playlist_matcher.find_best_match(&kick_in_playlist2) {
        println!("  Kick In .2 matched: {} (score: {}, use_takes: {})", 
            result.track.name, result.score, result.use_takes);
        if result.use_takes {
            println!("    ✓ Will use takes (item lanes) for playlist variation");
        }
    } else {
        println!("  Kick In .2: No match found");
    }
    
    // Example: Add an explicit send (for tracks without parent-child relationship)
    // e.g., "Snare Top" sending to "Snare Trig"
    let mut snare_top_track = template.get_track("Snare Top").unwrap().clone();
    snare_top_track.add_send(SendReceive::new("Snare Trig"));
    template.track_list_mut().remove_track("Snare Top");
    template.add_track(snare_top_track);
    
    println!("\nTemplate structure (tree view):");
    println!("{}", template);
}

