//! Default track list structures for testing and examples
//!
//! This module provides pre-built track hierarchies that can be used
//! in tests and as examples of how to structure tracks.

use daw::tracks::{Track, AddChild, PrintTrackTree};

/// Build the default track structure for "Marc Martel - Don't Stop Me Now"
///
/// Structure: BUS -> SUM -> individual tracks
/// Direct processing tracks (Sub, Ambient, Verb) are children of BUS, not SUM
pub fn build_marc_martel_tracks() -> Vec<Track> {
    // Kick hierarchy: Kick (BUS) -> Kick (SUM) -> individual tracks
    // Multi-mic tracks: In, Out, Trig are children of Kick (SUM)
    let kick_sum_tracks = Track::new("Kick (SUM)".to_string())
        .add_child(vec![
            Track::new("Kick In".to_string()),
            Track::new("Kick Out".to_string()),
            Track::new("Kick Trig".to_string()),
        ]);
    
    // Processing tracks: Sub, Ambient are direct children of Kick (BUS), not SUM
    let kick_sub = Track::new("Kick Sub".to_string());
    let kick_amb = Track::new("Kick Ambient".to_string());
    
    // Combine all children: SUM folder + Sub + Ambient, then add to Kick (BUS)
    let mut all_kick_children = kick_sum_tracks;
    all_kick_children.push(kick_sub);
    all_kick_children.push(kick_amb);
    
    let kick_tracks = Track::new("Kick".to_string())
        .add_child(all_kick_children);
    
    // Snare hierarchy: Snare (BUS) -> Snare (SUM) -> individual tracks
    // Multi-mic tracks: Top, Bottom, Trig are children of Snare (SUM)
    let snare_sum_tracks = Track::new("Snare (SUM)".to_string())
        .add_child(vec![
            Track::new("Snare Top".to_string()),
            Track::new("Snare Bottom".to_string()),
            Track::new("Snare Trig".to_string()),
        ]);
    
    // Processing track: Verb is direct child of Snare (BUS), not SUM
    let snare_verb = Track::new("Snare Verb".to_string());
    
    // Combine all children: SUM folder + Verb, then add to Snare (BUS)
    let mut all_snare_children = snare_sum_tracks;
    all_snare_children.push(snare_verb);
    
    let snare_tracks = Track::new("Snare".to_string())
        .add_child(all_snare_children);
    
    // Tom hierarchy: Tom (BUS) -> Tom (SUM) -> individual tracks
    let tom1 = Track::new("Tom1".to_string());
    let tom2 = Track::new("Tom2".to_string());
    
    let tom_sum_tracks = Track::new("Tom (SUM)".to_string())
        .add_child(vec![tom1, tom2]);
    
    let tom_tracks = Track::new("Tom".to_string())
        .add_child(tom_sum_tracks);
    
    // Room hierarchy: Room (BUS) -> Room (SUM) -> individual tracks
    let rooms = Track::new("Rooms".to_string());
    
    let room_sum_tracks = Track::new("Room (SUM)".to_string())
        .add_child(rooms);
    
    let room_tracks = Track::new("Room".to_string())
        .add_child(room_sum_tracks);
    
    // Combine all tracks in order
    let mut all_tracks = Vec::new();
    all_tracks.extend(kick_tracks);
    all_tracks.extend(snare_tracks);
    all_tracks.extend(tom_tracks);
    all_tracks.extend(room_tracks);
    
    all_tracks
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_print_track_tree() {
        let tracks = build_marc_martel_tracks();
        let tree = tracks.print_tree();
        println!("{}", tree);
        
        // Verify the tree contains expected track names
        assert!(tree.contains("Kick"));
        assert!(tree.contains("Kick (SUM)"));
        assert!(tree.contains("Kick In"));
        assert!(tree.contains("Snare"));
        assert!(tree.contains("Tom"));
        assert!(tree.contains("Room"));
    }
}
