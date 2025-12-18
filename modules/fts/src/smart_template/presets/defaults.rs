//! Default track list structures for testing and examples
//!
//! This module provides pre-built track hierarchies organized hierarchically
//! using the TrackGroup trait. Each group implements TrackGroup to provide
//! its default tracklist, and categories combine groups.

use daw::tracks::Track;
use crate::smart_template::presets::{
    drums, bass, guitar_electric, guitar_acoustic, keys, synths, vocals
};

// ============================================================================
// Category Default Tracklists (combine groups)
// ============================================================================

/// Default tracklist for Drums category
pub fn default_drums_tracklist() -> Vec<Track> {
    drums::default_tracklist()
}

/// Default tracklist for Guitars category
pub fn default_guitars_tracklist() -> Vec<Track> {
    let mut all_tracks = Vec::new();
    all_tracks.extend(guitar_electric::default_tracklist());
    all_tracks.extend(guitar_acoustic::default_tracklist());
    all_tracks
}

/// Default tracklist for Vocals category
pub fn default_vocals_category_tracklist() -> Vec<Track> {
    vocals::default_tracklist()
}

// ============================================================================
// Top-Level Default Tracklist
// ============================================================================

/// Build the default track list for all groups
pub fn default_tracklist() -> Vec<Track> {
    let mut all_tracks = Vec::new();
    
    // Drums category
    all_tracks.extend(default_drums_tracklist());
    
    // Bass
    all_tracks.extend(bass::default_tracklist());
    
    // Guitars category
    all_tracks.extend(default_guitars_tracklist());
    
    // Keys
    all_tracks.extend(keys::default_tracklist());
    
    // Synths
    all_tracks.extend(synths::default_tracklist());
    
    // Vocals category
    all_tracks.extend(default_vocals_category_tracklist());
    
    all_tracks
}

// ============================================================================
// Legacy Support
// ============================================================================

/// Build the default track structure for "Marc Martel - Don't Stop Me Now"
#[deprecated(note = "Use default_tracklist() instead")]
pub fn build_marc_martel_tracks() -> Vec<Track> {
    default_tracklist()
}

#[cfg(test)]
mod tests {
    use super::*;
    use daw::tracks::PrintTrackTree;
    
    #[test]
    fn test_default_tracklist() {
        let tracks = default_tracklist();
        let tree = tracks.print_tree();
        println!("{}", tree);
        
        // Verify the tree contains expected track names
        assert!(tree.contains("Kick"));
        assert!(tree.contains("Snare"));
        assert!(tree.contains("Bass"));
        assert!(tree.contains("Lead Vocal"));
    }
}
