//! Default track list structures for testing and examples
//!
//! This module provides pre-built track hierarchies organized hierarchically
//! using the TrackGroup trait. Each group implements TrackGroup to provide
//! its default tracklist, and categories combine groups.

use daw::tracks::Track;
use crate::smart_template::core::traits::GroupExt;
use crate::smart_template::presets::{
    drums, bass, guitar_electric, guitar_acoustic, keys, synths, vocals
};

// ============================================================================
// Category Default Tracklists (combine groups)
// ============================================================================

/// Default tracklist for Drums category
pub fn default_drums_tracklist() -> Vec<Track> {
    drums::DrumKit::get_default_tracklist()
}

/// Default tracklist for Guitars category
pub fn default_guitars_tracklist() -> Vec<Track> {
    let mut all_tracks = Vec::new();
    all_tracks.extend(guitar_electric::GuitarElectric::get_default_tracklist());
    all_tracks.extend(guitar_acoustic::GuitarAcoustic::get_default_tracklist());
    all_tracks
}

/// Default tracklist for Vocals category
pub fn default_vocals_category_tracklist() -> Vec<Track> {
    vocals::Vocals::get_default_tracklist()
}

// ============================================================================
// Top-Level Default Tracklists
// ============================================================================

/// Build the full track list for all groups (maximum detail)
pub fn full_tracklist() -> Vec<Track> {
    let mut all_tracks = Vec::new();
    
    all_tracks.extend(drums::DrumKit::get_full_template().tracks);
    all_tracks.extend(bass::Bass::get_full_template().tracks);
    all_tracks.extend(guitar_electric::GuitarElectric::get_full_template().tracks);
    all_tracks.extend(guitar_acoustic::GuitarAcoustic::get_full_template().tracks);
    all_tracks.extend(keys::Keys::get_full_template().tracks);
    all_tracks.extend(synths::Synths::get_full_template().tracks);
    all_tracks.extend(vocals::Vocals::get_full_template().tracks);
    
    all_tracks
}

/// Build the default track list for all groups
pub fn default_tracklist() -> Vec<Track> {
    let mut all_tracks = Vec::new();
    
    all_tracks.extend(drums::DrumKit::get_default_template().tracks);
    all_tracks.extend(bass::Bass::get_default_template().tracks);
    all_tracks.extend(guitar_electric::GuitarElectric::get_default_template().tracks);
    all_tracks.extend(guitar_acoustic::GuitarAcoustic::get_default_template().tracks);
    all_tracks.extend(keys::Keys::get_default_template().tracks);
    all_tracks.extend(synths::Synths::get_default_template().tracks);
    all_tracks.extend(vocals::Vocals::get_default_template().tracks);
    
    all_tracks
}

/// Build the minimal track list for all groups (only essential tracks)
pub fn minimal_tracklist() -> Vec<Track> {
    let mut all_tracks = Vec::new();
    
    all_tracks.extend(drums::DrumKit::get_minimal_template().tracks);
    all_tracks.extend(bass::Bass::get_minimal_template().tracks);
    all_tracks.extend(guitar_electric::GuitarElectric::get_minimal_template().tracks);
    all_tracks.extend(guitar_acoustic::GuitarAcoustic::get_minimal_template().tracks);
    all_tracks.extend(keys::Keys::get_minimal_template().tracks);
    all_tracks.extend(synths::Synths::get_minimal_template().tracks);
    all_tracks.extend(vocals::Vocals::get_minimal_template().tracks);
    
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
    fn test_tracklist_levels() {
        // Full
        let full = full_tracklist();
        println!("FULL HIERARCHY:\n{}", full.print_tree());
        assert!(full.iter().any(|t| t.name.0 == "Kick (SUM)"));
        
        // Default
        let default = default_tracklist();
        println!("DEFAULT HIERARCHY:\n{}", default.print_tree());
        // Default should have Kick folder but maybe not Sum folder depending on implementation
        assert!(default.iter().any(|t| t.name.0 == "Kick"));
        
        // Minimal
        let minimal = minimal_tracklist();
        println!("MINIMAL HIERARCHY:\n{}", minimal.print_tree());
        // Minimal should have Kick In directly
        assert!(minimal.iter().any(|t| t.name.0 == "Kick In"));
    }
}
