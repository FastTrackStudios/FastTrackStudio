//! Test file to print the default tracklist configuration
//!
//! These tests print the track hierarchies for verification.
//! They don't assert anything - they're just for visual inspection.

use fts::smart_template::presets::defaults;
use daw::tracks::PrintTrackTree;

#[test]
fn print_default() {
    let tracks = defaults::default_tracklist();
    println!("DEFAULT:\n{}", tracks.print_tree());
}

#[test]
fn print_full() {
    let tracks = defaults::full_tracklist();
    println!("FULL:\n{}", tracks.print_tree());
}

#[test]
fn print_minimal() {
    let tracks = defaults::minimal_tracklist();
    println!("MINIMAL:\n{}", tracks.print_tree());
}
