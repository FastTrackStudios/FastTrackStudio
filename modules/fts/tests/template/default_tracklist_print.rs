//! Test file to print the default tracklist configuration
//!
//! This test file allows you to see the complete default tracklist structure
//! by printing it in a tree format.

use fts::smart_template::presets::defaults::{
    default_tracklist,
    default_drums_tracklist,
    default_guitars_tracklist,
    default_vocals_category_tracklist,
};

use daw::tracks::PrintTrackTree;
use fts::smart_template::Group;

#[test]
fn print_default_tracklist() {
    println!("\n=== DEFAULT TRACKLIST ===\n");
    let tracks = default_tracklist();
    let tree = tracks.print_tree();
    println!("{}", tree);
}

#[test]
fn print_drums_tracklist() {
    println!("\n=== DRUMS TRACKLIST ===\n");
    let tracks = default_drums_tracklist();
    let tree = tracks.print_tree();
    println!("{}", tree);
}

#[test]
fn print_guitars_tracklist() {
    println!("\n=== GUITARS TRACKLIST ===\n");
    let tracks = default_guitars_tracklist();
    let tree = tracks.print_tree();
    println!("{}", tree);
}

#[test]
fn print_vocals_category_tracklist() {
    println!("\n=== VOCALS CATEGORY TRACKLIST ===\n");
    let tracks = default_vocals_category_tracklist();
    let tree = tracks.print_tree();
    println!("{}", tree);
}

#[test]
fn print_individual_group_tracklists() {
    use fts::smart_template::presets::drums::{
        Kick,
        Snare,
        Tom,
        Cymbals,
        Room,
    };
    use fts::smart_template::presets::bass::Bass;
    use fts::smart_template::presets::guitar_electric::GuitarElectric;
    use fts::smart_template::presets::guitar_acoustic::GuitarAcoustic;
    use fts::smart_template::presets::keys::Keys;
    use fts::smart_template::presets::synths::Synths;
    use fts::smart_template::presets::vocals::{
        Vocals,
        bgvs::BGVs,
    };
    
    println!("\n=== INDIVIDUAL GROUP TRACKLISTS ===\n");
    
    println!("\n--- Kick ---");
    let kick = Kick::new();
    let kick_tracks = kick.default_tracklist();
    println!("{}", kick_tracks.print_tree());
    
    println!("\n--- Snare ---");
    let snare = Snare::new();
    let snare_tracks = snare.default_tracklist();
    println!("{}", snare_tracks.print_tree());
    
    println!("\n--- Tom ---");
    let tom = Tom::new();
    let tom_tracks = tom.default_tracklist();
    println!("{}", tom_tracks.print_tree());
    
    println!("\n--- Cymbals ---");
    let cymbals = Cymbals::new();
    let cymbals_tracks = cymbals.default_tracklist();
    println!("{}", cymbals_tracks.print_tree());
    
    println!("\n--- Room ---");
    let room = Room::new();
    let room_tracks = room.default_tracklist();
    println!("{}", room_tracks.print_tree());
    
    println!("\n--- Bass ---");
    let bass = Bass::new();
    let bass_tracks = bass.default_tracklist();
    println!("{}", bass_tracks.print_tree());
    
    println!("\n--- Guitar Electric (By Performer) ---");
    let guitar_electric = GuitarElectric::new();
    let guitar_electric_tracks = guitar_electric.default_tracklist();
    println!("{}", guitar_electric_tracks.print_tree());
    
    println!("\n--- Guitar Electric (By Arrangement) ---");
    let guitar_electric_arr = GuitarElectric::with_mode(fts::smart_template::core::models::organization::OrganizationMode::ByArrangement);
    let guitar_electric_arr_tracks = guitar_electric_arr.default_tracklist();
    println!("{}", guitar_electric_arr_tracks.print_tree());
    
    println!("\n--- Guitar Acoustic (By Performer) ---");
    let guitar_acoustic = GuitarAcoustic::new();
    let guitar_acoustic_tracks = guitar_acoustic.default_tracklist();
    println!("{}", guitar_acoustic_tracks.print_tree());
    
    println!("\n--- Guitar Acoustic (By Arrangement) ---");
    let guitar_acoustic_arr = GuitarAcoustic::with_mode(fts::smart_template::core::models::organization::OrganizationMode::ByArrangement);
    let guitar_acoustic_arr_tracks = guitar_acoustic_arr.default_tracklist();
    println!("{}", guitar_acoustic_arr_tracks.print_tree());
    
    println!("\n--- Keys ---");
    let keys = Keys::new();
    let keys_tracks = keys.default_tracklist();
    println!("{}", keys_tracks.print_tree());
    
    println!("\n--- Synths ---");
    let synths = Synths::new();
    let synths_tracks = synths.default_tracklist();
    println!("{}", synths_tracks.print_tree());
    
    println!("\n--- Vocals ---");
    let vocals = Vocals::new();
    let vocals_tracks = vocals.default_tracklist();
    println!("{}", vocals_tracks.print_tree());
    
    println!("\n--- BGVs ---");
    let bgvs = BGVs::new();
    let bgvs_tracks = bgvs.default_tracklist();
    println!("{}", bgvs_tracks.print_tree());
}
