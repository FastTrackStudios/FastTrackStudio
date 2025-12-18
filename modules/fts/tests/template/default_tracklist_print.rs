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
    use fts::smart_template::presets::default_track_group::TrackGroup;
    use fts::smart_template::presets::drums::{
        Kick,
        Snare,
        Tom,
        Cymbals,
        Room,
    };
    use fts::smart_template::presets::bass::naming::BassParser;
    use fts::smart_template::presets::guitar_electric::naming::GuitarElectricParser;
    use fts::smart_template::presets::guitar_acoustic::naming::GuitarAcousticParser;
    use fts::smart_template::presets::keys::naming::KeysParser;
    use fts::smart_template::presets::synths::naming::SynthsParser;
    use fts::smart_template::presets::vocals::{
        VocalsParser,
        BGVsParser,
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
    let bass_parser = BassParser::new();
    let bass_tracks = bass_parser.default_tracklist();
    println!("{}", bass_tracks.print_tree());
    
    println!("\n--- Guitar Electric ---");
    let guitar_electric_parser = GuitarElectricParser::new();
    let guitar_electric_tracks = guitar_electric_parser.default_tracklist();
    println!("{}", guitar_electric_tracks.print_tree());
    
    println!("\n--- Guitar Acoustic ---");
    let guitar_acoustic_parser = GuitarAcousticParser::new();
    let guitar_acoustic_tracks = guitar_acoustic_parser.default_tracklist();
    println!("{}", guitar_acoustic_tracks.print_tree());
    
    println!("\n--- Keys ---");
    let keys_parser = KeysParser::new();
    let keys_tracks = keys_parser.default_tracklist();
    println!("{}", keys_tracks.print_tree());
    
    println!("\n--- Synths ---");
    let synths_parser = SynthsParser::new();
    let synths_tracks = synths_parser.default_tracklist();
    println!("{}", synths_tracks.print_tree());
    
    println!("\n--- Vocals ---");
    let vocals_parser = VocalsParser::new();
    let vocals_tracks = vocals_parser.default_tracklist();
    println!("{}", vocals_tracks.print_tree());
    
    println!("\n--- BGVs ---");
    let bgvs_parser = BGVsParser::new();
    let bgvs_tracks = bgvs_parser.default_tracklist();
    println!("{}", bgvs_tracks.print_tree());
}
