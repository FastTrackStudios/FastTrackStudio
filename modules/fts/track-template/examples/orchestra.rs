//! Orchestra template example
//!
//! This example demonstrates the ORCHESTRA structure:
//! ORCHESTRA (BUS) -> Woodwinds -> Flute, Oboe, Clarinet, Bassoon
//! ORCHESTRA (BUS) -> Brass -> Trumpets, Horns, Trombone, Bass Trombone, Tuba
//! ORCHESTRA (BUS) -> Strings -> Violin (with increments), Violin 2, Viola, Cello, Doublebass
//! ORCHESTRA (BUS) -> Harp -> Harp Tracks
//! ORCHESTRA (BUS) -> Orch Perc -> Timpani, Perc Low, Perc Med, Perc High, Perc Metal

use track_template::{Track, Template, Take};
use naming_convention::{TrackName, format_track_name_default};

fn main() {
    println!("Creating Orchestra template...\n");
    
    let mut template = Template::new("Orchestra Template");
    
    // Helper function to create a TrackName with all components
    fn create_track_name(
        group_prefix: &str,
        sub_type: Option<&str>,
        track_type: Option<&str>,
        increment: Option<u32>,
    ) -> String {
        let mut track_name = TrackName::new();
        track_name.group_prefix = Some(group_prefix.to_string());
        if let Some(st) = sub_type {
            track_name.sub_type = Some(vec![st.to_string()]);
        }
        if let Some(tt) = track_type {
            track_name.track_type = Some(tt.to_string());
        }
        if let Some(inc) = increment {
            track_name.increment = Some(inc.to_string());
        }
        format_track_name_default(&track_name)
    }
    
    // ============================================
    // ORCHESTRA (BUS) - Top level parent
    // ============================================
    let orchestra_bus_name = create_track_name("O", Some("Orchestra"), Some("BUS"), None);
    let orchestra_bus = Track::new(&orchestra_bus_name);
    template.add_track(orchestra_bus);
    
    // ============================================
    // WOODWINDS (BUS) - Child of Orchestra
    // ============================================
    let woodwinds_bus_name = create_track_name("O", Some("Woodwinds"), Some("BUS"), None);
    let mut woodwinds_bus = Track::new(&woodwinds_bus_name);
    woodwinds_bus.set_parent(&orchestra_bus_name);
    template.add_track(woodwinds_bus);
    
    let woodwind_instruments = vec!["Flute", "Oboe", "Clarinet", "Bassoon"];
    for instrument in &woodwind_instruments {
        let mut track_name_obj = TrackName::new();
        track_name_obj.group_prefix = Some("O".to_string());
        track_name_obj.sub_type = Some(vec![instrument.to_string()]);
        let formatted_name = format_track_name_default(&track_name_obj);
        
        let mut track = Track::new(&formatted_name);
        track.set_parent(&woodwinds_bus_name);
        track.add_take(Take::new(*instrument));
        template.add_track(track);
    }
    
    // ============================================
    // BRASS (BUS) - Child of Orchestra
    // ============================================
    let brass_bus_name = create_track_name("O", Some("Brass"), Some("BUS"), None);
    let mut brass_bus = Track::new(&brass_bus_name);
    brass_bus.set_parent(&orchestra_bus_name);
    template.add_track(brass_bus);
    
    let brass_instruments = vec!["Trumpets", "Horns", "Trombone", "Bass Trombone", "Tuba"];
    for instrument in &brass_instruments {
        let mut track_name_obj = TrackName::new();
        track_name_obj.group_prefix = Some("O".to_string());
        track_name_obj.sub_type = Some(vec![instrument.to_string()]);
        let formatted_name = format_track_name_default(&track_name_obj);
        
        let mut track = Track::new(&formatted_name);
        track.set_parent(&brass_bus_name);
        track.add_take(Take::new(*instrument));
        template.add_track(track);
    }
    
    // ============================================
    // STRINGS (BUS) - Child of Orchestra
    // ============================================
    let strings_bus_name = create_track_name("O", Some("Strings"), Some("BUS"), None);
    let mut strings_bus = Track::new(&strings_bus_name);
    strings_bus.set_parent(&orchestra_bus_name);
    template.add_track(strings_bus);
    
    // Violin with increments (Violin 1, Violin 2, etc.)
    for i in 1..=2 {
        let mut track_name_obj = TrackName::new();
        track_name_obj.group_prefix = Some("O".to_string());
        track_name_obj.sub_type = Some(vec!["Violin".to_string()]);
        track_name_obj.increment = Some(i.to_string());
        let formatted_name = format_track_name_default(&track_name_obj);
        
        let mut track = Track::new(&formatted_name);
        track.set_parent(&strings_bus_name);
        track.add_take(Take::new(&format!("Violin {}", i)));
        template.add_track(track);
    }
    
    // Other string instruments
    let other_strings = vec!["Viola", "Cello", "Doublebass"];
    for instrument in &other_strings {
        let mut track_name_obj = TrackName::new();
        track_name_obj.group_prefix = Some("O".to_string());
        track_name_obj.sub_type = Some(vec![instrument.to_string()]);
        let formatted_name = format_track_name_default(&track_name_obj);
        
        let mut track = Track::new(&formatted_name);
        track.set_parent(&strings_bus_name);
        track.add_take(Take::new(*instrument));
        template.add_track(track);
    }
    
    // ============================================
    // HARP (BUS) - Child of Orchestra
    // ============================================
    let harp_bus_name = create_track_name("O", Some("Harp"), Some("BUS"), None);
    let mut harp_bus = Track::new(&harp_bus_name);
    harp_bus.set_parent(&orchestra_bus_name);
    template.add_track(harp_bus);
    
    // Harp tracks
    let mut harp_track_name_obj = TrackName::new();
    harp_track_name_obj.group_prefix = Some("O".to_string());
    harp_track_name_obj.sub_type = Some(vec!["Harp".to_string()]);
    let harp_track_name = format_track_name_default(&harp_track_name_obj);
    
    let mut harp_track = Track::new(&harp_track_name);
    harp_track.set_parent(&harp_bus_name);
    harp_track.add_take(Take::new("Harp"));
    template.add_track(harp_track);
    
    // ============================================
    // ORCH PERC (BUS) - Child of Orchestra
    // ============================================
    let orch_perc_bus_name = create_track_name("O", Some("Orch Perc"), Some("BUS"), None);
    let mut orch_perc_bus = Track::new(&orch_perc_bus_name);
    orch_perc_bus.set_parent(&orchestra_bus_name);
    template.add_track(orch_perc_bus);
    
    let perc_instruments = vec!["Timpani", "Perc Low", "Perc Med", "Perc High", "Perc Metal"];
    for instrument in &perc_instruments {
        let mut track_name_obj = TrackName::new();
        track_name_obj.group_prefix = Some("O".to_string());
        track_name_obj.sub_type = Some(vec![instrument.to_string()]);
        let formatted_name = format_track_name_default(&track_name_obj);
        
        let mut track = Track::new(&formatted_name);
        track.set_parent(&orch_perc_bus_name);
        track.add_take(Take::new(*instrument));
        template.add_track(track);
    }
    
    // ============================================
    // Display the complete template
    // ============================================
    println!("{}", template);
    
    // ============================================
    // Verify structure
    // ============================================
    println!("\n=== Template Statistics ===");
    println!("Total tracks: {}", template.track_list().len());
    println!("Root tracks: {}", template.track_list().get_roots().len());
    
    println!("\nTracks by parent:");
    println!("  {}: {} children", orchestra_bus_name, template.track_list().get_children(&orchestra_bus_name).len());
    println!("  {}: {} children", woodwinds_bus_name, template.track_list().get_children(&woodwinds_bus_name).len());
    println!("  {}: {} children", brass_bus_name, template.track_list().get_children(&brass_bus_name).len());
    println!("  {}: {} children", strings_bus_name, template.track_list().get_children(&strings_bus_name).len());
    println!("  {}: {} children", harp_bus_name, template.track_list().get_children(&harp_bus_name).len());
    println!("  {}: {} children", orch_perc_bus_name, template.track_list().get_children(&orch_perc_bus_name).len());
}

