//! Complete drum kit template example
//!
//! This example demonstrates a full drum kit template structure with all
//! the standard drum tracks: Kick, Snare, Toms, Cymbals, and Rooms.
//!
//! Track names are formatted using TrackName so they can be parsed later.

use track_template::{Track, Template, Take, SendReceive};
use naming_convention::{TrackName, format_track_name_default};

fn main() {
    println!("Creating complete drum kit template...\n");
    
    let mut template = Template::new("Complete Drum Kit");
    
    // Helper function to create a TrackName and format it
    fn create_track_name(group_prefix: &str, sub_type: Option<&str>, track_type: Option<&str>, multi_mic: Option<&str>) -> String {
        let mut track_name = TrackName::new();
        track_name.group_prefix = Some(group_prefix.to_string());
        if let Some(st) = sub_type {
            track_name.sub_type = Some(vec![st.to_string()]);
        }
        if let Some(tt) = track_type {
            track_name.track_type = Some(tt.to_string());
        }
        if let Some(mm) = multi_mic {
            track_name.multi_mic = Some(vec![mm.to_string()]);
        }
        format_track_name_default(&track_name)
    }
    
    // ============================================
    // DRUMS (BUS) - Top level parent
    // ============================================
    let drums_name = create_track_name("D", Some("Drums"), Some("BUS"), None);
    let drums_bus = Track::new(&drums_name);
    template.add_track(drums_bus);
    
    // ============================================
    // KICK (BUS) - Child of Drums
    // ============================================
    let kick_bus_name = create_track_name("D", Some("Kick"), Some("BUS"), None);
    let mut kick_bus = Track::new(&kick_bus_name);
    kick_bus.set_parent(&drums_name);
    template.add_track(kick_bus);
    
    // Kick (SUM) - Child of Kick (BUS)
    // This contains the multi-mic tracks: In, Out, Trig
    let kick_sum_name = create_track_name("D", Some("Kick"), Some("SUM"), None);
    let mut kick_sum = Track::new(&kick_sum_name);
    kick_sum.set_parent(&kick_bus_name);
    template.add_track(kick_sum);
    
    // Kick In track - Child of Kick (SUM)
    let kick_in_name = create_track_name("D", Some("Kick"), None, Some("In"));
    let mut kick_in = Track::new(&kick_in_name);
    kick_in.set_parent(&kick_sum_name);
    kick_in.add_take(Take::new("Kick In"));
    template.add_track(kick_in);
    
    // Kick Out track - Child of Kick (SUM)
    let kick_out_name = create_track_name("D", Some("Kick"), None, Some("Out"));
    let mut kick_out = Track::new(&kick_out_name);
    kick_out.set_parent(&kick_sum_name);
    kick_out.add_take(Take::new("Kick Out"));
    template.add_track(kick_out);
    
    // Kick Trig track - Child of Kick (SUM)
    let kick_trig_name = create_track_name("D", Some("Kick"), None, Some("Trig"));
    let mut kick_trig = Track::new(&kick_trig_name);
    kick_trig.set_parent(&kick_sum_name);
    kick_trig.add_take(Take::new("Kick Trig"));
    template.add_track(kick_trig);
    
    // Kick Sub track - Direct child of Kick (BUS), not under SUM
    // (no parent send)
    let kick_sub_name = create_track_name("D", Some("Kick"), None, Some("Sub"));
    let mut kick_sub = Track::new(&kick_sub_name);
    kick_sub.set_parent(&kick_bus_name);
    kick_sub.set_parent_send(false); // Sub doesn't send to Kick
    kick_sub.add_take(Take::new("Kick Sub"));
    template.add_track(kick_sub);
    
    // Kick Ambient track - Direct child of Kick (BUS), not under SUM
    // (no parent send)
    let kick_ambient_name = create_track_name("D", Some("Kick"), None, Some("Ambient"));
    let mut kick_ambient = Track::new(&kick_ambient_name);
    kick_ambient.set_parent(&kick_bus_name);
    kick_ambient.set_parent_send(false); // Ambient doesn't send to Kick
    kick_ambient.add_take(Take::new("Kick Ambient"));
    template.add_track(kick_ambient);
    
    // ============================================
    // SNARE (BUS) - Child of Drums
    // ============================================
    let snare_bus_name = create_track_name("D", Some("Snare"), Some("BUS"), None);
    let mut snare_bus = Track::new(&snare_bus_name);
    snare_bus.set_parent(&drums_name);
    template.add_track(snare_bus);
    
    // Snare (SUM) - Child of Snare (BUS)
    // This contains the multi-mic tracks: Top, Bottom, Trig
    let snare_sum_name = create_track_name("D", Some("Snare"), Some("SUM"), None);
    let mut snare_sum = Track::new(&snare_sum_name);
    snare_sum.set_parent(&snare_bus_name);
    template.add_track(snare_sum);
    
    // Snare Top track - Child of Snare (SUM)
    let snare_top_name = create_track_name("D", Some("Snare"), None, Some("Top"));
    let mut snare_top = Track::new(&snare_top_name);
    snare_top.set_parent(&snare_sum_name);
    snare_top.add_take(Take::new("Snare Top"));
    template.add_track(snare_top);
    
    // Snare Bottom track - Child of Snare (SUM)
    let snare_bottom_name = create_track_name("D", Some("Snare"), None, Some("Bottom"));
    let mut snare_bottom = Track::new(&snare_bottom_name);
    snare_bottom.set_parent(&snare_sum_name);
    snare_bottom.add_take(Take::new("Snare Bottom"));
    template.add_track(snare_bottom);
    
    // Snare Trig track - Child of Snare (SUM)
    let snare_trig_name = create_track_name("D", Some("Snare"), None, Some("Trig"));
    let mut snare_trig = Track::new(&snare_trig_name);
    snare_trig.set_parent(&snare_sum_name);
    snare_trig.add_take(Take::new("Snare Trig"));
    template.add_track(snare_trig);
    
    // Snare Verb track - Direct child of Snare (BUS), not under SUM
    // (explicit send from Snare Top)
    let snare_verb_name = create_track_name("D", Some("Snare"), None, Some("Verb"));
    let mut snare_verb = Track::new(&snare_verb_name);
    snare_verb.set_parent(&snare_bus_name);
    snare_verb.set_parent_send(false); // Verb doesn't send to Snare
    snare_verb.add_take(Take::new("Snare Verb"));
    template.add_track(snare_verb);
    
    // Snare Fund track - Direct child of Snare (BUS), not under SUM
    let snare_fund_name = create_track_name("D", Some("Snare"), None, Some("Fund"));
    let mut snare_fund = Track::new(&snare_fund_name);
    snare_fund.set_parent(&snare_bus_name);
    snare_fund.set_parent_send(false); // Fund doesn't send to Snare
    snare_fund.add_take(Take::new("Snare Fund"));
    template.add_track(snare_fund);
    
    // Add explicit send from Snare Top to Snare Verb
    let mut snare_top_with_send = template.get_track(&snare_top_name).unwrap().clone();
    snare_top_with_send.add_send(SendReceive::new(&snare_verb_name));
    template.track_list_mut().remove_track(&snare_top_name);
    template.add_track(snare_top_with_send);
    
    // ============================================
    // TOM (BUS) - Child of Drums
    // ============================================
    let tom_bus_name = create_track_name("D", Some("Tom"), Some("BUS"), None);
    let mut tom_bus = Track::new(&tom_bus_name);
    tom_bus.set_parent(&drums_name);
    template.add_track(tom_bus);
    
    // Create multiple tom tracks (Tom 1, Tom 2, Tom 3, etc.)
    for i in 1..=4 {
        let mut track_name = TrackName::new();
        track_name.group_prefix = Some("D".to_string());
        track_name.sub_type = Some(vec!["Tom".to_string()]);
        track_name.increment = Some(i.to_string());
        let tom_name = format_track_name_default(&track_name);
        
        let mut tom = Track::new(&tom_name);
        tom.set_parent(&tom_bus_name);
        tom.add_take(Take::new(&format!("Tom {}", i)));
        template.add_track(tom);
    }
    
    // ============================================
    // CYMBALS (BUS) - Child of Drums
    // ============================================
    let cymbals_bus_name = create_track_name("D", Some("Cymbals"), Some("BUS"), None);
    let mut cymbals_bus = Track::new(&cymbals_bus_name);
    cymbals_bus.set_parent(&drums_name);
    template.add_track(cymbals_bus);
    
    // OH (Overheads) track
    let oh_name = create_track_name("D", Some("OH"), None, None);
    let mut oh = Track::new(&oh_name);
    oh.set_parent(&cymbals_bus_name);
    oh.add_take(Take::new("OH"));
    template.add_track(oh);
    
    // HiHat track
    let hihat_name = create_track_name("D", Some("HiHat"), None, None);
    let mut hihat = Track::new(&hihat_name);
    hihat.set_parent(&cymbals_bus_name);
    hihat.add_take(Take::new("HiHat"));
    template.add_track(hihat);
    
    // Ride track
    let ride_name = create_track_name("D", Some("Ride"), None, None);
    let mut ride = Track::new(&ride_name);
    ride.set_parent(&cymbals_bus_name);
    ride.add_take(Take::new("Ride"));
    template.add_track(ride);
    
    // FX Cymbals track
    let fx_cymbals_name = create_track_name("D", Some("FX Cymbals"), None, None);
    let mut fx_cymbals = Track::new(&fx_cymbals_name);
    fx_cymbals.set_parent(&cymbals_bus_name);
    fx_cymbals.add_take(Take::new("FX Cymbals"));
    template.add_track(fx_cymbals);
    
    // Mono Cym track
    let mono_cym_name = create_track_name("D", Some("Mono Cym"), None, None);
    let mut mono_cym = Track::new(&mono_cym_name);
    mono_cym.set_parent(&cymbals_bus_name);
    mono_cym.add_take(Take::new("Mono Cym"));
    template.add_track(mono_cym);
    
    // Extra Perc track
    let extra_perc_name = create_track_name("D", Some("Extra Perc"), None, None);
    let mut extra_perc = Track::new(&extra_perc_name);
    extra_perc.set_parent(&cymbals_bus_name);
    extra_perc.add_take(Take::new("Extra Perc"));
    template.add_track(extra_perc);
    
    // ============================================
    // ROOMS (BUS) - Child of Drums
    // ============================================
    let rooms_bus_name = create_track_name("D", Some("Rooms"), Some("BUS"), None);
    let mut rooms_bus = Track::new(&rooms_bus_name);
    rooms_bus.set_parent(&drums_name);
    template.add_track(rooms_bus);
    
    // Rooms track (Short/Close)
    let rooms_name = create_track_name("D", Some("Rooms"), None, None);
    let mut rooms = Track::new(&rooms_name);
    rooms.set_parent(&rooms_bus_name);
    rooms.add_take(Take::new("Rooms"));
    template.add_track(rooms);
    
    // Rooms Far track
    let rooms_far_name = create_track_name("D", Some("Rooms"), None, Some("Far"));
    let mut rooms_far = Track::new(&rooms_far_name);
    rooms_far.set_parent(&rooms_bus_name);
    rooms_far.add_take(Take::new("Rooms Far"));
    template.add_track(rooms_far);
    
    // Rooms Mono track
    let rooms_mono_name = create_track_name("D", Some("Rooms"), None, Some("Mono"));
    let mut rooms_mono = Track::new(&rooms_mono_name);
    rooms_mono.set_parent(&rooms_bus_name);
    rooms_mono.add_take(Take::new("Rooms Mono"));
    template.add_track(rooms_mono);
    
    // ============================================
    // OTHER (BUS) - Child of Drums
    // ============================================
    let other_bus_name = create_track_name("D", Some("Other"), Some("BUS"), None);
    let mut other_bus = Track::new(&other_bus_name);
    other_bus.set_parent(&drums_name);
    template.add_track(other_bus);
    
    // Random Other Drum Related Stuff - flexible, just add as needed
    // This is a placeholder for any other drum-related tracks
    
    // ============================================
    // PARALLEL (BUS) - Child of Drums
    // ============================================
    let parallel_bus_name = create_track_name("D", Some("Parallel"), Some("BUS"), None);
    let mut parallel_bus = Track::new(&parallel_bus_name);
    parallel_bus.set_parent(&drums_name);
    template.add_track(parallel_bus);
    
    // Reserved for mixing parallel fx like SMASH or COMP etc
    // This is a placeholder for parallel processing tracks
    
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
    
    // Count tracks by parent (using formatted names)
    println!("\nTracks by parent:");
    println!("  {}: {} children", drums_name, template.track_list().get_children(&drums_name).len());
    println!("  {}: {} children", kick_bus_name, template.track_list().get_children(&kick_bus_name).len());
    println!("  {}: {} children", kick_sum_name, template.track_list().get_children(&kick_sum_name).len());
    println!("  {}: {} children", snare_bus_name, template.track_list().get_children(&snare_bus_name).len());
    println!("  {}: {} children", snare_sum_name, template.track_list().get_children(&snare_sum_name).len());
    println!("  {}: {} children", tom_bus_name, template.track_list().get_children(&tom_bus_name).len());
    println!("  {}: {} children", cymbals_bus_name, template.track_list().get_children(&cymbals_bus_name).len());
    println!("  {}: {} children", rooms_bus_name, template.track_list().get_children(&rooms_bus_name).len());
    println!("  {}: {} children", other_bus_name, template.track_list().get_children(&other_bus_name).len());
    println!("  {}: {} children", parallel_bus_name, template.track_list().get_children(&parallel_bus_name).len());
}

