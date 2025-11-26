//! Test for Marc Martel - Don't Stop Me Now (Cover) Multitracks
//!
//! Track Structure:
//! - DRUMS (BUS) -> Kick (BUS) -> Kick (SUM) -> Kick In, Kick Out, Kick Sample
//! - DRUMS (BUS) -> Snare (BUS) -> Snare (SUM) -> Snare Top, Snare Bottom, Snare Sample, Snare Sample Two
//! - DRUMS (BUS) -> Toms (BUS) -> Tom1, Tom2
//! - DRUMS (BUS) -> Cymbals (BUS) -> HighHat, OH
//! - DRUMS (BUS) -> Rooms (BUS) -> Rooms
//! - DRUMS (BUS) -> Percussion (BUS) -> Percussion
//! - BASS -> Bass Guitar (BUS) -> Bass DI
//! - KEYS (BUS) -> Piano
//! - GTR E (BUS) -> Lead (SUM) -> L, R
//! - GTR E (BUS) -> Clean (SUM) -> DI L, DI R
//! - VOCALS (BUS) -> Lead Vocal (SUM) -> Vocal
//! - VOCALS (BUS) -> Background Vocals (SUM) -> BGV1, BGV2, BGV3, BGV4
//! - VOCALS (BUS) -> VocalFX (BUS) -> Vocal.Eko.Plate, Vocal.Magic
//! - UNSORTED (BUS) -> H3000.One, H3000.Two, H3000.Three

use track_template::{Track, Template, Take, SortStatus, DisplayMode};
use naming_convention::{SimpleParser, create_default_groups, format_track_name_default, TrackName};

#[test]
fn test_marc_martel_dont_stop_me_now() {
    let track_names = vec![
        "01.Kick In_01.wav", // Kick In
        "02.Kick Out_01.wav", // Kick Out
        "03.Kick Sample_01.wav", // Kick Trig
        "04.Snare Top_01.wav", // Snare Top
        "05.Snare Bottom_01.wav", // Snare Bottom
        "06.Snare Sample_01.wav", // Snare Trig
        "07.Snare Sample Two_01.wav", // Snare Trig 2
        "08.Tom1_01.wav", // Tom 1
        "09.Tom2_01.wav", // Tom 2
        "10.HighHat_01.wav", // HiHat
        "11.OH_01.wav", // Overhead
        "12.Rooms_01.wav", // Rooms
        "13.Percussion_01.wav", // Percussion
        "14.Bass DI_01.wav", // bass DI/
        "15.Piano_01.wav",
        "16.Lead Guitar Amplitube Left_01.wav",
        "17.Lead Guitar Amplitube Right_01.wav",
        "18.Lead Guitar Clean DI Left_01.wav",
        "19.Lead Guitar Clean DI Right_01.wav",
        "20.Vocal_01.wav",
        "21.H3000.One_01.wav", //Not Sorted
        "22.H3000.Two_01.wav", //Not Sorted 
        "23.H3000.Three_01.wav", //Not Sorted
        "24.Vocal.Eko.Plate_01.wav", // Vocal FX (need new group for this)
        "25.Vocal.Magic_01.wav", // Vocal FX (need new group for this)
        "26.BGV1_01.wav", // Background Vocals 1
        "27.BGV2_01.wav", // Background Vocals 2
        "28.BGV3_01.wav", // Background Vocals 3
        "29.BGV4_01.wav", // Background Vocals 4
    ];

    let parser = SimpleParser::new(create_default_groups());
    
    println!("\n=== Parsing Marc Martel - Don't Stop Me Now ===\n");
    
    for name in &track_names {
        let parsed = parser.parse(name);
        let formatted = format_track_name_default(&parsed);
        println!("{} -> {}", name, formatted);
    }
    
    // Helper function to create a TrackName
    fn create_track_name(
        group_prefix: &str,
        sub_type: Option<&str>,
        track_type: Option<&str>,
        multi_mic: Option<&str>,
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
        if let Some(mm) = multi_mic {
            track_name.multi_mic = Some(vec![mm.to_string()]);
        }
        if let Some(inc) = increment {
            track_name.increment = Some(inc.to_string());
        }
        format_track_name_default(&track_name)
    }
    
    // ============================================
    // Build Expected Track Structure
    // ============================================
    let mut template = Template::new("Marc Martel - Don't Stop Me Now");
    
    // ============================================
    // DRUMS (BUS) - Top level parent
    // ============================================
    let drums_bus_name = create_track_name("D", Some("Drums"), Some("BUS"), None, None);
    let drums_bus = Track::new(&drums_bus_name);
    template.add_track(drums_bus);
    
    // ============================================
    // KICK (BUS) - Child of Drums
    // ============================================
    let kick_bus_name = create_track_name("D", Some("Kick"), Some("BUS"), None, None);
    let mut kick_bus = Track::new(&kick_bus_name);
    kick_bus.set_parent(&drums_bus_name);
    template.add_track(kick_bus);
    
    // Kick (SUM) - Child of Kick (BUS)
    let kick_sum_name = create_track_name("D", Some("Kick"), Some("SUM"), None, None);
    let mut kick_sum = Track::new(&kick_sum_name);
    kick_sum.set_parent(&kick_bus_name);
    template.add_track(kick_sum);
    
    // Kick In
    let kick_in_name = create_track_name("D", Some("Kick"), None, Some("In"), None);
    let mut kick_in = Track::new(&kick_in_name);
    kick_in.set_parent(&kick_sum_name);
    kick_in.add_take(Take::new("Kick In"));
    template.add_track(kick_in);
    
    // Kick Out
    let kick_out_name = create_track_name("D", Some("Kick"), None, Some("Out"), None);
    let mut kick_out = Track::new(&kick_out_name);
    kick_out.set_parent(&kick_sum_name);
    kick_out.add_take(Take::new("Kick Out"));
    template.add_track(kick_out);
    
    // Kick Sample (Trig)
    let kick_sample_name = create_track_name("D", Some("Kick"), None, Some("Trig"), None);
    let mut kick_sample = Track::new(&kick_sample_name);
    kick_sample.set_parent(&kick_sum_name);
    kick_sample.add_take(Take::new("Kick Sample"));
    template.add_track(kick_sample);
    
    // ============================================
    // SNARE (BUS) - Child of Drums
    // ============================================
    let snare_bus_name = create_track_name("D", Some("Snare"), Some("BUS"), None, None);
    let mut snare_bus = Track::new(&snare_bus_name);
    snare_bus.set_parent(&drums_bus_name);
    template.add_track(snare_bus);
    
    // Snare (SUM) - Child of Snare (BUS)
    let snare_sum_name = create_track_name("D", Some("Snare"), Some("SUM"), None, None);
    let mut snare_sum = Track::new(&snare_sum_name);
    snare_sum.set_parent(&snare_bus_name);
    template.add_track(snare_sum);
    
    // Snare Top
    let snare_top_name = create_track_name("D", Some("Snare"), None, Some("Top"), None);
    let mut snare_top = Track::new(&snare_top_name);
    snare_top.set_parent(&snare_sum_name);
    snare_top.add_take(Take::new("Snare Top"));
    template.add_track(snare_top);
    
    // Snare Bottom
    let snare_bottom_name = create_track_name("D", Some("Snare"), None, Some("Bottom"), None);
    let mut snare_bottom = Track::new(&snare_bottom_name);
    snare_bottom.set_parent(&snare_sum_name);
    snare_bottom.add_take(Take::new("Snare Bottom"));
    template.add_track(snare_bottom);
    
    // Snare Sample (Trig)
    let snare_sample_name = create_track_name("D", Some("Snare"), None, Some("Trig"), None);
    let mut snare_sample = Track::new(&snare_sample_name);
    snare_sample.set_parent(&snare_sum_name);
    snare_sample.add_take(Take::new("Snare Sample"));
    template.add_track(snare_sample);
    
    // Snare Sample Two (Trig 2)
    let snare_sample_two_name = create_track_name("D", Some("Snare"), None, Some("Trig"), Some(2));
    let mut snare_sample_two = Track::new(&snare_sample_two_name);
    snare_sample_two.set_parent(&snare_sum_name);
    snare_sample_two.add_take(Take::new("Snare Sample Two"));
    template.add_track(snare_sample_two);
    
    // ============================================
    // TOMS (BUS) - Child of Drums
    // ============================================
    let toms_bus_name = create_track_name("D", Some("Tom"), Some("BUS"), None, None);
    let mut toms_bus = Track::new(&toms_bus_name);
    toms_bus.set_parent(&drums_bus_name);
    template.add_track(toms_bus);
    
    // Tom 1
    let tom1_name = create_track_name("D", Some("Tom"), None, None, Some(1));
    let mut tom1 = Track::new(&tom1_name);
    tom1.set_parent(&toms_bus_name);
    tom1.add_take(Take::new("Tom1"));
    template.add_track(tom1);
    
    // Tom 2
    let tom2_name = create_track_name("D", Some("Tom"), None, None, Some(2));
    let mut tom2 = Track::new(&tom2_name);
    tom2.set_parent(&toms_bus_name);
    tom2.add_take(Take::new("Tom2"));
    template.add_track(tom2);
    
    // ============================================
    // CYMBALS (BUS) - Child of Drums
    // ============================================
    let cymbals_bus_name = create_track_name("D", Some("Cymbals"), Some("BUS"), None, None);
    let mut cymbals_bus = Track::new(&cymbals_bus_name);
    cymbals_bus.set_parent(&drums_bus_name);
    template.add_track(cymbals_bus);
    
    // HighHat
    let hihat_name = create_track_name("D", Some("HiHat"), None, None, None);
    let mut hihat = Track::new(&hihat_name);
    hihat.set_parent(&cymbals_bus_name);
    hihat.add_take(Take::new("HighHat"));
    template.add_track(hihat);
    
    // OH (Overhead)
    let oh_name = create_track_name("D", Some("OH"), None, None, None);
    let mut oh = Track::new(&oh_name);
    oh.set_parent(&cymbals_bus_name);
    oh.add_take(Take::new("OH"));
    template.add_track(oh);
    
    // ============================================
    // ROOMS (BUS) - Child of Drums
    // ============================================
    let rooms_bus_name = create_track_name("D", Some("Rooms"), Some("BUS"), None, None);
    let mut rooms_bus = Track::new(&rooms_bus_name);
    rooms_bus.set_parent(&drums_bus_name);
    template.add_track(rooms_bus);
    
    // Rooms
    let rooms_name = create_track_name("D", Some("Rooms"), None, None, None);
    let mut rooms = Track::new(&rooms_name);
    rooms.set_parent(&rooms_bus_name);
    rooms.add_take(Take::new("Rooms"));
    template.add_track(rooms);
    
    // ============================================
    // PERCUSSION (BUS) - Child of Drums
    // ============================================
    let percussion_bus_name = create_track_name("D", Some("Percussion"), Some("BUS"), None, None);
    let mut percussion_bus = Track::new(&percussion_bus_name);
    percussion_bus.set_parent(&drums_bus_name);
    template.add_track(percussion_bus);
    
    // Percussion
    let percussion_name = create_track_name("D", Some("Percussion"), None, None, None);
    let mut percussion = Track::new(&percussion_name);
    percussion.set_parent(&percussion_bus_name);
    percussion.add_take(Take::new("Percussion"));
    template.add_track(percussion);
    
    // ============================================
    // BASS (top level, no BUS type)
    // ============================================
    // Prefix is "Bass", not "B"
    let bass_name = create_track_name("Bass", Some("Bass"), None, None, None);
    let bass = Track::new(&bass_name);
    template.add_track(bass);
    
    // ============================================
    // Bass Guitar (BUS) - Child of Bass
    // ============================================
    // Use "Guitar" as sub_type - the hierarchy extraction will handle it
    // The parent is "Bass", so "Guitar" will be shown as "Bass Guitar" in hierarchy
    let bass_guitar_bus_name = create_track_name("Bass", Some("Guitar"), Some("BUS"), None, None);
    let mut bass_guitar_bus = Track::new(&bass_guitar_bus_name);
    bass_guitar_bus.set_parent(&bass_name);
    template.add_track(bass_guitar_bus);
    
    // Bass DI - Child of Bass Guitar (BUS)
    // Just "DI" as multi_mic - the hierarchy extraction will remove "Bass Guitar" to show just "DI"
    let bass_di_name = create_track_name("Bass", None, None, Some("DI"), None);
    let mut bass_di = Track::new(&bass_di_name);
    bass_di.set_parent(&bass_guitar_bus_name);
    bass_di.add_take(Take::new("Bass DI"));
    template.add_track(bass_di);
    
    // ============================================
    // KEYS (BUS)
    // ============================================
    let keys_bus_name = create_track_name("K", Some("Keys"), Some("BUS"), None, None);
    let mut keys_bus = Track::new(&keys_bus_name);
    template.add_track(keys_bus);
    
    // Piano
    let piano_name = create_track_name("K", Some("Piano"), None, None, None);
    let mut piano = Track::new(&piano_name);
    piano.set_parent(&keys_bus_name);
    piano.add_take(Take::new("Piano"));
    template.add_track(piano);
    
    // ============================================
    // GTR E (BUS) - Electric Guitar
    // ============================================
    let gtr_e_bus_name = create_track_name("G", Some("GTR E"), Some("BUS"), None, None);
    let mut gtr_e_bus = Track::new(&gtr_e_bus_name);
    template.add_track(gtr_e_bus);
    
    // Lead (SUM) - Child of GTR E (BUS)
    let lead_sum_name = create_track_name("G", Some("GTR E Lead"), Some("SUM"), None, None);
    let mut lead_sum = Track::new(&lead_sum_name);
    lead_sum.set_parent(&gtr_e_bus_name);
    template.add_track(lead_sum);
    
    // Lead L (Left)
    let lead_l_name = create_track_name("G", Some("GTR E Lead"), None, Some("L"), None);
    let mut lead_l = Track::new(&lead_l_name);
    lead_l.set_parent(&lead_sum_name);
    lead_l.add_take(Take::new("Lead Guitar Amplitube Left"));
    template.add_track(lead_l);
    
    // Lead R (Right)
    let lead_r_name = create_track_name("G", Some("GTR E Lead"), None, Some("R"), None);
    let mut lead_r = Track::new(&lead_r_name);
    lead_r.set_parent(&lead_sum_name);
    lead_r.add_take(Take::new("Lead Guitar Amplitube Right"));
    template.add_track(lead_r);
    
    // Clean (SUM) - Child of GTR E (BUS)
    let clean_sum_name = create_track_name("G", Some("GTR E Clean"), Some("SUM"), None, None);
    let mut clean_sum = Track::new(&clean_sum_name);
    clean_sum.set_parent(&gtr_e_bus_name);
    template.add_track(clean_sum);
    
    // Clean DI L (DI Left)
    let clean_di_l_name = create_track_name("G", Some("GTR E Clean"), None, Some("DI L"), None);
    let mut clean_di_l = Track::new(&clean_di_l_name);
    clean_di_l.set_parent(&clean_sum_name);
    clean_di_l.add_take(Take::new("Lead Guitar Clean DI Left"));
    template.add_track(clean_di_l);
    
    // Clean DI R (DI Right)
    let clean_di_r_name = create_track_name("G", Some("GTR E Clean"), None, Some("DI R"), None);
    let mut clean_di_r = Track::new(&clean_di_r_name);
    clean_di_r.set_parent(&clean_sum_name);
    clean_di_r.add_take(Take::new("Lead Guitar Clean DI Right"));
    template.add_track(clean_di_r);
    
    // ============================================
    // VOCALS (BUS)
    // ============================================
    let vocals_bus_name = create_track_name("V", Some("Vocals"), Some("BUS"), None, None);
    let mut vocals_bus = Track::new(&vocals_bus_name);
    template.add_track(vocals_bus);
    
    // Lead Vocal (SUM) - Child of Vocals (BUS)
    let lead_vocal_sum_name = create_track_name("V", Some("Lead Vocal"), Some("SUM"), None, None);
    let mut lead_vocal_sum = Track::new(&lead_vocal_sum_name);
    lead_vocal_sum.set_parent(&vocals_bus_name);
    template.add_track(lead_vocal_sum);
    
    // Vocal
    let vocal_name = create_track_name("V", Some("Vocal"), None, None, None);
    let mut vocal = Track::new(&vocal_name);
    vocal.set_parent(&lead_vocal_sum_name);
    vocal.add_take(Take::new("Vocal"));
    template.add_track(vocal);
    
    // Background Vocals (SUM) - Child of Vocals (BUS)
    let bgv_sum_name = create_track_name("V", Some("Background Vocals"), Some("SUM"), None, None);
    let mut bgv_sum = Track::new(&bgv_sum_name);
    bgv_sum.set_parent(&vocals_bus_name);
    template.add_track(bgv_sum);
    
    // BGV1-4
    for i in 1..=4 {
        let bgv_name = create_track_name("V", Some("BGV"), None, None, Some(i));
        let mut bgv = Track::new(&bgv_name);
        bgv.set_parent(&bgv_sum_name);
        bgv.add_take(Take::new(&format!("BGV{}", i)));
        template.add_track(bgv);
    }
    
    // VocalFX (BUS) - Child of Vocals (BUS)
    // For vocal effects like Hall, Verb, Delay, Plate, etc.
    let vocal_fx_bus_name = create_track_name("VF", Some("VocalFX"), Some("BUS"), None, None);
    let mut vocal_fx_bus = Track::new(&vocal_fx_bus_name);
    vocal_fx_bus.set_parent(&vocals_bus_name);
    template.add_track(vocal_fx_bus);
    
    // Vocal.Eko.Plate
    let vocal_eko_plate_name = create_track_name("VF", Some("Vocal.Eko.Plate"), None, None, None);
    let mut vocal_eko_plate = Track::new(&vocal_eko_plate_name);
    vocal_eko_plate.set_parent(&vocal_fx_bus_name);
    vocal_eko_plate.add_take(Take::new("Vocal.Eko.Plate"));
    template.add_track(vocal_eko_plate);
    
    // Vocal.Magic
    let vocal_magic_name = create_track_name("VF", Some("Vocal.Magic"), None, None, None);
    let mut vocal_magic = Track::new(&vocal_magic_name);
    vocal_magic.set_parent(&vocal_fx_bus_name);
    vocal_magic.add_take(Take::new("Vocal.Magic"));
    template.add_track(vocal_magic);
    
    // ============================================
    // UNSORTED (BUS) - Tracks we can't categorize
    // ============================================
    let unsorted_bus_name = create_track_name("U", Some("Unsorted"), Some("BUS"), None, None);
    let unsorted_bus = Track::new(&unsorted_bus_name);
    template.add_track(unsorted_bus);
    
    // H3000.One - Unsorted (can't determine what it is from name alone)
    let h3000_one_name = create_track_name("U", Some("H3000"), None, Some("One"), None);
    let mut h3000_one = Track::new(&h3000_one_name);
    h3000_one.set_parent(&unsorted_bus_name);
    h3000_one.set_sort_status(SortStatus::NotSorted);
    h3000_one.add_take(Take::new("H3000.One"));
    template.add_track(h3000_one);
    
    // H3000.Two - Unsorted
    let h3000_two_name = create_track_name("U", Some("H3000"), None, Some("Two"), None);
    let mut h3000_two = Track::new(&h3000_two_name);
    h3000_two.set_parent(&unsorted_bus_name);
    h3000_two.set_sort_status(SortStatus::NotSorted);
    h3000_two.add_take(Take::new("H3000.Two"));
    template.add_track(h3000_two);
    
    // H3000.Three - Unsorted
    let h3000_three_name = create_track_name("U", Some("H3000"), None, Some("Three"), None);
    let mut h3000_three = Track::new(&h3000_three_name);
    h3000_three.set_parent(&unsorted_bus_name);
    h3000_three.set_sort_status(SortStatus::NotSorted);
    h3000_three.add_take(Take::new("H3000.Three"));
    template.add_track(h3000_three);
    
    // ============================================
    // Display the complete template
    // ============================================
    println!("\n=== Expected Track Structure (Full Name Mode) ===");
    println!("{}", template.track_list());
    
    // Switch to Hierarchy mode
    template.track_list_mut().set_display_mode(DisplayMode::Hierarchy);
    println!("\n=== Expected Track Structure (Hierarchy Mode) ===");
    println!("{}", template.track_list());
    
    assert!(!track_names.is_empty(), "Should have track names to test");
}

#[test]
fn test_bass_hierarchy_debug() {
    use track_template::{Track, Template, Take, DisplayMode};
    use naming_convention::{SimpleParser, create_default_groups, format_track_name_default, TrackName};
    
    // Helper function to create a TrackName
    fn create_track_name(
        group_prefix: &str,
        sub_type: Option<&str>,
        track_type: Option<&str>,
        multi_mic: Option<&str>,
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
        if let Some(mm) = multi_mic {
            track_name.multi_mic = Some(vec![mm.to_string()]);
        }
        if let Some(inc) = increment {
            track_name.increment = Some(inc.to_string());
        }
        format_track_name_default(&track_name)
    }
    
    let parser = SimpleParser::new(create_default_groups());
    let mut template = Template::new("Bass Hierarchy Debug");
    
    // Top-level Bass track
    let bass_name = create_track_name("Bass", Some("Bass"), None, None, None);
    println!("\n=== Top-level Bass track ===");
    println!("Created name: {}", bass_name);
    let parsed = parser.parse(&bass_name);
    println!("Parsed: group_prefix={:?}, sub_type={:?}, arrangement={:?}", 
             parsed.group_prefix, parsed.sub_type, parsed.arrangement);
    let bass = Track::new(&bass_name);
    template.add_track(bass);
    
    // Bass Guitar (BUS) - Child of Bass
    let bass_guitar_bus_name = create_track_name("Bass", Some("Guitar"), Some("BUS"), None, None);
    println!("\n=== Bass Guitar (BUS) track ===");
    println!("Created name: {}", bass_guitar_bus_name);
    let parsed_guitar = parser.parse(&bass_guitar_bus_name);
    println!("Parsed: group_prefix={:?}, sub_type={:?}, arrangement={:?}", 
             parsed_guitar.group_prefix, parsed_guitar.sub_type, parsed_guitar.arrangement);
    let mut bass_guitar_bus = Track::new(&bass_guitar_bus_name);
    bass_guitar_bus.set_parent(&bass_name);
    template.add_track(bass_guitar_bus);
    
    // Bass DI - Child of Bass Guitar (BUS)
    let bass_di_name = create_track_name("Bass", None, None, Some("DI"), None);
    println!("\n=== Bass DI track ===");
    println!("Created name: {}", bass_di_name);
    let parsed_di = parser.parse(&bass_di_name);
    println!("Parsed: group_prefix={:?}, sub_type={:?}, arrangement={:?}, multi_mic={:?}", 
             parsed_di.group_prefix, parsed_di.sub_type, parsed_di.arrangement, parsed_di.multi_mic);
    let mut bass_di = Track::new(&bass_di_name);
    bass_di.set_parent(&bass_guitar_bus_name);
    bass_di.add_take(Take::new("Bass DI"));
    template.add_track(bass_di);
    
    // Test what parser returns for Drums
    println!("\n=== Testing Drums parsing ===");
    let drums_name = create_track_name("D", Some("Drums"), Some("BUS"), None, None);
    let drums_parsed = parser.parse(&drums_name);
    println!("Drums track name: {}", drums_name);
    println!("Parsed: group_prefix={:?}, sub_type={:?}", drums_parsed.group_prefix, drums_parsed.sub_type);
    
    // Display in Hierarchy mode
    template.track_list_mut().set_display_mode(DisplayMode::Hierarchy);
    println!("\n=== Hierarchy Mode Output ===");
    println!("{}", template.track_list());
}

