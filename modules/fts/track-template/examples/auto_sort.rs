//! Auto-sort example
//!
//! This example demonstrates automatically sorting a messy list of track names
//! into a comprehensive template structure. It includes:
//! - Various instrument groups (Drums, Bass, Guitars, Keys, Synths, Vocals, BGVs, Orchestra)
//! - Performer information
//! - Arrangement descriptors
//! - Multi-mic positions
//! - Playlist variations
//! - Mixed naming formats

use track_template::{Track, Template};
use track_template::matcher::MatchType;
use naming_convention::format_track_name_default;

fn main() {
    println!("=== Auto-Sort Example ===\n");
    println!("This example demonstrates matching messy track names to a comprehensive template.\n");
    
    // ============================================
    // Step 1: Create a comprehensive template
    // ============================================
    println!("Step 1: Creating comprehensive template...");
    let template = create_comprehensive_template();
    println!("Template created with {} tracks\n", template.track_list().len());
    
    // ============================================
    // Step 2: Create a messy list of track names
    // ============================================
    let messy_track_names = vec![
        // Drums - various formats
        "D Kick In",
        "D Kick Out",
        "D Kick Trig",
        "D Kick Sub",
        "D Snare Top",
        "D Snare Bottom",
        "D Snare Verb",
        "D Snare Fund",
        "D Tom 1",
        "D Tom 2",
        "D Tom 3",
        "D OH",
        "D HiHat",
        "D Ride",
        "D Rooms",
        "D Rooms Far",
        "D Rooms Mono",
        
        // Bass
        "B Bass Amp",
        "B Bass DI",
        "B Bass Synth",
        
        // Electric Guitar - with performers and arrangements
        "G GTR E John Rhythm L Amp",
        "G GTR E John Rhythm L DI",
        "G GTR E John Rhythm L NO-FX",
        "G GTR E Jane Lead R Amp",
        "G GTR E Jane Lead R DI",
        "G GTR E Mike Chug DBL Amp",
        "G GTR E Mike Chug DBL DI",
        
        // Acoustic Guitar
        "G GTR AG Sarah L Neck",
        "G GTR AG Sarah L Bridge",
        "G GTR AG Sarah L DI",
        "G GTR AG Tom R Bridge",
        
        // Keys
        "K Piano",
        "K Electric",
        "K Electric Amp",
        "K Electric DI",
        
        // Synths
        "SY Synths Chords",
        "SY Synths Pads",
        "SY Synths Arps",
        "SY Synths Leads",
        
        // Vocals - with performers and languages
        "V V LEAD John English",
        "V V LEAD John Spanish",
        "V V LEAD John Portuguese",
        "V V LEAD DBL Jane English",
        "V V LEAD DBL Jane Spanish",
        
        // BGVs
        "BGV Soprano",
        "BGV Alto",
        "BGV Tenor",
        "BGV Baritone",
        
        // Orchestra
        "O Woodwinds Flute",
        "O Woodwinds Oboe",
        "O Brass Trumpets",
        "O Brass Horns",
        "O Strings Violin 1",
        "O Strings Violin 2",
        "O Strings Viola",
        "O Strings Cello",
        "O Harp",
        "O Orch Perc Timpani",
        
        // Playlist variations (should match to same tracks but use takes)
        "D Kick In .1",
        "D Kick In .2",
        "D Snare Top .1",
        "D Snare Top .2",
        "G GTR E John Rhythm L Amp .1",
        "G GTR E John Rhythm L Amp .2",
        
        // Some messy/inconsistent formats
        "D Kick In Close",  // Extra word
        "D Snare Top Mic",  // Extra word
        "GTR E John Rhythm", // Missing some components
        "Bass Amp",  // Missing prefix
        "Kick In",  // Missing prefix
    ];
    
    println!("Step 2: Processing {} messy track names...\n", messy_track_names.len());
    
    // ============================================
    // Step 3: Match each name to the template
    // ============================================
    let matcher = template.create_matcher();
    
    let mut matched_count = 0;
    let mut unmatched_count = 0;
    let mut playlist_variations = 0;
    let mut use_takes_count = 0;
    
    println!("Step 3: Matching track names to template...\n");
    println!("{:-<80}", "");
    println!("{:<50} | {:<20} | {:<5} | Takes", "Track Name", "Matched To", "Score");
    println!("{:-<80}", "");
    
    for track_name_str in &messy_track_names {
        let parsed = matcher.parse(track_name_str);
        
        if let Some(result) = matcher.find_best_match(&parsed) {
            matched_count += 1;
            if result.use_takes {
                use_takes_count += 1;
                if result.match_type == MatchType::PlaylistVariation {
                    playlist_variations += 1;
                }
            }
            
            let match_type_str = match result.match_type {
                MatchType::Exact => "Exact",
                MatchType::SubType => "SubType",
                MatchType::PlaylistVariation => "Playlist",
                MatchType::Partial => "Partial",
            };
            
            let takes_str = if result.use_takes { "Yes" } else { "No" };
            
            println!(
                "{:<50} | {:<20} | {:<5} | {} ({})",
                track_name_str,
                result.track.name,
                result.score,
                takes_str,
                match_type_str
            );
        } else {
            unmatched_count += 1;
            println!(
                "{:<50} | {:<20} | {:<5} | {}",
                track_name_str,
                "NO MATCH",
                "0",
                "No"
            );
        }
    }
    
    println!("{:-<80}", "");
    
    // ============================================
    // Step 4: Summary
    // ============================================
    println!("\n=== Summary ===");
    println!("Total track names processed: {}", messy_track_names.len());
    println!("Matched: {} ({:.1}%)", matched_count, 
        (matched_count as f64 / messy_track_names.len() as f64) * 100.0);
    println!("Unmatched: {} ({:.1}%)", unmatched_count,
        (unmatched_count as f64 / messy_track_names.len() as f64) * 100.0);
    println!("Playlist variations detected: {}", playlist_variations);
    println!("Tracks that should use takes (item lanes): {}", use_takes_count);
    
    // Show parsed examples
    println!("\n=== Parsed Examples ===");
    let example_names = vec![
        "G GTR E John Rhythm L Amp",
        "V V LEAD John English",
        "D Kick In .1",
    ];
    
    for name in &example_names {
        let parsed = matcher.parse(name);
        println!("\nParsed '{}':", name);
        println!("  Group Prefix: {:?}", parsed.group_prefix);
        println!("  Sub Type: {:?}", parsed.sub_type);
        println!("  Performer: {:?}", parsed.performer);
        println!("  Arrangement: {:?}", parsed.arrangement);
        println!("  Channel: {:?}", parsed.channel);
        println!("  Multi-Mic: {:?}", parsed.multi_mic);
        println!("  Playlist: {:?}", parsed.playlist);
        println!("  Formatted: {}", format_track_name_default(&parsed));
    }
    
    // ============================================
    // Step 5: Display the complete template structure
    // ============================================
    println!("\n=== Complete Template Structure ===");
    println!("{}", template);
}

/// Create a comprehensive template with all instrument groups
fn create_comprehensive_template() -> Template {
    use naming_convention::{TrackName, SimpleParser, create_default_groups, format_track_name_default};
    
    let parser = SimpleParser::new(create_default_groups());
    let mut template = Template::new("Comprehensive Template");
    
    // Helper to create a track name and format it
    fn create_track_name(
        group_prefix: &str,
        sub_type: Option<&str>,
        track_type: Option<&str>,
        performer: Option<&str>,
        arrangement: Option<&str>,
        channel: Option<&str>,
        multi_mic: Option<&str>,
        increment: Option<&str>,
        section: Option<&str>,
        layers: Option<&str>,
    ) -> String {
        let mut track_name = TrackName::new();
        track_name.group_prefix = Some(group_prefix.to_string());
        if let Some(st) = sub_type {
            track_name.sub_type = Some(vec![st.to_string()]);
        }
        if let Some(tt) = track_type {
            track_name.track_type = Some(tt.to_string());
        }
        if let Some(p) = performer {
            track_name.performer = Some(p.to_string());
        }
        if let Some(a) = arrangement {
            track_name.arrangement = Some(a.to_string());
        }
        if let Some(c) = channel {
            track_name.channel = Some(c.to_string());
        }
        if let Some(mm) = multi_mic {
            track_name.multi_mic = Some(vec![mm.to_string()]);
        }
        if let Some(inc) = increment {
            track_name.increment = Some(inc.to_string());
        }
        if let Some(sec) = section {
            track_name.section = Some(sec.to_string());
        }
        if let Some(l) = layers {
            track_name.layers = Some(l.to_string());
        }
        format_track_name_default(&track_name)
    }
    
    // Helper to create and add tracks using formatted names
    fn add_track(template: &mut Template, formatted_name: &str, parent: Option<&str>) {
        let mut track = Track::new(formatted_name);
        
        if let Some(p) = parent {
            track.set_parent(p);
        }
        
        template.add_track(track);
    }
    
    // ============================================
    // DRUMS
    // ============================================
    let drums_bus = create_track_name("D", Some("Drums"), Some("BUS"), None, None, None, None, None, None, None);
    add_track(&mut template, &drums_bus, None);
    
    // Kick
    let kick_bus = create_track_name("D", Some("Kick"), Some("BUS"), None, None, None, None, None, None, None);
    add_track(&mut template, &kick_bus, Some(&drums_bus));
    let kick_sum = create_track_name("D", Some("Kick"), Some("SUM"), None, None, None, None, None, None, None);
    add_track(&mut template, &kick_sum, Some(&kick_bus));
    add_track(&mut template, &create_track_name("D", Some("Kick"), None, None, None, None, Some("In"), None, None, None), Some(&kick_sum));
    add_track(&mut template, &create_track_name("D", Some("Kick"), None, None, None, None, Some("Out"), None, None, None), Some(&kick_sum));
    add_track(&mut template, &create_track_name("D", Some("Kick"), None, None, None, None, Some("Trig"), None, None, None), Some(&kick_sum));
    add_track(&mut template, &create_track_name("D", Some("Kick"), None, None, None, None, Some("Sub"), None, None, None), Some(&kick_bus));
    
    // Snare
    let snare_bus = create_track_name("D", Some("Snare"), Some("BUS"), None, None, None, None, None, None, None);
    add_track(&mut template, &snare_bus, Some(&drums_bus));
    let snare_sum = create_track_name("D", Some("Snare"), Some("SUM"), None, None, None, None, None, None, None);
    add_track(&mut template, &snare_sum, Some(&snare_bus));
    add_track(&mut template, &create_track_name("D", Some("Snare"), None, None, None, None, Some("Top"), None, None, None), Some(&snare_sum));
    add_track(&mut template, &create_track_name("D", Some("Snare"), None, None, None, None, Some("Bottom"), None, None, None), Some(&snare_sum));
    add_track(&mut template, &create_track_name("D", Some("Snare"), None, None, None, None, Some("Verb"), None, None, None), Some(&snare_bus));
    add_track(&mut template, &create_track_name("D", Some("Snare"), None, None, None, None, Some("Fund"), None, None, None), Some(&snare_bus));
    
    // Toms
    let tom_bus = create_track_name("D", Some("Tom"), Some("BUS"), None, None, None, None, None, None, None);
    add_track(&mut template, &tom_bus, Some(&drums_bus));
    add_track(&mut template, &create_track_name("D", Some("Tom"), None, None, None, None, None, Some("1"), None, None), Some(&tom_bus));
    add_track(&mut template, &create_track_name("D", Some("Tom"), None, None, None, None, None, Some("2"), None, None), Some(&tom_bus));
    add_track(&mut template, &create_track_name("D", Some("Tom"), None, None, None, None, None, Some("3"), None, None), Some(&tom_bus));
    
    // Cymbals
    let cymbals_bus = create_track_name("D", Some("Cymbals"), Some("BUS"), None, None, None, None, None, None, None);
    add_track(&mut template, &cymbals_bus, Some(&drums_bus));
    add_track(&mut template, &create_track_name("D", Some("OH"), None, None, None, None, None, None, None, None), Some(&cymbals_bus));
    add_track(&mut template, &create_track_name("D", Some("HiHat"), None, None, None, None, None, None, None, None), Some(&cymbals_bus));
    add_track(&mut template, &create_track_name("D", Some("Ride"), None, None, None, None, None, None, None, None), Some(&cymbals_bus));
    
    // Rooms
    let rooms_bus = create_track_name("D", Some("Rooms"), Some("BUS"), None, None, None, None, None, None, None);
    add_track(&mut template, &rooms_bus, Some(&drums_bus));
    add_track(&mut template, &create_track_name("D", Some("Rooms"), None, None, None, None, None, None, None, None), Some(&rooms_bus));
    add_track(&mut template, &create_track_name("D", Some("Rooms"), None, None, None, None, Some("Far"), None, None, None), Some(&rooms_bus));
    add_track(&mut template, &create_track_name("D", Some("Rooms"), None, None, None, None, Some("Mono"), None, None, None), Some(&rooms_bus));
    
    // ============================================
    // BASS
    // ============================================
    let bass_bus = create_track_name("B", Some("Bass"), Some("BUS"), None, None, None, None, None, None, None);
    add_track(&mut template, &bass_bus, None);
    let bass_gtr_bus = create_track_name("B", Some("Bass GTR"), Some("BUS"), None, None, None, None, None, None, None);
    add_track(&mut template, &bass_gtr_bus, Some(&bass_bus));
    let bass_amp = create_track_name("B", Some("Bass"), None, None, None, None, Some("Amp"), None, None, None);
    add_track(&mut template, &bass_amp, Some(&bass_gtr_bus));
    let bass_di = create_track_name("B", Some("Bass"), None, None, None, None, Some("DI"), None, None, None);
    add_track(&mut template, &bass_di, Some(&bass_gtr_bus));
    let bass_synth = create_track_name("B", Some("Bass Synth"), None, None, None, None, None, None, None, None);
    add_track(&mut template, &bass_synth, Some(&bass_bus));
    
    // ============================================
    // ELECTRIC GUITAR
    // ============================================
    let gtr_e_bus = create_track_name("G", Some("GTR E"), Some("BUS"), None, None, None, None, None, None, None);
    add_track(&mut template, &gtr_e_bus, None);
    
    // John's tracks - parse the actual input to get the formatted name
    let john_amp = parser.parse("G GTR E John Rhythm L Amp");
    let john_amp_name = format_track_name_default(&john_amp);
    add_track(&mut template, &john_amp_name, Some(&gtr_e_bus));
    let john_di = parser.parse("G GTR E John Rhythm L DI");
    add_track(&mut template, &format_track_name_default(&john_di), Some(&john_amp_name));
    let john_no_fx = parser.parse("G GTR E John Rhythm L NO-FX");
    add_track(&mut template, &format_track_name_default(&john_no_fx), Some(&john_amp_name));
    
    // Jane's tracks
    let jane_amp = parser.parse("G GTR E Jane Lead R Amp");
    let jane_amp_name = format_track_name_default(&jane_amp);
    add_track(&mut template, &jane_amp_name, Some(&gtr_e_bus));
    let jane_di = parser.parse("G GTR E Jane Lead R DI");
    add_track(&mut template, &format_track_name_default(&jane_di), Some(&jane_amp_name));
    
    // Mike's tracks
    let mike_amp = parser.parse("G GTR E Mike Chug DBL Amp");
    let mike_amp_name = format_track_name_default(&mike_amp);
    add_track(&mut template, &mike_amp_name, Some(&gtr_e_bus));
    let mike_di = parser.parse("G GTR E Mike Chug DBL DI");
    add_track(&mut template, &format_track_name_default(&mike_di), Some(&mike_amp_name));
    
    // ============================================
    // ACOUSTIC GUITAR
    // ============================================
    let gtr_ag_bus = create_track_name("G", Some("GTR AG"), Some("BUS"), None, None, None, None, None, None, None);
    add_track(&mut template, &gtr_ag_bus, None);
    let sarah_neck = parser.parse("G GTR AG Sarah L Neck");
    let sarah_neck_name = format_track_name_default(&sarah_neck);
    add_track(&mut template, &sarah_neck_name, Some(&gtr_ag_bus));
    let sarah_bridge = parser.parse("G GTR AG Sarah L Bridge");
    add_track(&mut template, &format_track_name_default(&sarah_bridge), Some(&sarah_neck_name));
    let sarah_di = parser.parse("G GTR AG Sarah L DI");
    add_track(&mut template, &format_track_name_default(&sarah_di), Some(&sarah_neck_name));
    let tom_bridge = parser.parse("G GTR AG Tom R Bridge");
    add_track(&mut template, &format_track_name_default(&tom_bridge), Some(&gtr_ag_bus));
    
    // ============================================
    // KEYS
    // ============================================
    let keys_bus = create_track_name("K", Some("Keys"), Some("BUS"), None, None, None, None, None, None, None);
    add_track(&mut template, &keys_bus, None);
    add_track(&mut template, &create_track_name("K", Some("Piano"), None, None, None, None, None, None, None, None), Some(&keys_bus));
    let electric = create_track_name("K", Some("Electric"), None, None, None, None, None, None, None, None);
    add_track(&mut template, &electric, Some(&keys_bus));
    add_track(&mut template, &create_track_name("K", Some("Electric"), None, None, None, None, Some("Amp"), None, None, None), Some(&electric));
    add_track(&mut template, &create_track_name("K", Some("Electric"), None, None, None, None, Some("DI"), None, None, None), Some(&electric));
    
    // ============================================
    // SYNTHS
    // ============================================
    let synths_bus = create_track_name("SY", Some("Synths"), Some("BUS"), None, None, None, None, None, None, None);
    add_track(&mut template, &synths_bus, None);
    add_track(&mut template, &create_track_name("SY", Some("Synths"), None, None, None, None, None, None, None, Some("Chords")), Some(&synths_bus));
    add_track(&mut template, &create_track_name("SY", Some("Synths"), None, None, None, None, None, None, None, Some("Pads")), Some(&synths_bus));
    add_track(&mut template, &create_track_name("SY", Some("Synths"), None, None, None, None, None, None, None, Some("Arps")), Some(&synths_bus));
    add_track(&mut template, &create_track_name("SY", Some("Synths"), None, None, None, None, None, None, None, Some("Leads")), Some(&synths_bus));
    
    // ============================================
    // VOCALS
    // ============================================
    let vocals_bus = create_track_name("V", Some("Vocals"), Some("BUS"), None, None, None, None, None, None, None);
    add_track(&mut template, &vocals_bus, None);
    let v_lead_john = parser.parse("V V LEAD John");
    let v_lead_john_name = format_track_name_default(&v_lead_john);
    add_track(&mut template, &v_lead_john_name, Some(&vocals_bus));
    let v_lead_john_eng = parser.parse("V V LEAD John English");
    add_track(&mut template, &format_track_name_default(&v_lead_john_eng), Some(&v_lead_john_name));
    let v_lead_john_spa = parser.parse("V V LEAD John Spanish");
    add_track(&mut template, &format_track_name_default(&v_lead_john_spa), Some(&v_lead_john_name));
    let v_lead_john_por = parser.parse("V V LEAD John Portuguese");
    add_track(&mut template, &format_track_name_default(&v_lead_john_por), Some(&v_lead_john_name));
    let v_lead_dbl_jane = parser.parse("V V LEAD DBL Jane");
    let v_lead_dbl_jane_name = format_track_name_default(&v_lead_dbl_jane);
    add_track(&mut template, &v_lead_dbl_jane_name, Some(&vocals_bus));
    let v_lead_dbl_jane_eng = parser.parse("V V LEAD DBL Jane English");
    add_track(&mut template, &format_track_name_default(&v_lead_dbl_jane_eng), Some(&v_lead_dbl_jane_name));
    let v_lead_dbl_jane_spa = parser.parse("V V LEAD DBL Jane Spanish");
    add_track(&mut template, &format_track_name_default(&v_lead_dbl_jane_spa), Some(&v_lead_dbl_jane_name));
    
    // ============================================
    // BGVs
    // ============================================
    add_track(&mut template, &create_track_name("BGV", Some("Soprano"), None, None, None, None, None, None, None, None), None);
    add_track(&mut template, &create_track_name("BGV", Some("Alto"), None, None, None, None, None, None, None, None), None);
    add_track(&mut template, &create_track_name("BGV", Some("Tenor"), None, None, None, None, None, None, None, None), None);
    add_track(&mut template, &create_track_name("BGV", Some("Baritone"), None, None, None, None, None, None, None, None), None);
    
    // ============================================
    // ORCHESTRA
    // ============================================
    let orchestra_bus = create_track_name("O", Some("Orchestra"), Some("BUS"), None, None, None, None, None, None, None);
    add_track(&mut template, &orchestra_bus, None);
    let woodwinds_bus = create_track_name("O", Some("Woodwinds"), Some("BUS"), None, None, None, None, None, None, None);
    add_track(&mut template, &woodwinds_bus, Some(&orchestra_bus));
    add_track(&mut template, &create_track_name("O", Some("Woodwinds"), None, None, None, None, None, None, Some("Flute"), None), Some(&woodwinds_bus));
    add_track(&mut template, &create_track_name("O", Some("Woodwinds"), None, None, None, None, None, None, Some("Oboe"), None), Some(&woodwinds_bus));
    let brass_bus = create_track_name("O", Some("Brass"), Some("BUS"), None, None, None, None, None, None, None);
    add_track(&mut template, &brass_bus, Some(&orchestra_bus));
    add_track(&mut template, &create_track_name("O", Some("Brass"), None, None, None, None, None, None, Some("Trumpets"), None), Some(&brass_bus));
    add_track(&mut template, &create_track_name("O", Some("Brass"), None, None, None, None, None, None, Some("Horns"), None), Some(&brass_bus));
    let strings_bus = create_track_name("O", Some("Strings"), Some("BUS"), None, None, None, None, None, None, None);
    add_track(&mut template, &strings_bus, Some(&orchestra_bus));
    add_track(&mut template, &create_track_name("O", Some("Strings"), None, None, None, None, None, Some("1"), Some("Violin"), None), Some(&strings_bus));
    add_track(&mut template, &create_track_name("O", Some("Strings"), None, None, None, None, None, Some("2"), Some("Violin"), None), Some(&strings_bus));
    add_track(&mut template, &create_track_name("O", Some("Strings"), None, None, None, None, None, None, Some("Viola"), None), Some(&strings_bus));
    add_track(&mut template, &create_track_name("O", Some("Strings"), None, None, None, None, None, None, Some("Cello"), None), Some(&strings_bus));
    let harp_bus = create_track_name("O", Some("Harp"), Some("BUS"), None, None, None, None, None, None, None);
    add_track(&mut template, &harp_bus, Some(&orchestra_bus));
    add_track(&mut template, &create_track_name("O", Some("Harp"), None, None, None, None, None, None, None, None), Some(&harp_bus));
    let orch_perc_bus = create_track_name("O", Some("Orch Perc"), Some("BUS"), None, None, None, None, None, None, None);
    add_track(&mut template, &orch_perc_bus, Some(&orchestra_bus));
    add_track(&mut template, &create_track_name("O", Some("Orch Perc"), None, None, None, None, None, None, Some("Timpani"), None), Some(&orch_perc_bus));
    
    template
}

