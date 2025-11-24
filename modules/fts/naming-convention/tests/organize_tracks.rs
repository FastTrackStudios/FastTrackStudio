//! Integration test for organizing track names into a structured hierarchy
//!
//! This test demonstrates how to:
//! 1. Parse a list of track name strings
//! 2. Match them against default group configurations
//! 3. Organize them into a track structure hierarchy with proper nesting

use std::collections::HashMap;
use naming_convention::{
    TrackName, TrackStructure, SimpleParser,
    create_default_groups,
    format_track_name_default,
};

#[test]
fn test_organize_tracks_dynamically() {
    // Example track names from a typical recording session
    let track_names = vec![
        // Drums
        "D KICK (BUS)",
        "D KICK (SUM)",
        "Kick In",
        "Kick Out",
        "Kick Trig",
        "Kick Sub",
        "Kick Ambient",
        "D SNARE (BUS)",
        "D SNARE (SUM)",
        "Snare Top",
        "Snare Bottom",
        "Snare Trig",
        "Snare Verb",
        "Tom 1",
        "Tom 2",
        "Tom 3",
        "D CYMBALS (BUS)",
        "Hi-Hat",
        "Ride",
        "OH",
        "D ROOM (BUS)",
        "Rooms",
        "Rooms Far",
        "Rooms Mono",
        
        // Bass
        "B (BUS)",
        "Bass Guitar (BUS)",
        "Bass Guitar DI",
        "Bass Guitar Amp",
        "Synth Bass",
        
        // Guitar Electric
        "GTR ELEC",
        "GTR Clean",
        "GTR Crunch",
        "GTR Lead",
        "GTR DI",
        "GTR Amp",
        "GTR Cab",
        
        // Guitar Acoustic
        "GTR AG",
        "GTR AG Fingerstyle",
        "GTR AG Strum",
        "GTR AG Mic L",
        "GTR AG Mic R",
        "GTR AG DI",
        
        // Keys
        "Keys",
        "Piano",
        "Piano Classical",
        "Piano Jazz",
        "EP Clean",
        "EP Chorus",
        "Organ Drawbar",
        "Organ Gospel",
        
        // Synths
        "SYNTHS",
        "SY Arp",
        "SY Pad",
        "SY Lead",
        "SY Sequence",
        
        // Vocals
        "Vocals",
        "V Lead",
        "V Verse",
        "V Chorus",
        "V DBL",
        "V BGVs",
        "V BGVs Harmony",
        "V BGVs Unison",
        "Soprano",
        "Alto",
        "Tenor",
    ];
    
    // Get default groups for matching
    let groups = create_default_groups();
    
    // Create parser with default groups
    let parser = SimpleParser::new(groups);
    
    // Parse all track names
    let parsed_tracks: Vec<(String, TrackName)> = track_names.iter()
        .map(|name| {
            let track_name = parser.parse(name);
            (name.to_string(), track_name)
        })
        .collect();
    
    // Build track structure dynamically from parsed tracks
    let structure = build_track_structure_from_parsed(&parsed_tracks);
    
    // Verify structure was created
    assert_eq!(structure.name, "Project");
    assert!(structure.has_children());
    
    // Print for debugging
    println!("\n\n{}", "=".repeat(70));
    println!();
    println!("  {}", structure.name.to_uppercase());
    println!();
    println!("{}\n", "=".repeat(70));
    print_structure(&structure, 0);
}

/// Build track structure dynamically from parsed track names
/// This allows auto-incrementing and multiple tracks with the same name
fn build_track_structure_from_parsed(parsed_tracks: &[(String, TrackName)]) -> TrackStructure {
    let mut root = TrackStructure::new("Project");
    
    // Group tracks by their group prefix (e.g., "D", "B", "GTR", etc.)
    let mut by_group: HashMap<String, Vec<&(String, TrackName)>> = HashMap::new();
    
    for track in parsed_tracks {
        let group_key = track.1.group_prefix.as_ref()
            .map(|p| p.split_whitespace().next().unwrap_or("").to_string())
            .unwrap_or_else(|| "Unknown".to_string());
        by_group.entry(group_key).or_insert_with(Vec::new).push(track);
    }
    
    // Build structure for each group with proper nesting
    for (group_prefix, tracks) in by_group {
        let group_name = match group_prefix.as_str() {
            "D" => "Drums",
            "R" => "Room", // Rooms group prefix
            "B" => "Bass",
            "GTR" | "G" => "Guitar",
            "K" => "Keys",
            "SY" => "Synths",
            "V" => "Vocals",
            _ => &group_prefix,
        };
        
        // Build nested structure based on track components
        // For groups without sub-types (like Rooms), build directly into the group
        // For groups with sub-types (like Drums -> Kick), create a group node first
        let has_subtypes = tracks.iter().any(|t| t.1.sub_type.is_some() && !t.1.sub_type.as_ref().unwrap().is_empty());
        
        if has_subtypes {
            let mut group = TrackStructure::new(group_name);
            build_nested_group_structure(&mut group, tracks);
            root.add_child(group);
        } else {
            // No sub-types - build structure directly (e.g., Rooms group)
            build_nested_group_structure(&mut root, tracks);
        }
    }
    
    root
}

/// Build nested structure for a group
fn build_nested_group_structure(parent: &mut TrackStructure, tracks: Vec<&(String, TrackName)>) {
    use std::collections::HashMap;
    
    // Group tracks by sub-type first (e.g., "Kick", "Snare", "Tom")
    // If there's no sub-type, the tracks belong directly to the group itself
    let mut by_subtype: HashMap<String, Vec<&(String, TrackName)>> = HashMap::new();
    let mut no_subtype_tracks: Vec<&(String, TrackName)> = Vec::new();
    
    for track in tracks {
        if let Some(ref sub_types) = track.1.sub_type {
            if !sub_types.is_empty() {
                let subtype_key = sub_types.last().unwrap().clone();
                by_subtype.entry(subtype_key).or_insert_with(Vec::new).push(track);
            } else {
                no_subtype_tracks.push(track);
            }
        } else {
            no_subtype_tracks.push(track);
        }
    }
    
    // If we have tracks without sub-types, they belong directly to the group
    // (e.g., "Rooms", "Rooms Far" belong directly to Room group, not a sub-type)
    if !no_subtype_tracks.is_empty() && by_subtype.is_empty() {
        // All tracks are at the group level - build structure directly
        let group_name = parent.name.clone();
        let mut group_node = TrackStructure::new(&group_name);
        
        // Group by track type
        let mut tracks_with_type: Vec<&(String, TrackName)> = Vec::new();
        let mut tracks_without_type: Vec<&(String, TrackName)> = Vec::new();
        
        for track in &no_subtype_tracks {
            if track.1.track_type.is_some() {
                tracks_with_type.push(track);
            } else {
                tracks_without_type.push(track);
            }
        }
        
        // Handle BUS tracks
        let mut by_track_type: HashMap<String, Vec<&(String, TrackName)>> = HashMap::new();
        for track in tracks_with_type {
            if let Some(ref tt) = track.1.track_type {
                by_track_type.entry(tt.clone()).or_insert_with(Vec::new).push(track);
            }
        }
        
        if let Some(bus_tracks) = by_track_type.remove("BUS") {
            let mut bus_node = TrackStructure::with_track_type(&group_name, "BUS".to_string());
            
            // Add tracks without types directly to BUS
            for track in tracks_without_type {
                let track_name = format_track_name_default(&track.1);
                bus_node.add_child(TrackStructure::new(track_name));
            }
            
            // Skip the BUS track itself
            for track in bus_tracks {
                let track_name = format_track_name_default(&track.1);
                if !track_name.contains(&format!("{} (BUS)", group_name)) {
                    bus_node.add_child(TrackStructure::new(track_name));
                }
            }
            
            group_node.add_child(bus_node);
        } else {
            // No BUS track, add all tracks directly to group
            for track in &no_subtype_tracks {
                let track_name = format_track_name_default(&track.1);
                let mut leaf = TrackStructure::new(track_name);
                if let Some(ref tt) = track.1.track_type {
                    leaf.track_type = Some(tt.clone());
                }
                group_node.add_child(leaf);
            }
        }
        
        parent.add_child(group_node);
        return; // Early return - we've handled all tracks
    }
    
    // For each sub-type, build its structure
    for (subtype_name, subtype_tracks) in by_subtype {
        // Create sub-type node (e.g., "Kick")
        let mut subtype_node = TrackStructure::new(&subtype_name);
        
        // Group by track type hierarchy: BUS tracks contain SUM tracks, which contain multi-mic tracks
        // First, separate tracks with track types from those without
        let mut tracks_with_type: Vec<&(String, TrackName)> = Vec::new();
        let mut tracks_without_type: Vec<&(String, TrackName)> = Vec::new();
        
        for track in subtype_tracks {
            if track.1.track_type.is_some() {
                tracks_with_type.push(track);
            } else {
                tracks_without_type.push(track);
            }
        }
        
        // Group tracks with types by their track type
        let mut by_track_type: HashMap<String, Vec<&(String, TrackName)>> = HashMap::new();
        for track in tracks_with_type {
            if let Some(ref tt) = track.1.track_type {
                by_track_type.entry(tt.clone()).or_insert_with(Vec::new).push(track);
            }
        }
        
        // Build hierarchy: BUS -> SUM -> multi-mic tracks
        // First, handle BUS tracks
        if let Some(bus_tracks) = by_track_type.remove("BUS") {
            let mut bus_node = TrackStructure::with_track_type(&subtype_name, "BUS".to_string());
            
            // Check if there are SUM tracks that should be nested under BUS
            if let Some(sum_tracks) = by_track_type.remove("SUM") {
                let mut sum_node = TrackStructure::with_track_type(&subtype_name, "SUM".to_string());
                
                // Skip the SUM track itself (it's already represented by sum_node)
                for track in sum_tracks {
                    let track_name = format_track_name_default(&track.1);
                    if !track_name.contains(&format!("{} (SUM)", subtype_name)) {
                        sum_node.add_child(TrackStructure::new(track_name));
                    }
                }
                
                // Find multi-mic tracks that should go under SUM
                let mut sum_multi_mic: Vec<&(String, TrackName)> = Vec::new();
                let mut bus_level_tracks: Vec<&(String, TrackName)> = Vec::new();
                
                for track in &tracks_without_type {
                    // If track has multi-mic descriptors, it goes under SUM
                    let has_multi_mic = track.1.multi_mic.is_some() 
                        && !track.1.multi_mic.as_ref().unwrap().is_empty();
                    
                    // Check track name for common multi-mic patterns as fallback
                    let track_name_lower = track.0.to_lowercase();
                    let has_multi_mic_pattern = track_name_lower.contains(" in")
                        || track_name_lower.contains(" out")
                        || track_name_lower.contains(" top")
                        || track_name_lower.contains(" bottom")
                        || track_name_lower.contains(" trig")
                        || track_name_lower.contains(" close")
                        || track_name_lower.contains(" room");
                    
                    if has_multi_mic || has_multi_mic_pattern {
                        sum_multi_mic.push(track);
                    } else {
                        // Otherwise, it goes directly under BUS (like "Kick Sub", "Kick Ambient")
                        bus_level_tracks.push(track);
                    }
                }
                
                // Add multi-mic tracks under SUM
                for track in sum_multi_mic {
                    let track_name = format_track_name_default(&track.1);
                    sum_node.add_child(TrackStructure::new(track_name));
                }
                
                bus_node.add_child(sum_node);
                
                // Add remaining tracks without types that should be at BUS level
                for track in bus_level_tracks {
                    let track_name = format_track_name_default(&track.1);
                    bus_node.add_child(TrackStructure::new(track_name));
                }
            } else {
                // No SUM track, add all tracks without types directly to BUS
                for track in &tracks_without_type {
                    let track_name = format_track_name_default(&track.1);
                    bus_node.add_child(TrackStructure::new(track_name));
                }
            }
            
            // Add other BUS tracks (like "Kick Sub", "Kick Ambient") directly under BUS
            for track in bus_tracks {
                // Skip if this track is the BUS track itself (it's already represented by the bus_node)
                let track_name = format_track_name_default(&track.1);
                // Only add if it's not just the BUS track name
                if !track_name.contains(&format!("{} (BUS)", subtype_name)) {
                    bus_node.add_child(TrackStructure::new(track_name));
                }
            }
            
            subtype_node.add_child(bus_node);
        } else {
            // No BUS track, but might have SUM or other types
            // Handle remaining track types (SUM if not under BUS, or other types)
            for (track_type, type_tracks) in by_track_type {
                let mut type_node = TrackStructure::with_track_type(&subtype_name, track_type.clone());
                
                for track in type_tracks {
                    let track_name = format_track_name_default(&track.1);
                    type_node.add_child(TrackStructure::new(track_name));
                }
                
                subtype_node.add_child(type_node);
            }
            
            // Add tracks without track types directly to sub-type (allowing duplicates and auto-incrementing)
            for track in tracks_without_type {
                let track_name = format_track_name_default(&track.1);
                let mut leaf = TrackStructure::new(track_name);
                if let Some(ref tt) = track.1.track_type {
                    leaf.track_type = Some(tt.clone());
                }
                subtype_node.add_child(leaf);
            }
        }
        
        parent.add_child(subtype_node);
    }
}

/// Print track structure tree
fn print_structure(structure: &TrackStructure, indent: usize) {
    // Skip printing the root structure name since we already printed it prominently
    if indent == 0 {
        // Just print children
        for (i, child) in structure.children.iter().enumerate() {
            let is_last = i == structure.children.len() - 1;
            let child_prefix = if is_last { "└── " } else { "├── " };
            print_structure_indented(child, indent, child_prefix);
        }
        return;
    }
    
    let indent_str = "  ".repeat(indent);
    let prefix = if indent == 0 { "" } else { "├── " };
    
    if let Some(ref track_type) = structure.track_type {
        println!("{}{}{} ({})", indent_str, prefix, structure.name, track_type);
    } else {
        println!("{}{}{}", indent_str, prefix, structure.name);
    }
    
    for (i, child) in structure.children.iter().enumerate() {
        let is_last = i == structure.children.len() - 1;
        let child_prefix = if is_last { "└── " } else { "├── " };
        print_structure_indented(child, indent + 1, child_prefix);
    }
}

fn print_structure_indented(structure: &TrackStructure, indent: usize, prefix: &str) {
    let indent_str = "  ".repeat(indent);
    
    if let Some(ref track_type) = structure.track_type {
        println!("{}{}{} ({})", indent_str, prefix, structure.name, track_type);
    } else {
        println!("{}{}{}", indent_str, prefix, structure.name);
    }
    
    for (i, child) in structure.children.iter().enumerate() {
        let is_last = i == structure.children.len() - 1;
        let child_prefix = if is_last { "└── " } else { "├── " };
        print_structure_indented(child, indent + 1, child_prefix);
    }
}

