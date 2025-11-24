//! Example: Organizing random track names into a structured hierarchy
//!
//! This example demonstrates how to:
//! 1. Parse a list of track name strings
//! 2. Match them against default group configurations
//! 3. Organize them into a track structure hierarchy
//!
//! Run with: `cargo run --example organize_tracks --features default-groups`

use std::collections::HashMap;
use naming_convention::{
    TrackName, TrackStructure, SimpleParser,
    create_default_groups,
};

fn main() {
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
    
    println!("Organizing {} track names into structure...\n", track_names.len());
    
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
    
    // Print the structure
    println!("\n\n{}", "=".repeat(70));
    println!();
    println!("  {}", structure.name.to_uppercase());
    println!();
    println!("{}\n", "=".repeat(70));
    print_structure(&structure, 0);
    
    println!("\n\nParsed {} track names:", parsed_tracks.len());
    for (original, parsed) in &parsed_tracks {
        println!("  {} -> {:?}", original, format_track_name_simple(parsed));
    }
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
    
    // Build structure for each group
    for (group_prefix, tracks) in by_group {
        let tracks_clone = tracks.clone();
        let group_structure = build_group_structure(&group_prefix, tracks);
        if group_structure.has_children() || !tracks_clone.is_empty() {
            root.add_child(group_structure);
        }
    }
    
    root
}

/// Build structure for a specific group (e.g., "D" for Drums)
fn build_group_structure(group_prefix: &str, tracks: Vec<&(String, TrackName)>) -> TrackStructure {
    // Determine group name from prefix or first track
    let group_name = if let Some(first_track) = tracks.first() {
        // Try to get group name from sub-type or use prefix
        if let Some(ref sub_types) = first_track.1.sub_type {
            if !sub_types.is_empty() {
                // If we have sub-types, the parent group is likely the top-level group
                // For "D" prefix with "Kick" sub-type, the group is "Drums"
                match group_prefix {
                    "D" => "Drums",
                    "B" => "Bass",
                    "GTR" | "G" => "Guitar",
                    "K" => "Keys",
                    "SY" => "Synths",
                    "V" => "Vocals",
                    _ => group_prefix,
                }
            } else {
                group_prefix
            }
        } else {
            match group_prefix {
                "D" => "Drums",
                "B" => "Bass",
                "GTR" | "G" => "Guitar",
                "K" => "Keys",
                "SY" => "Synths",
                "V" => "Vocals",
                _ => group_prefix,
            }
        }
    } else {
        group_prefix
    };
    
    let mut group = TrackStructure::new(group_name);
    
    // Group tracks by their hierarchy path (group prefix -> sub-type -> track type -> multi-mic -> etc.)
    // This builds the nested structure
    let mut by_path: HashMap<Vec<String>, Vec<&(String, TrackName)>> = HashMap::new();
    
    for track in tracks {
        let path = build_track_path(&track.1);
        by_path.entry(path).or_insert_with(Vec::new).push(track);
    }
    
    // Build nested structure
    build_nested_structure(&mut group, by_path);
    
    group
}

/// Build the path for a track based on its components
fn build_track_path(track_name: &TrackName) -> Vec<String> {
    let mut path = Vec::new();
    
    // Add sub-types (these represent the nested group hierarchy)
    if let Some(ref sub_types) = track_name.sub_type {
        for sub_type in sub_types {
            path.push(sub_type.clone());
        }
    }
    
    // Add track type if present (BUS, SUM, etc.)
    if let Some(ref track_type) = track_name.track_type {
        path.push(format!("({})", track_type));
    }
    
    // Add multi-mic descriptors
    if let Some(ref multi_mic) = track_name.multi_mic {
        for mic in multi_mic {
            path.push(mic.clone());
        }
    }
    
    // Add arrangement
    if let Some(ref arrangement) = track_name.arrangement {
        path.push(arrangement.clone());
    }
    
    // Add increment (for auto-incrementing tracks like "Tom 1", "Tom 2")
    if let Some(ref increment) = track_name.increment {
        path.push(increment.clone());
    }
    
    // Add channel
    if let Some(ref channel) = track_name.channel {
        path.push(channel.clone());
    }
    
    path
}

/// Build nested structure recursively
fn build_nested_structure(
    parent: &mut TrackStructure,
    tracks_by_path: HashMap<Vec<String>, Vec<&(String, TrackName)>>,
) {
    use std::collections::HashMap;
    
    // Group tracks by their first path component
    let mut by_first_component: HashMap<String, (Option<String>, Vec<&(String, TrackName)>)> = HashMap::new();
    
    for (path, tracks) in tracks_by_path {
        if path.is_empty() {
            // Leaf node - add all tracks directly (allowing duplicates)
            for track in tracks {
                let track_name = format_track_name_simple(&track.1);
                let mut leaf = TrackStructure::new(track_name);
                if let Some(ref tt) = track.1.track_type {
                    leaf.track_type = Some(tt.clone());
                }
                parent.add_child(leaf);
            }
        } else {
            let first = path[0].clone();
            let rest = if path.len() > 1 {
                path[1..].to_vec()
            } else {
                Vec::new()
            };
            
            // Check if first component is a track type (in parentheses)
            let (node_name, track_type, remaining_path) = if first.starts_with('(') && first.ends_with(')') {
                // This is a track type, create a node with this type
                let tt = first.trim_start_matches('(').trim_end_matches(')').to_string();
                // Get the name from the sub-type or use a default
                let name = if !rest.is_empty() {
                    rest[0].clone()
                } else if let Some(track) = tracks.first() {
                    if let Some(ref sub_types) = track.1.sub_type {
                        if !sub_types.is_empty() {
                            sub_types.last().unwrap().clone()
                        } else {
                            "Track".to_string()
                        }
                    } else {
                        "Track".to_string()
                    }
                } else {
                    "Track".to_string()
                };
                let track_type_str = tt.clone();
                (name, Some(track_type_str), if rest.len() > 1 { rest[1..].to_vec() } else { Vec::new() })
        } else {
            // Regular component
            (first, None, rest)
        };
        
        let entry = by_first_component.entry(node_name.clone()).or_insert_with(|| (track_type, Vec::new()));
            // Store remaining path and tracks
            for track in tracks {
                entry.1.push(track);
            }
        }
    }
    
    // Create child nodes for each first component
    for (node_name, (track_type, tracks)) in by_first_component {
        let track_type_clone = track_type.clone();
        let tracks_clone = tracks.clone();
        let mut child = if let Some(ref tt) = &track_type_clone {
            TrackStructure::with_track_type(&node_name, tt.clone())
        } else {
            TrackStructure::new(&node_name)
        };
        
        // Group remaining tracks by their remaining paths
        // We need to track which components we've already processed
        let mut remaining_by_path: HashMap<Vec<String>, Vec<&(String, TrackName)>> = HashMap::new();
        for track in &tracks_clone {
            let full_path = build_track_path(&track.1);
            
            // Find where this track's path starts matching the node we're creating
            // Skip components until we find the node_name or track_type match
            let mut skip_count = 0;
            let mut found_match = false;
            
            // Check if first component matches node_name or is a track type
            if !full_path.is_empty() {
                let first = &full_path[0];
                if first == &node_name || (first.starts_with('(') && first.ends_with(')') && track_type_clone.is_some()) {
                    skip_count = 1;
                    found_match = true;
                }
            }
            
            // If we found a match, skip that component and use the rest
            let remaining = if found_match && full_path.len() > skip_count {
                full_path[skip_count..].to_vec()
            } else if !found_match && !full_path.is_empty() {
                // If no match found, use the full path (shouldn't happen, but be safe)
                full_path
            } else {
                Vec::new()
            };
            
            remaining_by_path.entry(remaining).or_insert_with(Vec::new).push(*track);
        }
        
        // Recursively build nested structure
        if !remaining_by_path.is_empty() {
            build_nested_structure(&mut child, remaining_by_path);
        } else {
            // All tracks are leaves at this level - add them all (allowing duplicates)
            for track in tracks_clone {
                let track_name = format_track_name_simple(&track.1);
                let mut leaf = TrackStructure::new(track_name);
                if let Some(ref tt) = track.1.track_type {
                    leaf.track_type = Some(tt.clone());
                }
                child.add_child(leaf);
            }
        }
        
        parent.add_child(child);
    }
}

/// Format track name for display using the formatter
fn format_track_name_simple(track_name: &TrackName) -> String {
    naming_convention::format_track_name_default(track_name)
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

