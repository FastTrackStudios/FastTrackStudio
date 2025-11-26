use rpp_parser::{parse_rpp_file, RppBlockContent};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    // Read the template file
    let template_path = "resources/Template-with-takes-lanes.RPP";
    let content = std::fs::read_to_string(template_path)
        .expect("Failed to read template file");
    
    // Parse the RPP file
    let rpp_project = parse_rpp_file(&content)?;
    
    // Print the track hierarchy
    println!("=== REAPER PROJECT TRACK HIERARCHY ===");
    println!();
    
    // Collect all tracks with their folder state and indentation levels
    let mut tracks = Vec::new();
    for block in &rpp_project.blocks {
        if block.block_type == rpp_parser::BlockType::Track {
            let track_name = get_track_name(block);
            let (folder_state, indentation) = get_track_folder_info(block);
            tracks.push((track_name, folder_state, indentation));
        }
    }
    
    // Print tracks with proper indentation based on the indentation field
    print_track_hierarchy(&tracks);
    
    Ok(())
}

fn print_track_hierarchy(tracks: &[(String, i32, i32)]) {
    let mut current_indent = 0;
    
    for (name, folder_state, indentation_change) in tracks {
        // Create indentation string for current track
        let indent = "  ".repeat(current_indent);
        
        // Choose the appropriate tree character based on folder state
        // field 1 of ISBUS: 0=regular track, 1=folder parent, 2=last track in folder
        let tree_char = if *folder_state == 1 { "📁" } else { "├─" };
        
        // Print track name with proper indentation
        println!("{}{} {}", indent, tree_char, name);
        
        // Update indentation for NEXT track based on the current track's indentation field
        // field 2 of ISBUS: 0=no change, 1=increase by 1, -n=decrease by n
        if *indentation_change > 0 {
            current_indent += *indentation_change as usize;
        } else if *indentation_change < 0 {
            current_indent = current_indent.saturating_sub((-*indentation_change) as usize);
        }
    }
}

fn get_track_name(block: &rpp_parser::RppBlock) -> String {
    for child in &block.children {
        if let RppBlockContent::Content(tokens) = child {
            if let Some(first_token) = tokens.first() {
                if first_token.to_string() == "NAME" {
                    if let Some(name_token) = tokens.get(1) {
                        return name_token.to_string();
                    }
                }
            }
        }
    }
    format!("Track {}", block.params.get(0).map(|p| p.to_string()).unwrap_or_else(|| "Unknown".to_string()))
}

fn get_track_folder_info(block: &rpp_parser::RppBlock) -> (i32, i32) {
    for child in &block.children {
        if let RppBlockContent::Content(tokens) = child {
            if let Some(first_token) = tokens.first() {
                if first_token.to_string() == "ISBUS" {
                    if tokens.len() >= 3 {
                        let folder_state = tokens[1].to_string().parse::<i32>().unwrap_or(0);
                        let indentation = tokens[2].to_string().parse::<i32>().unwrap_or(0);
                        return (folder_state, indentation);
                    }
                }
            }
        }
    }
    (0, 0) // Default to regular track with no indentation change
}
