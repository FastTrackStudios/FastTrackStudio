//! Example: Parse markers and regions from RPP files
//! 
//! This example demonstrates how to parse markers and regions from REAPER
//! project files and display them in a structured format.

use rpp_parser::{parse_rpp_file, ReaperProject};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    // Read the Region Test file
    let template_path = "resources/Region Test.RPP";
    let content = std::fs::read_to_string(template_path)
        .expect("Failed to read Region Test file");
    
    // Parse the RPP file
    let rpp_project = parse_rpp_file(&content)?;
    
    // Convert to ReaperProject
    let project = ReaperProject::from_rpp_project(&rpp_project)?;
    
    // Display the project summary
    println!("=== REAPER PROJECT WITH MARKERS AND REGIONS ===");
    println!("{}", project);
    
    // Display detailed markers and regions
    println!("\n=== MARKERS AND REGIONS DETAILS ===");
    println!("{}", project.markers_regions);
    
    // Display with musical positions if tempo is available
    if let Some(tempo) = project.properties.tempo {
        let (bpm, time_sig_num, time_sig_den, _) = tempo;
        println!("\n=== MARKERS AND REGIONS WITH MUSICAL POSITIONS ===");
        println!("{}", project.markers_regions.display_with_musical_positions(bpm as f64, time_sig_num, time_sig_den));
    }
    
    // Show markers and regions separately
    println!("\n=== MARKERS ONLY ===");
    for marker in project.markers_regions.markers_sorted() {
        println!("{}", marker);
    }
    
    println!("\n=== REGIONS ONLY ===");
    for region in project.markers_regions.regions_sorted() {
        println!("{}", region);
    }
    
    // Show timeline view
    println!("\n=== TIMELINE VIEW ===");
    for item in project.markers_regions.all_sorted() {
        let marker_type = if item.is_region() { "Region" } else { "Marker" };
        let duration_info = if let Some(duration) = item.duration() {
            format!(" (duration: {:.3}s)", duration)
        } else {
            String::new()
        };
        println!("{:.3}s: {} #{} \"{}\"{}", 
                 item.position, marker_type, item.id, item.name, duration_info);
    }
    
    Ok(())
}
