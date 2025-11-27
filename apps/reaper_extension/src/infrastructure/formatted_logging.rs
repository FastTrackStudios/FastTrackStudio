//! Formatted logging utilities for better log readability
//!
//! Provides formatted display implementations for various log messages
//! to make them more visually appealing and easier to scan.
//! Uses consistent formatting with emojis, brackets, and separators for REAPER console compatibility.
//! Format: [ICON] TYPE [identifier] | details

/// Format a transport change message with better readability
pub fn format_transport_change(changes: &[String], project_id: &str) -> String {
    let changes_str = if changes.is_empty() {
        "".to_string()
    } else {
        format!(" | {}", changes.join(", "))
    };
    
    format!("🚀 TRANSPORT [{}]{}", project_id, changes_str)
}

/// Format a track change message with better readability
pub fn format_track_change(changes: &[String], project_id: &str) -> String {
    let changes_str = if changes.is_empty() {
        "".to_string()
    } else {
        // Limit to first few changes to avoid log spam
        let display_changes: Vec<_> = changes.iter().take(3).cloned().collect();
        let more = if changes.len() > 3 {
            format!(" (+{} more)", changes.len() - 3)
        } else {
            String::new()
        };
        format!(" | {}", display_changes.join(", ").to_string() + &more)
    };
    
    format!("🎵 TRACKS [{}]{}", project_id, changes_str)
}

/// Format a lyrics change message with better readability
pub fn format_lyrics_change(song_name: &str) -> String {
    format!("🎵 LYRICS [{}]", song_name)
}

/// Format an active slide change message with better readability
pub fn format_active_slide_change(song_name: &str, slide_index: Option<usize>, previous: Option<usize>) -> String {
    let slide_str = if let Some(idx) = slide_index {
        idx.to_string()
    } else {
        "None".to_string()
    };
    
    let prev_str = if let Some(prev_idx) = previous {
        prev_idx.to_string()
    } else {
        "—".to_string()
    };
    
    format!("📄 LYRICS [{}] {} → {}", song_name, prev_str, slide_str)
}

/// Format a setlist change message with better readability
pub fn format_setlist_change(changes: &[String]) -> String {
    let changes_str = if changes.is_empty() {
        "".to_string()
    } else {
        // Limit to first few changes to avoid log spam
        let display_changes: Vec<_> = changes.iter().take(3).cloned().collect();
        let more = if changes.len() > 3 {
            format!(" (+{} more)", changes.len() - 3)
        } else {
            String::new()
        };
        format!(" | {}", display_changes.join(", ").to_string() + &more)
    };
    
    format!("📋 SETLIST{}", changes_str)
}

/// Format an active indices change message with better readability
pub fn format_active_indices_change(changes: &[String]) -> String {
    let changes_str = if changes.is_empty() {
        "".to_string()
    } else {
        format!(" | {}", changes.join(", "))
    };
    
    format!("📍 INDICES{}", changes_str)
}

/// Format a track-specific change message with better readability
pub fn format_track_specific_change(track_name: &str, track_index: usize, changes: &[String]) -> String {
    let changes_str = if changes.is_empty() {
        "".to_string()
    } else {
        format!(" | {}", changes.join(", "))
    };
    
    format!("🎵 TRACK[{}] [{}]{}", track_index, track_name, changes_str)
}

