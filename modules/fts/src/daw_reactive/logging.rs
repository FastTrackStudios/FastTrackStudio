//! Formatted logging utilities for better log readability
//!
//! Provides formatted display implementations for various log messages
//! to make them more visually appealing and easier to scan.

/// Format a transport change message with better readability
pub fn format_transport_change(changes: &[String], project_id: &str) -> String {
    let changes_str = if changes.is_empty() {
        String::new()
    } else {
        format!(" | {}", changes.join(", "))
    };

    format!("TRANSPORT [{}]{}", project_id, changes_str)
}

/// Format a track change message with better readability
pub fn format_track_change(changes: &[String], project_id: &str) -> String {
    let changes_str = if changes.is_empty() {
        String::new()
    } else {
        // Limit to first few changes to avoid log spam
        let display_changes: Vec<_> = changes.iter().take(3).cloned().collect();
        let more = if changes.len() > 3 {
            format!(" (+{} more)", changes.len() - 3)
        } else {
            String::new()
        };
        format!(" | {}", display_changes.join(", ") + &more)
    };

    format!("TRACKS [{}]{}", project_id, changes_str)
}

/// Format a track-specific change message with better readability
pub fn format_track_specific_change(
    track_name: &str,
    track_index: usize,
    changes: &[String],
) -> String {
    let changes_str = if changes.is_empty() {
        String::new()
    } else {
        format!(" | {}", changes.join(", "))
    };

    format!("TRACK[{}] [{}]{}", track_index, track_name, changes_str)
}
