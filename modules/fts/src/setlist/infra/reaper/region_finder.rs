//! Region and Marker Finding Utilities
//!
//! Helper functions for finding song regions, markers, and related data.

use daw::marker_region::core::{Marker, Region};

/// Find a marker by name (case-insensitive)
pub fn find_marker_by_name<'a>(markers: &'a [Marker], name: &str) -> Option<&'a Marker> {
    markers
        .iter()
        .find(|m| m.name.trim().eq_ignore_ascii_case(name))
}

/// Find song regions by identifying regions that contain other regions (sections)
/// A song region is the largest region that contains multiple other regions within it.
pub fn find_song_regions(regions: &[Region]) -> Vec<Region> {
    let mut song_regions = Vec::new();

    for region in regions {
        // Count how many other regions are contained within this region
        let contained_count = regions
            .iter()
            .filter(|other| {
                other != &region
                    && other.start_seconds() >= region.start_seconds()
                    && other.end_seconds() <= region.end_seconds()
            })
            .count();

        // If this region contains at least 2 other regions, it's likely a song region
        if contained_count >= 2 {
            song_regions.push(region.clone());
        }
    }

    // Sort by start time
    song_regions.sort_by(|a, b| {
        a.start_seconds()
            .partial_cmp(&b.start_seconds())
            .unwrap_or(std::cmp::Ordering::Equal)
    });

    song_regions
}

/// Find the song region for a specific SONGSTART marker
/// Returns the largest region that contains the marker and the most sections
pub fn find_song_region_for_marker<'a>(
    start_marker: &Marker,
    _markers: &'a [Marker],
    regions: &'a [Region],
) -> Option<Region> {
    use tracing::debug;

    // Find all regions that contain this marker
    let marker_time = start_marker.position.time.to_seconds();
    let containing_regions: Vec<&Region> = regions
        .iter()
        .filter(|r| r.start_seconds() <= marker_time && r.end_seconds() >= marker_time)
        .collect();

    debug!(
        marker_pos = marker_time,
        containing_count = containing_regions.len(),
        "Finding song region for marker"
    );

    if containing_regions.is_empty() {
        return None;
    }

    // If only one, use it
    if containing_regions.len() == 1 {
        let selected = containing_regions[0].clone();
        debug!(
            region_name = %selected.name,
            region_color = ?selected.color,
            "Selected single containing region as song region"
        );
        return Some(selected);
    }

    // If multiple, find the one that contains the most other regions (sections)
    // This is the parent container (song region)
    let mut best_region: Option<&Region> = None;
    let mut max_contained_count = 0;

    for candidate in &containing_regions {
        let contained_count = regions
            .iter()
            .filter(|other| {
                // Don't count self
                if other.start_seconds() == candidate.start_seconds()
                    && other.end_seconds() == candidate.end_seconds()
                {
                    return false;
                }

                // Check if contained
                other.start_seconds() >= candidate.start_seconds()
                    && other.end_seconds() <= candidate.end_seconds()
            })
            .count();

        if contained_count > max_contained_count {
            max_contained_count = contained_count;
            best_region = Some(candidate);
        } else if contained_count == max_contained_count && max_contained_count > 0 {
            // If tied, prefer the larger one (longer duration)
            if let Some(current_best) = best_region {
                let candidate_duration = candidate.end_seconds() - candidate.start_seconds();
                let best_duration = current_best.end_seconds() - current_best.start_seconds();
                if candidate_duration > best_duration {
                    best_region = Some(candidate);
                }
            }
        }
    }

    if let Some(selected) = best_region {
        let cloned = selected.clone();
        debug!(
            region_name = %cloned.name,
            region_color = ?cloned.color,
            contained_sections = max_contained_count,
            "Selected best containing region as song region"
        );
        Some(cloned)
    } else {
        None
    }
}

/// Find a song region based on markers and regions
/// Looks for SONGSTART marker and finds the corresponding region
/// Returns a reference to the region (for compatibility with old code)
pub fn find_song_region<'a>(markers: &'a [Marker], regions: &'a [Region]) -> Option<&'a Region> {
    // Look for SONGSTART marker
    let song_start_marker = find_marker_by_name(markers, "SONGSTART")
        .or_else(|| find_marker_by_name(markers, "=START"))
        .or_else(|| find_marker_by_name(markers, "START"));

    if let Some(marker) = song_start_marker {
        // Find the region that contains this marker
        // find_song_region_for_marker returns Option<Region>, so we need to find it in the slice
        if let Some(region) = find_song_region_for_marker(marker, markers, regions) {
            // Find the matching region in the slice
            return regions.iter().find(|r| {
                r.start_seconds() == region.start_seconds()
                    && r.end_seconds() == region.end_seconds()
            });
        }
    }

    // If no SONGSTART marker, look for the largest region
    if let Some(largest) = regions.iter().max_by(|a, b| {
        let a_len = a.end_seconds() - a.start_seconds();
        let b_len = b.end_seconds() - b.start_seconds();
        a_len
            .partial_cmp(&b_len)
            .unwrap_or(std::cmp::Ordering::Equal)
    }) {
        return Some(largest);
    }

    None
}
