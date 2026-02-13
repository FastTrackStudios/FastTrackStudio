//! Local state for the preset browser dialog.
//!
//! These are NOT global signals — they're local to the browser dialog
//! and reset when the dialog is closed.

use std::collections::HashSet;

use signal_control::id::TagId;
use signal_control::rating::PresetStatus;
use signal_control::tags::browse::{BrowseLevel, BrowseMode, SortMode};

/// Local state for the preset browser.
///
/// All filtering, sorting, and selection state lives here.
/// Created fresh when the browser opens.
#[derive(Debug, Clone)]
pub struct BrowserState {
    /// Text search query.
    pub query: String,
    /// Active browse mode.
    pub browse_mode: BrowseMode,
    /// Active browse level filter.
    pub browse_level: BrowseLevel,
    /// Active sort mode.
    pub sort_mode: SortMode,
    /// Selected tags per category (category display_name -> set of TagIds).
    pub selected_tags: Vec<HashSet<TagId>>,
    /// Status filter (None = show all).
    pub status_filter: Option<PresetStatus>,
    /// Minimum quality rating (0 = any).
    pub min_quality: u8,
    /// Currently selected preset index in results.
    pub selected_result_index: Option<usize>,
    /// Currently selected scene index in preview.
    pub selected_scene_index: Option<usize>,
}

impl Default for BrowserState {
    fn default() -> Self {
        Self {
            query: String::new(),
            browse_mode: BrowseMode::default(),
            browse_level: BrowseLevel::default(),
            sort_mode: SortMode::default(),
            selected_tags: Vec::new(),
            status_filter: None,
            min_quality: 0,
            selected_result_index: None,
            selected_scene_index: None,
        }
    }
}

impl BrowserState {
    /// Create a new browser state.
    pub fn new() -> Self {
        Self::default()
    }

    /// Reset all filters to defaults.
    pub fn reset_filters(&mut self) {
        self.query.clear();
        self.selected_tags.clear();
        self.status_filter = None;
        self.min_quality = 0;
        self.selected_result_index = None;
        self.selected_scene_index = None;
    }

    /// Whether any filter is active.
    pub fn has_active_filters(&self) -> bool {
        !self.query.is_empty()
            || !self.selected_tags.iter().all(|s| s.is_empty())
            || self.status_filter.is_some()
            || self.min_quality > 0
    }

    /// Toggle a tag selection within a column.
    pub fn toggle_tag(&mut self, column_index: usize, tag_id: TagId) {
        // Ensure we have enough columns
        while self.selected_tags.len() <= column_index {
            self.selected_tags.push(HashSet::new());
        }

        let set = &mut self.selected_tags[column_index];
        if set.contains(&tag_id) {
            set.remove(&tag_id);
        } else {
            set.insert(tag_id);
        }

        // Reset result selection when filters change
        self.selected_result_index = None;
        self.selected_scene_index = None;
    }
}
