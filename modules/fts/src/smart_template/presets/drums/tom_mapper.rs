//! Tom mapping system
//!
//! Maps sub-type + increment combinations to unified tom numbers.
//! For example:
//! - "Rack 1" → "Tom 1" (unified) or "Rack Tom 1" (preserved)
//! - "Rack 2" → "Tom 2" (unified) or "Rack Tom 2" (preserved)
//! - "Floor 1" → "Tom 3" (unified) or "Floor Tom 1" (preserved)
//! - "Floor 2" → "Tom 4" (unified) or "Floor Tom 2" (preserved)
//!
//! Uses the generic IncrementGroupMapper for the mapping logic.

use crate::smart_template::features::naming::item_properties::ItemProperties;
use crate::smart_template::features::matching::group_mapper::{IncrementGroupMapper, TrackNameExtractor, NamePreservation, MappingResult};

/// Implement TrackNameExtractor for ItemProperties
impl TrackNameExtractor for ItemProperties {
    fn get_sub_type(&self) -> Option<&str> {
        self.sub_type.as_ref()
            .and_then(|v| v.first())
            .map(|s| s.as_str())
    }
    
    fn get_increment(&self) -> Option<u32> {
        self.increment.as_ref()
            .and_then(|s| s.parse::<u32>().ok())
    }
    
    fn get_original_name(&self) -> &str {
        self.original_name.as_deref().unwrap_or("")
    }
}

/// Maps (sub_type, increment) combinations to unified tom numbers
///
/// Wraps IncrementGroupMapper with tom-specific logic
#[derive(Debug)]
pub struct TomMapper {
    mapper: IncrementGroupMapper,
    preservation_strategy: NamePreservation,
}

impl TomMapper {
    /// Create a new tom mapper with unified naming (always "Tom 1", "Tom 2", etc.)
    pub fn new() -> Self {
        Self {
            mapper: IncrementGroupMapper::new("Tom".to_string(), 1),
            preservation_strategy: NamePreservation::Unified,
        }
    }
    
    /// Create a tom mapper with name preservation strategy
    pub fn with_preservation(preservation: NamePreservation) -> Self {
        Self {
            mapper: IncrementGroupMapper::new("Tom".to_string(), 1),
            preservation_strategy: preservation,
        }
    }
    
    /// Map a tom track name with the configured preservation strategy
    pub fn map_track_name(&mut self, track_name: &ItemProperties) -> MappingResult {
        self.mapper.map_with_preservation(track_name, self.preservation_strategy)
    }
    
    /// Get unified tom number for a parsed tom track name
    /// (for backward compatibility)
    pub fn get_tom_number(&mut self, track_name: &ItemProperties) -> u32 {
        let result = self.map_track_name(track_name);
        // Extract number from unified name (e.g., "Tom 1" → 1)
        result.unified_name
            .split_whitespace()
            .last()
            .and_then(|s| s.parse::<u32>().ok())
            .unwrap_or(1)
    }
    
    /// Get the unified track name for a tom number
    pub fn get_track_name(&self, tom_number: u32) -> String {
        self.mapper.build_unified_name(tom_number)
    }
    
    /// Get the display name for a tom number (may preserve sub-type)
    pub fn get_display_name(&self, tom_number: u32) -> Option<String> {
        self.mapper.get_display_name(tom_number).cloned()
    }
    
    /// Get all mapped tom numbers
    pub fn get_all_tom_numbers(&self) -> Vec<u32> {
        // This would need to be exposed from IncrementGroupMapper
        // For now, return empty - can be enhanced later
        vec![]
    }
    
    /// Check if a tom number exists
    pub fn has_tom_number(&self, tom_number: u32) -> bool {
        self.mapper.get_display_name(tom_number).is_some()
    }
}

impl Default for TomMapper {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::smart_template::features::naming::item_properties::ItemProperties;
    
    #[test]
    fn test_rack_floor_mapping() {
        let mut mapper = TomMapper::new();
        
        // Rack 1 → Tom 1
        let rack1 = ItemProperties {
            original_name: Some("Rack 1".to_string()),
            group_prefix: Some("T".to_string()),
            sub_type: Some(vec!["Rack".to_string()]),
            increment: Some("1".to_string()),
            playlist: None,
            ..Default::default()
        };
        assert_eq!(mapper.get_tom_number(&rack1), 1);
        assert_eq!(mapper.get_track_name(1), "Tom 1");
        
        // Rack 2 → Tom 2
        let rack2 = ItemProperties {
            original_name: Some("Rack 2".to_string()),
            group_prefix: Some("T".to_string()),
            sub_type: Some(vec!["Rack".to_string()]),
            increment: Some("2".to_string()),
            playlist: None,
            ..Default::default()
        };
        assert_eq!(mapper.get_tom_number(&rack2), 2);
        assert_eq!(mapper.get_track_name(2), "Tom 2");
        
        // Floor 1 → Tom 3 (next available)
        let floor1 = ItemProperties {
            original_name: Some("Floor 1".to_string()),
            group_prefix: Some("T".to_string()),
            sub_type: Some(vec!["Floor".to_string()]),
            increment: Some("1".to_string()),
            playlist: None,
            ..Default::default()
        };
        assert_eq!(mapper.get_tom_number(&floor1), 3);
        assert_eq!(mapper.get_track_name(3), "Tom 3");
        
        // Floor 2 → Tom 4
        let floor2 = ItemProperties {
            original_name: Some("Floor 2".to_string()),
            group_prefix: Some("T".to_string()),
            sub_type: Some(vec!["Floor".to_string()]),
            increment: Some("2".to_string()),
            playlist: None,
            ..Default::default()
        };
        assert_eq!(mapper.get_tom_number(&floor2), 4);
        assert_eq!(mapper.get_track_name(4), "Tom 4");
    }
    
    #[test]
    fn test_same_combination_returns_same_number() {
        let mut mapper = TomMapper::new();
        
        let rack1 = ItemProperties {
            original_name: Some("Rack 1".to_string()),
            group_prefix: Some("T".to_string()),
            sub_type: Some(vec!["Rack".to_string()]),
            increment: Some("1".to_string()),
            playlist: None,
            ..Default::default()
        };
        
        let tom1 = mapper.get_tom_number(&rack1);
        let tom2 = mapper.get_tom_number(&rack1);
        
        assert_eq!(tom1, tom2);
        assert_eq!(tom1, 1);
    }
    
    #[test]
    fn test_no_sub_type() {
        let mut mapper = TomMapper::new();
        
        let tom1 = ItemProperties {
            original_name: Some("Tom 1".to_string()),
            group_prefix: Some("T".to_string()),
            sub_type: None,
            increment: Some("1".to_string()),
            playlist: None,
            ..Default::default()
        };
        
        assert_eq!(mapper.get_tom_number(&tom1), 1);
    }
}
