//! Generic group mapping system
//!
//! Provides a framework for mapping alternate naming styles to unified names
//! while preserving source naming when appropriate.
//!
//! Example use cases:
//! - Tom: "Rack 1", "Floor 2" → unified "Tom 1", "Tom 2" (or preserve "Rack Tom 1", "Floor Tom 2")
//! - Guitar: "Acoustic", "Electric" → unified "Guitar 1", "Guitar 2" (or preserve original)
//! - Vocals: "Lead", "Harmony" → unified "Vocal 1", "Vocal 2" (or preserve original)

use std::collections::HashMap;

/// Strategy for name preservation
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum NamePreservation {
    /// Always use unified names (e.g., always "Tom 1", never "Rack Tom 1")
    Unified,
    
    /// Preserve source naming when it uses alternate style (e.g., "Rack Tom 1" stays "Rack Tom 1")
    PreserveAlternate,
    
    /// Always preserve original source name
    PreserveOriginal,
}

/// Result of mapping a track name
#[derive(Debug, Clone)]
pub struct MappingResult {
    /// The unified track name (for template matching/organization)
    pub unified_name: String,
    
    /// The display name (may be different if preserving source naming)
    pub display_name: String,
    
    /// Whether this is a new mapping (first time seeing this combination)
    pub is_new: bool,
}

/// Generic trait for mapping group track names
///
/// Implementations handle:
/// - Mapping alternate naming styles to unified names
/// - Preserving source naming when appropriate
/// - Tracking mappings for consistency
pub trait GroupMapper: Send + Sync {
    /// The parsed track name type this mapper works with
    type TrackName;
    
    /// Map a parsed track name to unified and display names
    ///
    /// Returns both the unified name (for organization) and display name
    /// (which may preserve source naming based on strategy).
    fn map_track_name(
        &mut self,
        track_name: &Self::TrackName,
        preservation: NamePreservation,
    ) -> MappingResult;
    
    /// Get the unified name for a track name (without creating mapping)
    fn get_unified_name(&self, track_name: &Self::TrackName) -> Option<String>;
    
    /// Get the display name for a track name
    fn get_display_name(&self, track_name: &Self::TrackName) -> Option<String>;
    
    /// Check if a unified name exists
    fn has_unified_name(&self, unified_name: &str) -> bool;
    
    /// Get all unified names that have been mapped
    fn get_all_unified_names(&self) -> Vec<String>;
}

/// Trait for extracting sub-type and increment from track names
pub trait TrackNameExtractor {
    /// Get the sub-type (e.g., "Rack", "Floor", "Acoustic", "Electric")
    fn get_sub_type(&self) -> Option<&str>;
    
    /// Get the increment number (e.g., 1, 2, 3)
    fn get_increment(&self) -> Option<u32>;
    
    /// Get the original source name
    fn get_original_name(&self) -> &str;
}

/// Base implementation for increment-based group mappers
///
/// Handles common patterns like:
/// - Sub-type + increment → unified increment
/// - Preserving sub-type in display name
#[derive(Debug)]
pub struct IncrementGroupMapper {
    /// Maps (sub_type, increment) → unified_increment
    mapping: HashMap<(String, u32), u32>,
    
    /// Reverse mapping: unified_increment → (sub_type, increment)
    reverse_mapping: HashMap<u32, (String, u32)>,
    
    /// Maps unified_increment → original source name (for preservation)
    source_names: HashMap<u32, String>,
    
    /// Maps unified_increment → display name (may preserve sub-type)
    display_names: HashMap<u32, String>,
    
    /// Next available unified increment
    next_increment: u32,
    
    /// Base name for unified tracks (e.g., "Tom", "Guitar")
    base_name: String,
}

impl IncrementGroupMapper {
    /// Create a new increment-based mapper
    pub fn new(base_name: String, start_increment: u32) -> Self {
        Self {
            mapping: HashMap::new(),
            reverse_mapping: HashMap::new(),
            source_names: HashMap::new(),
            display_names: HashMap::new(),
            next_increment: start_increment,
            base_name,
        }
    }
    
    /// Map a track name with name preservation strategy
    pub fn map_with_preservation<T: TrackNameExtractor>(
        &mut self,
        track_name: &T,
        preservation: NamePreservation,
    ) -> MappingResult {
        let sub_type = track_name.get_sub_type();
        let increment = track_name.get_increment();
        let original = track_name.get_original_name();
        
        // Get unified increment (for organization)
        let unified_inc = self.get_unified_increment(sub_type, increment);
        let unified_name = self.build_unified_name(unified_inc);
        
        // Determine display name based on preservation strategy
        // Note: display name uses the ORIGINAL increment, not unified increment
        let display_name = match preservation {
            NamePreservation::Unified => unified_name.clone(),
            NamePreservation::PreserveAlternate => {
                // If we have a sub-type, preserve it with original increment
                if let Some(sub) = sub_type {
                    if !sub.is_empty() {
                        if let Some(orig_inc) = increment {
                            format!("{} {} {}", sub, self.base_name, orig_inc)
                        } else {
                            self.build_display_name(sub_type, unified_inc)
                        }
                    } else {
                        unified_name.clone()
                    }
                } else {
                    unified_name.clone()
                }
            }
            NamePreservation::PreserveOriginal => {
                // Store and use original name
                self.store_source_name(unified_inc, original.to_string());
                original.to_string()
            }
        };
        
        // Store display name
        let is_new = !self.display_names.contains_key(&unified_inc);
        self.display_names.insert(unified_inc, display_name.clone());
        
        MappingResult {
            unified_name,
            display_name,
            is_new,
        }
    }
    
    /// Get or create unified increment for a (sub_type, increment) combination
    pub fn get_unified_increment(&mut self, sub_type: Option<&str>, increment: Option<u32>) -> u32 {
        let sub_type_key = sub_type.unwrap_or("").to_string();
        let increment_key = increment.unwrap_or(0);
        let key = (sub_type_key.clone(), increment_key);
        
        // Check if we already have a mapping
        if let Some(&unified_inc) = self.mapping.get(&key) {
            return unified_inc;
        }
        
        // Determine candidate increment
        // If no sub-type, try to use increment directly (base name like "Tom 1")
        // If sub-type exists, also try increment first, but will find next if taken
        let mut candidate_inc = if increment_key > 0 {
            increment_key
        } else {
            self.next_increment
        };
        
        // If the candidate is already taken, find next available
        // For base names (no sub-type), we still need to find next if taken
        // but ideally base names should come first and get their increment
        while self.reverse_mapping.contains_key(&candidate_inc) {
            candidate_inc = self.next_increment;
            self.next_increment += 1;
        }
        
        // Update next_increment if we used a higher number
        if candidate_inc >= self.next_increment {
            self.next_increment = candidate_inc + 1;
        }
        
        // Create the mapping
        self.mapping.insert(key.clone(), candidate_inc);
        self.reverse_mapping.insert(candidate_inc, key);
        
        candidate_inc
    }
    
    /// Build unified name from increment
    pub fn build_unified_name(&self, increment: u32) -> String {
        format!("{} {}", self.base_name, increment)
    }
    
    /// Build display name preserving sub-type
    pub fn build_display_name(&self, sub_type: Option<&str>, increment: u32) -> String {
        if let Some(sub) = sub_type {
            if !sub.is_empty() {
                return format!("{} {} {}", sub, self.base_name, increment);
            }
        }
        self.build_unified_name(increment)
    }
    
    /// Store source name for preservation
    pub fn store_source_name(&mut self, increment: u32, source_name: String) {
        self.source_names.insert(increment, source_name);
    }
    
    /// Get stored source name
    pub fn get_source_name(&self, increment: u32) -> Option<&String> {
        self.source_names.get(&increment)
    }
    
    /// Get display name for a unified increment
    pub fn get_display_name(&self, increment: u32) -> Option<&String> {
        self.display_names.get(&increment)
    }
    
    /// Get unified increment for a track name (read-only, doesn't create)
    pub fn get_unified_increment_readonly(&self, sub_type: Option<&str>, increment: Option<u32>) -> Option<u32> {
        let sub_type_key = sub_type.unwrap_or("").to_string();
        let increment_key = increment.unwrap_or(0);
        let key = (sub_type_key, increment_key);
        self.mapping.get(&key).copied()
    }
    
    /// Get all unified names that have been mapped
    pub fn get_all_unified_names(&self) -> Vec<String> {
        self.reverse_mapping.keys()
            .map(|&inc| self.build_unified_name(inc))
            .collect()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_increment_mapper_basic() {
        let mut mapper = IncrementGroupMapper::new("Tom".to_string(), 1);
        
        // Rack 1 → Tom 1
        let inc1 = mapper.get_unified_increment(Some("Rack"), Some(1));
        assert_eq!(inc1, 1);
        assert_eq!(mapper.build_unified_name(inc1), "Tom 1");
        assert_eq!(mapper.build_display_name(Some("Rack"), inc1), "Rack Tom 1");
        
        // Rack 2 → Tom 2
        let inc2 = mapper.get_unified_increment(Some("Rack"), Some(2));
        assert_eq!(inc2, 2);
        
        // Floor 1 → Tom 3 (next available)
        let inc3 = mapper.get_unified_increment(Some("Floor"), Some(1));
        assert_eq!(inc3, 3);
    }
    
    #[test]
    fn test_name_preservation_strategies() {
        let mut mapper = IncrementGroupMapper::new("Tom".to_string(), 1);
        
        let inc = mapper.get_unified_increment(Some("Rack"), Some(1));
        
        // Unified: always use "Tom 1"
        assert_eq!(mapper.build_unified_name(inc), "Tom 1");
        
        // Preserve alternate: use "Rack Tom 1"
        assert_eq!(mapper.build_display_name(Some("Rack"), inc), "Rack Tom 1");
    }
}
