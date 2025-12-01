//! Component pattern associations
//!
//! Trait-based system for associating patterns with component types.

use crate::component::ComponentType;
use std::collections::HashMap;

/// Trait for types that can provide patterns for specific component types
pub trait ComponentPatternProvider {
    /// Get patterns for a specific component type
    fn get_patterns(&self, component_type: ComponentType) -> Vec<String>;
    
    /// Get negative patterns for a specific component type
    fn get_negative_patterns(&self, component_type: ComponentType) -> Vec<String>;
    
    /// Set patterns for a specific component type
    fn set_patterns(&mut self, component_type: ComponentType, patterns: Vec<String>);
    
    /// Set negative patterns for a specific component type
    fn set_negative_patterns(&mut self, component_type: ComponentType, patterns: Vec<String>);
    
    /// Add a pattern to a component type
    fn add_pattern(&mut self, component_type: ComponentType, pattern: String);
    
    /// Add a negative pattern to a component type
    fn add_negative_pattern(&mut self, component_type: ComponentType, pattern: String);
}

/// Storage for component patterns
#[derive(Debug, Clone, Default)]
pub struct ComponentPatterns {
    /// Maps component type to patterns
    patterns: HashMap<ComponentType, Vec<String>>,
    /// Maps component type to negative patterns
    negative_patterns: HashMap<ComponentType, Vec<String>>,
}

impl ComponentPatterns {
    /// Create a new empty ComponentPatterns
    pub fn new() -> Self {
        Self::default()
    }
    
    /// Get all component types that have patterns
    pub fn component_types_with_patterns(&self) -> Vec<ComponentType> {
        self.patterns.keys().copied().collect()
    }
    
    /// Check if a component type has any patterns
    pub fn has_patterns(&self, component_type: ComponentType) -> bool {
        self.patterns.contains_key(&component_type) && !self.patterns[&component_type].is_empty()
    }
    
    /// Check if a component type has any negative patterns
    pub fn has_negative_patterns(&self, component_type: ComponentType) -> bool {
        self.negative_patterns.contains_key(&component_type) && !self.negative_patterns[&component_type].is_empty()
    }
}

impl ComponentPatternProvider for ComponentPatterns {
    fn get_patterns(&self, component_type: ComponentType) -> Vec<String> {
        self.patterns.get(&component_type).cloned().unwrap_or_default()
    }
    
    fn get_negative_patterns(&self, component_type: ComponentType) -> Vec<String> {
        self.negative_patterns.get(&component_type).cloned().unwrap_or_default()
    }
    
    fn set_patterns(&mut self, component_type: ComponentType, patterns: Vec<String>) {
        if patterns.is_empty() {
            self.patterns.remove(&component_type);
        } else {
            self.patterns.insert(component_type, patterns);
        }
    }
    
    fn set_negative_patterns(&mut self, component_type: ComponentType, patterns: Vec<String>) {
        if patterns.is_empty() {
            self.negative_patterns.remove(&component_type);
        } else {
            self.negative_patterns.insert(component_type, patterns);
        }
    }
    
    fn add_pattern(&mut self, component_type: ComponentType, pattern: String) {
        self.patterns.entry(component_type).or_insert_with(Vec::new).push(pattern);
    }
    
    fn add_negative_pattern(&mut self, component_type: ComponentType, pattern: String) {
        self.negative_patterns.entry(component_type).or_insert_with(Vec::new).push(pattern);
    }
}

