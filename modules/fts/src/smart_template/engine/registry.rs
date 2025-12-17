//! Component registry
//!
//! Registry for managing and creating parser, matcher, formatter, and generator instances.
//!
//! Note: The registry uses type erasure through trait objects. For now, this is a placeholder
//! structure. Actual implementations will need to handle the generic types properly, possibly
//! using a different approach or concrete types.

use std::collections::HashMap;

/// Component registry for managing component instances
///
/// This is a placeholder structure. The actual implementation will depend on
/// how we handle the generic types in the traits. For now, this provides
/// a structure that can be extended as we implement concrete components.
#[derive(Default)]
pub struct ComponentRegistry {
    // Component names registered
    parser_names: Vec<String>,
    matcher_names: Vec<String>,
    formatter_names: Vec<String>,
    generator_names: Vec<String>,
}

impl ComponentRegistry {
    /// Create a new empty registry
    pub fn new() -> Self {
        Self::default()
    }
    
    /// Register a parser name
    pub fn register_parser_name(&mut self, name: String) {
        if !self.parser_names.contains(&name) {
            self.parser_names.push(name);
        }
    }
    
    /// Register a matcher name
    pub fn register_matcher_name(&mut self, name: String) {
        if !self.matcher_names.contains(&name) {
            self.matcher_names.push(name);
        }
    }
    
    /// Register a formatter name
    pub fn register_formatter_name(&mut self, name: String) {
        if !self.formatter_names.contains(&name) {
            self.formatter_names.push(name);
        }
    }
    
    /// Register a generator name
    pub fn register_generator_name(&mut self, name: String) {
        if !self.generator_names.contains(&name) {
            self.generator_names.push(name);
        }
    }
    
    /// Get registered parser names
    pub fn parser_names(&self) -> &[String] {
        &self.parser_names
    }
    
    /// Get registered matcher names
    pub fn matcher_names(&self) -> &[String] {
        &self.matcher_names
    }
    
    /// Get registered formatter names
    pub fn formatter_names(&self) -> &[String] {
        &self.formatter_names
    }
    
    /// Get registered generator names
    pub fn generator_names(&self) -> &[String] {
        &self.generator_names
    }
}
