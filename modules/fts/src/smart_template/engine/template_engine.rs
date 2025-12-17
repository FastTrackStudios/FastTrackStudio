//! Template engine
//!
//! Main engine that orchestrates parsers, matchers, formatters, and template generators.

use std::collections::HashMap;
use crate::smart_template::template::TemplateConfig;
use crate::smart_template::traits::*;

/// Main template engine
///
/// This engine coordinates all the components (parser, matcher, formatter, generator)
/// to provide a complete template management system.
///
/// The engine uses trait objects to allow different implementations to be swapped in.
/// Components are optional and can be set using the builder pattern.
pub struct TemplateEngine {
    // Configuration
    config: TemplateConfig,
    
    // Generated templates
    templates: HashMap<String, Template>,
}

impl TemplateEngine {
    /// Create a new template engine with a configuration
    pub fn new(config: TemplateConfig) -> Self {
        Self {
            config,
            templates: HashMap::new(),
        }
    }
    
    /// Get the configuration
    pub fn config(&self) -> &TemplateConfig {
        &self.config
    }
    
    /// Get a mutable reference to the configuration
    pub fn config_mut(&mut self) -> &mut TemplateConfig {
        &mut self.config
    }
    
    /// Get a generated template by name
    pub fn get_template(&self, name: &str) -> Option<&Template> {
        self.templates.get(name)
    }
    
    /// Get all template names
    pub fn template_names(&self) -> Vec<String> {
        self.templates.keys().cloned().collect()
    }
}
