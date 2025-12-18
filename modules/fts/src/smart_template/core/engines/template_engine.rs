//! Template engine
//!
//! Main engine for template operations.

use std::collections::HashMap;
use crate::smart_template::core::models::template_config::TemplateConfig;
use crate::smart_template::core::models::template::Template;

/// Main template engine
///
/// This engine coordinates template generation, matching, and building operations.
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
