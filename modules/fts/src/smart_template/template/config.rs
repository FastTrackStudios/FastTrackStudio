//! Template configuration
//!
//! Main configuration structure that contains groups and settings for the template engine.

use serde::{Deserialize, Serialize};
use crate::smart_template::group::GroupConfig;

/// Inheritance mode for configuration loading
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum InheritanceMode {
    /// Use only default configuration
    DefaultOnly,
    /// Use defaults plus user overrides (merge)
    DefaultPlusOverride,
    /// Use only user overrides
    OverrideOnly,
}

/// Template configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TemplateConfig {
    /// Template name
    pub name: String,
    
    /// Groups in this template
    pub groups: Vec<GroupConfig>,
    
    /// Default parser type name
    pub default_parser: Option<String>,
    
    /// Default matcher type name
    pub default_matcher: Option<String>,
    
    /// Default formatter type name
    pub default_formatter: Option<String>,
    
    /// Default template generator type name
    pub default_template_generator: Option<String>,
    
    /// Inheritance mode for loading configs
    pub inheritance_mode: Option<InheritanceMode>,
}

impl Default for TemplateConfig {
    fn default() -> Self {
        Self {
            name: "Default Template".to_string(),
            groups: Vec::new(),
            default_parser: Some("simple".to_string()),
            default_matcher: Some("track".to_string()),
            default_formatter: Some("default".to_string()),
            default_template_generator: Some("group".to_string()),
            inheritance_mode: Some(InheritanceMode::DefaultPlusOverride),
        }
    }
}

impl TemplateConfig {
    /// Merge another config into this one (for inheritance)
    pub fn merge(&mut self, other: &TemplateConfig) {
        // Merge groups (user overrides take precedence)
        for other_group in &other.groups {
            if let Some(existing_group) = self.groups.iter_mut()
                .find(|g| g.name == other_group.name) {
                // Merge group properties (user values override defaults)
                *existing_group = other_group.clone();
            } else {
                // Add new group
                self.groups.push(other_group.clone());
            }
        }
        
        // Override other settings if provided
        if other.default_parser.is_some() {
            self.default_parser = other.default_parser.clone();
        }
        if other.default_matcher.is_some() {
            self.default_matcher = other.default_matcher.clone();
        }
        if other.default_formatter.is_some() {
            self.default_formatter = other.default_formatter.clone();
        }
        if other.default_template_generator.is_some() {
            self.default_template_generator = other.default_template_generator.clone();
        }
        if other.inheritance_mode.is_some() {
            self.inheritance_mode = other.inheritance_mode;
        }
    }
}
