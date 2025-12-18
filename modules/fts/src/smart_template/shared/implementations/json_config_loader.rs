//! JSON configuration loader
//!
//! Loads configurations from JSON files, compatible with the REAPER Lua script format.

use crate::smart_template::config::template_config::{TemplateConfig, InheritanceMode};
use crate::smart_template::shared::ConfigLoader;
use serde_json;
use std::fs;
use std::io;

/// Error type for JSON config loading
#[derive(Debug, thiserror::Error)]
pub enum JsonConfigLoaderError {
    #[error("IO error: {0}")]
    Io(#[from] io::Error),
    
    #[error("JSON parse error: {0}")]
    Json(#[from] serde_json::Error),
    
    #[error("Invalid source: {0}")]
    InvalidSource(String),
}

/// JSON configuration loader
///
/// Loads template configurations from JSON files, supporting the same format
/// as the REAPER Lua scripts' defaults.json.
pub struct JsonConfigLoader;

impl JsonConfigLoader {
    /// Create a new JSON config loader
    pub fn new() -> Self {
        Self
    }
}

impl Default for JsonConfigLoader {
    fn default() -> Self {
        Self::new()
    }
}

impl ConfigLoader for JsonConfigLoader {
    type Config = TemplateConfig;
    type Error = JsonConfigLoaderError;
    
    fn load(&self, source: &str) -> Result<Self::Config, Self::Error> {
        // Try to read as file path first
        let content = match fs::read_to_string(source) {
            Ok(content) => content,
            Err(_) => {
                // If file read fails, treat as JSON string
                source.to_string()
            }
        };
        
        let config: TemplateConfig = serde_json::from_str(&content)?;
        Ok(config)
    }
    
    fn load_with_inheritance(
        &self,
        defaults: &str,
        overrides: &str,
        mode: InheritanceMode,
    ) -> Result<Self::Config, Self::Error> {
        match mode {
            InheritanceMode::DefaultOnly => {
                self.load(defaults)
            }
            InheritanceMode::OverrideOnly => {
                self.load(overrides)
            }
            InheritanceMode::DefaultPlusOverride => {
                // Load both configs
                let mut default_config = self.load(defaults)?;
                let override_config = self.load(overrides)?;
                
                // Merge override into default
                default_config.merge(&override_config);
                
                Ok(default_config)
            }
        }
    }
    
    fn name(&self) -> &str {
        "json"
    }
}
