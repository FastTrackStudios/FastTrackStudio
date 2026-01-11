//! Keybind Configuration Persistence
//!
//! Handles saving/loading keybind configuration to/from disk.
//! Configuration is stored in REAPER's resource directory.

use super::{Keybind, KeybindContext};
use serde::{Deserialize, Serialize};
use std::path::PathBuf;

/// Keybind configuration that persists across sessions
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct KeybindConfig {
    /// Currently selected base preset name
    #[serde(default = "default_preset")]
    pub active_preset: String,

    /// Currently enabled override layers
    #[serde(default)]
    pub enabled_overrides: Vec<String>,

    /// User-specific binding overrides (applied on top of everything)
    #[serde(default)]
    pub user_bindings: Vec<Keybind>,
}

fn default_preset() -> String {
    "fastrackstudio".to_string()
}

impl Default for KeybindConfig {
    fn default() -> Self {
        Self {
            active_preset: default_preset(),
            enabled_overrides: Vec::new(),
            user_bindings: Vec::new(),
        }
    }
}

impl KeybindConfig {
    /// Get the config file path in REAPER's resource directory
    pub fn config_path() -> Option<PathBuf> {
        // Try to get REAPER's resource path
        let reaper = reaper_high::Reaper::get();
        let resource_path = reaper.medium_reaper().get_resource_path(|p| p.to_path_buf());

        let mut config_path = resource_path;
        config_path.push("FTS-Keybinds");
        config_path.push("config.json");

        Some(config_path.into())
    }

    /// Load configuration from disk, or return default if not found
    pub fn load() -> Self {
        let Some(path) = Self::config_path() else {
            tracing::warn!("Could not determine keybind config path, using defaults");
            return Self::default();
        };

        if !path.exists() {
            tracing::debug!("Keybind config not found at {:?}, using defaults", path);
            return Self::default();
        }

        match std::fs::read_to_string(&path) {
            Ok(content) => match serde_json::from_str(&content) {
                Ok(config) => {
                    tracing::info!("Loaded keybind config from {:?}", path);
                    config
                }
                Err(e) => {
                    tracing::error!("Failed to parse keybind config: {}", e);
                    Self::default()
                }
            },
            Err(e) => {
                tracing::error!("Failed to read keybind config: {}", e);
                Self::default()
            }
        }
    }

    /// Save configuration to disk
    pub fn save(&self) -> Result<(), ConfigError> {
        let path = Self::config_path().ok_or(ConfigError::NoResourcePath)?;

        // Ensure directory exists
        if let Some(parent) = path.parent() {
            std::fs::create_dir_all(parent).map_err(|e| ConfigError::Io(e.to_string()))?;
        }

        let content =
            serde_json::to_string_pretty(self).map_err(|e| ConfigError::Serialize(e.to_string()))?;

        std::fs::write(&path, content).map_err(|e| ConfigError::Io(e.to_string()))?;

        tracing::info!("Saved keybind config to {:?}", path);
        Ok(())
    }

    /// Add a user binding override
    pub fn add_user_binding(&mut self, binding: Keybind) {
        // Remove any existing binding for the same keys/context
        let context = binding.effective_context();
        self.user_bindings.retain(|b| {
            !(b.keys == binding.keys && b.effective_context() == context)
        });
        self.user_bindings.push(binding);
    }

    /// Remove a user binding
    pub fn remove_user_binding(&mut self, keys: &str, context: KeybindContext) {
        self.user_bindings
            .retain(|b| !(b.keys == keys && b.effective_context() == context));
    }
}

/// Configuration error types
#[derive(Debug, Clone)]
pub enum ConfigError {
    NoResourcePath,
    Io(String),
    Serialize(String),
}

impl std::fmt::Display for ConfigError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ConfigError::NoResourcePath => write!(f, "Could not determine REAPER resource path"),
            ConfigError::Io(e) => write!(f, "IO error: {}", e),
            ConfigError::Serialize(e) => write!(f, "Serialization error: {}", e),
        }
    }
}

impl std::error::Error for ConfigError {}
