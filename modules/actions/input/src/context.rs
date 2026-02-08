//! Action context for when-clause evaluation.
//!
//! A lightweight context that keybindings can check against to decide
//! whether a binding is active (e.g., "only in insert mode" or
//! "only when panel is focused").

use std::collections::{HashMap, HashSet};

/// Execution context carrying tags (boolean flags) and variables (key-value pairs).
///
/// This is a local version of the `ActionContext` from `actions-proto`,
/// kept self-contained to avoid the upstream `roam-session` breakage.
#[derive(Debug, Clone, Default)]
pub struct ActionContext {
    tags: HashSet<String>,
    vars: HashMap<String, String>,
}

impl ActionContext {
    pub fn new() -> Self {
        Self::default()
    }

    /// Set a boolean tag (e.g., "panel:focused").
    pub fn set_tag(&mut self, tag: impl Into<String>) {
        self.tags.insert(tag.into());
    }

    /// Check whether a tag is present.
    pub fn has_tag(&self, tag: &str) -> bool {
        self.tags.contains(tag)
    }

    /// Set a variable (e.g., "mode" → "normal").
    pub fn set_var(&mut self, key: impl Into<String>, value: impl Into<String>) {
        self.vars.insert(key.into(), value.into());
    }

    /// Get a variable value.
    pub fn get_var(&self, key: &str) -> Option<&str> {
        self.vars.get(key).map(|s| s.as_str())
    }
}
