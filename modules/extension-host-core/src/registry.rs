//! Extension Registry - Tracks loaded SHM guest plugins

use std::collections::HashMap;
use std::path::PathBuf;
use std::process::Child;

/// Metadata about a loaded plugin
#[derive(Debug, Clone)]
pub struct ExtensionMetadata {
    /// Extension name (derived from binary filename)
    pub name: String,

    /// Path to the plugin binary
    pub path: PathBuf,

    /// Assigned SHM peer ID
    pub peer_id: u8,
}

/// A running plugin guest process
pub struct ExtensionGuest {
    /// Extension metadata
    pub metadata: ExtensionMetadata,

    /// The spawned child process
    pub process: Child,
}

impl std::fmt::Debug for ExtensionGuest {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("ExtensionGuest")
            .field("metadata", &self.metadata)
            .field("process_pid", &self.process.id())
            .finish()
    }
}

/// Registry of all loaded plugins
#[derive(Debug)]
pub struct ExtensionRegistry {
    plugins: HashMap<String, ExtensionGuest>,
}

impl ExtensionRegistry {
    /// Create a new empty registry
    pub fn new() -> Self {
        Self {
            plugins: HashMap::new(),
        }
    }

    /// Register a plugin
    pub fn register(&mut self, name: String, guest: ExtensionGuest) {
        self.plugins.insert(name, guest);
    }

    /// Unregister a plugin by name, returning the guest if found
    pub fn unregister(&mut self, name: &str) -> Option<ExtensionGuest> {
        self.plugins.remove(name)
    }

    /// Get a plugin by name
    pub fn get(&self, name: &str) -> Option<&ExtensionGuest> {
        self.plugins.get(name)
    }

    /// Get a mutable reference to a plugin by name
    pub fn get_mut(&mut self, name: &str) -> Option<&mut ExtensionGuest> {
        self.plugins.get_mut(name)
    }

    /// List all plugin names
    pub fn list(&self) -> Vec<String> {
        self.plugins.keys().cloned().collect()
    }

    /// Get plugin count
    pub fn count(&self) -> usize {
        self.plugins.len()
    }
}

impl Default for ExtensionRegistry {
    fn default() -> Self {
        Self::new()
    }
}
