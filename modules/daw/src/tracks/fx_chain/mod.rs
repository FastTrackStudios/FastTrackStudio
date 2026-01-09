//! FX chain data structures for REAPER

use serde::{Deserialize, Serialize};
use std::fmt;

/// A REAPER FX chain
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct FxChain {
    pub name: String,
    pub plugins: Vec<Plugin>,
}

/// A plugin in an FX chain
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Plugin {
    pub name: String,
    pub plugin_type: String,
    pub enabled: bool,
    pub parameters: Vec<f64>,
}

impl fmt::Display for FxChain {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "FX Chain: {}", self.name)?;
        writeln!(f, "  Plugins: {}", self.plugins.len())?;
        Ok(())
    }
}

impl fmt::Display for Plugin {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "Plugin: {}", self.name)?;
        writeln!(f, "  Type: {}, Enabled: {}", self.plugin_type, self.enabled)?;
        writeln!(f, "  Parameters: {}", self.parameters.len())?;
        Ok(())
    }
}
