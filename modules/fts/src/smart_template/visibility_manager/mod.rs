//! Visibility Manager module
//!
//! Contains visibility manager functionality including configuration and snapshot management.

pub mod traits;

pub use traits::*;

// Re-export from new locations for backward compatibility
pub use crate::smart_template::config::visibility_config::*;
pub use crate::smart_template::core::visibility_engine::*;
pub use crate::smart_template::implementations::visibility::*;
