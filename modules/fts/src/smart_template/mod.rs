//! Smart Template Module
//!
//! A trait-based, configurable system for track template management, naming conventions,
//! and track organization. Supports loading configurations from JSON/YAML and provides
//! extensible components through traits.

pub mod template;
pub mod group;
pub mod visibility_manager;
pub mod traits;
pub mod engine;
pub mod implementations;

// Re-export commonly used types
pub use template::*;
pub use group::*;
pub use visibility_manager::*;
pub use engine::*;
pub use traits::*;
pub use implementations::*;
