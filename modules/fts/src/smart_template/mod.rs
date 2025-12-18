//! Smart Template Module
//!
//! A trait-based, configurable system for track template management, naming conventions,
//! and track organization. Supports loading configurations from JSON/YAML and provides
//! extensible components through traits.

pub mod core;
pub mod naming;
pub mod template;
pub mod matching;
pub mod transform;
pub mod display;
pub mod helpers;
pub mod implementations;
pub mod config;
pub mod shared;
pub mod visibility_manager;

// Re-export commonly used types
pub use core::*;
pub use naming::*;
pub use template::*;
pub use matching::*;
pub use transform::*;
pub use display::*;
pub use helpers::*;
pub use implementations::*;
pub use config::*;
pub use shared::*;
pub use visibility_manager::*;
