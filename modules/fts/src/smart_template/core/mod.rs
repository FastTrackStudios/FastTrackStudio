//! Core module
//!
//! Contains core types, traits, and engines for the smart template system.

pub mod template;
pub mod traits;
pub mod template_engine;
pub mod group_engine;
pub mod visibility_engine;

pub use template::*;
pub use traits::*;
pub use template_engine::*;
pub use group_engine::*;
pub use visibility_engine::*;
