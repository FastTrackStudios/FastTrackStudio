//! Shared types and utilities for mouse modifier behaviors

pub mod traits;
pub mod behavior;
pub mod conversion;
pub mod macros;

// Re-export everything for convenience
pub use traits::{BehaviorId, BehaviorDisplay, MouseBehavior};
pub use behavior::MouseModifierBehavior;
pub use conversion::{get_behavior, get_mouse_modifier_name};
