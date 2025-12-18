//! Integration tests entry point
//!
//! This file makes the integration subdirectory available as a test crate.
//! All integration tests are organized in the `integration/` subdirectory.

mod naming;
mod template;

// Re-export tests so they run
pub use naming::*;
pub use template::*;
