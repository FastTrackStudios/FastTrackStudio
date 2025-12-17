//! Core traits for extensibility
//!
//! These traits define the interfaces for all major operations, allowing
//! different implementations to be swapped in and out.

pub mod parser;
pub mod matcher;
pub mod formatter;
pub mod template_generator;
pub mod config_loader;
pub mod template_builder;

pub use parser::*;
pub use matcher::*;
pub use formatter::*;
pub use template_generator::*;
pub use config_loader::*;
pub use template_builder::*;
