//! Core domain types for setlist functionality
//!
//! This module contains the fundamental domain types and traits for managing
//! setlists, songs, and sections in a DAW environment.

mod setlist;
mod song;
mod section;
mod error;

pub use setlist::*;
pub use song::*;
pub use section::*;
pub use error::*;
