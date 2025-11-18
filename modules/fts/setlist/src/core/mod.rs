//! Core domain types for setlist functionality
//!
//! This module contains the fundamental domain types and traits for managing
//! setlists, songs, and sections in a DAW environment.

mod error;
mod section;
mod setlist;
mod song;

pub use error::*;
pub use section::*;
pub use setlist::*;
pub use song::*;
