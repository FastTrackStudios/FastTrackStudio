//! Default block definitions — instrument-agnostic DSP building blocks.
//!
//! Each function returns a pre-configured [`Block`] with correct plugin
//! identity and tags. These are used as building blocks in module presets.

pub mod amp;
pub mod drive;
pub mod dummy;
pub mod dynamics;
pub mod eq;
pub mod master;
pub mod modulation;
pub mod motion;
pub mod source;
pub mod special;
pub mod time;
pub mod vocal;
