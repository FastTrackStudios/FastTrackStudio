//! # REAPER Types
//!
//! REAPER-specific data structures and types for the RPP file format.
//! These modules provide strongly-typed representations of REAPER's
//! data structures, built on top of the generic parsing primitives.
//!
//! ## Modules
//!
//! - **`track`**: Track data structures and parsing
//! - **`item`**: Media item data structures and parsing
//! - **`envelope`**: Envelope data structures and parsing
//! - **`fx_chain`**: FX chain data structures and parsing
//! - **`project`**: REAPER project data structures and parsing
//!
//! ## Architecture
//!
//! These types form the domain layer of RPP parsing:
//! 1. **Primitives** provide generic RPP parsing
//! 2. **Types** provide REAPER-specific data structures
//! 3. **Conversion** from primitives to types happens here
//!
//! ## Example
//!
//! ```rust
//! use rpp_parser::{parse_rpp_file, ReaperProject};
//!
//! fn main() -> Result<(), Box<dyn std::error::Error>> {
//!     let rpp_content = r#"<REAPER_PROJECT 0.1 "6.75/linux-x86_64" 1681651369
//!       <TRACK
//!         NAME "Track 1"
//!         VOL 1.0 0.0
//!       >
//!     >"#;
//!
//!     let project = parse_rpp_file(rpp_content)?;
//!     let reaper_project = ReaperProject::from_rpp_project(&project)?;
//!     // Now you have strongly-typed REAPER data structures!
//!     Ok(())
//! }
//! ```

pub mod track;
pub mod item;
pub mod envelope;
pub mod fx_chain;
pub mod project;
pub mod marker_region;
pub mod time_tempo;
pub mod time_pos_utils;

// Re-export the main types for convenience
pub use track::Track;
pub use item::Item;
pub use envelope::Envelope;
pub use fx_chain::FxChain;
pub use project::ReaperProject;
pub use marker_region::{MarkerRegion, MarkerRegionCollection};
pub use time_tempo::{TempoTimePoint, TempoTimeEnvelope};
pub use time_pos_utils::{
    time_to_beat_position,
    time_to_beat_position_structured,
    time_to_beat_position_with_envelope,
    time_to_beat_position_structured_with_envelope,
};
