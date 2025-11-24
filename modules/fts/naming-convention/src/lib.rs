//! FTS Naming Convention domain module
//! 
//! This module defines the domain model for the FTS naming convention used for
//! audio tracks and files. The naming convention supports structured metadata
//! including instrument groups, performers, sections, and more.
//! 
//! This is a pure domain model - parsing logic is provided by other crates
//! (like fts-naming-parser).

pub mod track_name;
pub mod component;
pub mod component_patterns;
pub mod multi_mic_descriptor;
pub mod group;
pub mod source;
pub mod formatter;
pub mod component_order;
pub mod track_structure;
pub mod simple_parser;

#[macro_use]
pub mod macros;

#[cfg(feature = "default-groups")]
pub mod default_groups;

pub use track_name::TrackName;
pub use component::ComponentType;
pub use group::{Group, GroupType, FullGroup};
pub use component_patterns::{ComponentPatternProvider, ComponentPatterns};
pub use multi_mic_descriptor::MultiMicDescriptor;
pub use source::NamingConventionSource;
pub use formatter::{format_track_name, format_track_name_default};
pub use component_order::{ComponentOrder, ComponentOrderType};
pub use track_structure::TrackStructure;
pub use simple_parser::SimpleParser;

#[cfg(feature = "default-groups")]
pub use default_groups::{create_default_groups, create_kick_group, create_kick_track_structure, create_tom_group, create_tom_track_structure, create_cymbals_group, create_cymbals_track_structure, create_drums_group, create_drums_track_structure, create_snare_group, create_snare_track_structure, create_rooms_group, create_rooms_track_structure, create_bass_group, create_bass_track_structure, create_guitar_electric_group, create_guitar_acoustic_group, create_keys_group, create_synths_group, create_vocals_group};

