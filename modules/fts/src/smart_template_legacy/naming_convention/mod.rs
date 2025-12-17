//! FTS naming convention domain model
//!
//! Provides track and file naming conventions.

pub mod component;
pub mod component_order;
pub mod component_patterns;
pub mod formatter;
pub mod group;
pub mod group_track_list;
#[macro_use]
pub mod macros;
pub mod multi_mic_descriptor;
pub mod performers;
pub mod simple_parser;
pub mod source;
pub mod track_name;
pub mod track_structure;
pub mod default_groups;

pub use component::*;
pub use component_order::*;
pub use component_patterns::*;
pub use formatter::*;
pub use group::*;
pub use group_track_list::*;
pub use multi_mic_descriptor::*;
pub use performers::*;
pub use simple_parser::*;
pub use source::*;
pub use track_name::*;
pub use track_structure::*;

