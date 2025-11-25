//! Track Template Engine
//!
//! This crate provides a DAW-agnostic engine for matching track names to templates,
//! creating tracks, and managing track hierarchies with sends/receives.
//!
//! It uses the `naming-convention` crate to parse track names, but is independent
//! of any specific DAW implementation.

pub mod track;
pub mod track_list;
pub mod matcher;
pub mod template;

pub use track::{Track, Take, SendReceive, SortStatus};
pub use track_list::{TrackList, DisplayMode};
pub use matcher::TrackMatcher;
pub use template::Template;

