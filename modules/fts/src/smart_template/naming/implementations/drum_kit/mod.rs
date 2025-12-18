//! Drum kit naming implementations
//!
//! Contains all drum-related naming implementations using ItemProperties for parsing.
//! All parsing logic is handled by ItemPropertiesParser - these implementations just
//! validate and extract group-specific information.
//!
//! Groups:
//! - **Kick**: Multi-mic descriptors (In, Out, Trig, Sub, Ambient), arrangement patterns (Thump, Click, etc.)
//! - **Snare**: Multi-mic descriptors (Top, Bottom, Trig), effect patterns (Verb, Reverb)
//! - **Tom**: Increment-based naming (Tom 1, Tom 2), sub-types (Rack, Floor, Hi, Mid, Low)
//! - **Cymbals**: Sub-types (Hi Hat, Ride, Overheads)
//! - **Room**: Multi-mic descriptors (Short, Far, Mono)
//!
//! All implementations:
//! - Use `ItemPropertiesParser` for parsing
//! - Return `ItemProperties` as output
//! - Use `ItemPropertiesFormatter` for formatting (no group-specific formatters needed)
//! - Define group-specific patterns in `GroupConfig` (multi-mic descriptors, component patterns)

pub mod kick;
pub mod snare;
pub mod tom;
pub mod cymbals;
pub mod room;

pub use kick::*;
pub use snare::*;
pub use tom::*;
pub use cymbals::*;
pub use room::*;
