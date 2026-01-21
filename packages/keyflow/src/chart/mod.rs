//! Chart Module
//!
//! Complete chart parsing and representation

pub mod commands;
pub mod cues;
pub mod display;
pub mod dynamics;
pub mod index;
pub mod melody;
pub mod memory;
pub mod parser;
pub mod position;
pub mod semantic_role;
pub mod settings;
pub mod source_link;
pub mod structure;
pub mod templates;
pub mod track;
pub mod types;

#[macro_use]
pub mod r#macro;

pub use commands::Command;
pub use cues::{InstrumentGroup, TextCue};
pub use dynamics::DynamicMarking;
pub use index::{ChartIndex, ElementId};
pub use melody::{Melody, MelodyNote, MelodyVariables, OctaveModifier};
pub use memory::ChordMemory;
pub use position::ChartPosition;
pub use semantic_role::{NavigationType, SemanticRole};
pub use settings::{ChartSetting, ChartSettings, SettingValue};
pub use source_link::SourceLink;
pub use structure::Chart;
pub use templates::TemplateManager;
pub use track::{Track, TrackType};
pub use types::{
    ChartSection, ChordInstance, KeyChange, Measure, RhythmSlash, TempoChange, TimeSignatureChange,
};
