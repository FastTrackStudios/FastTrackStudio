// Core modules
pub mod parser;
pub mod chart;
pub mod types;

// Trait modules
pub mod metadata_parser;
pub mod section_parser;
pub mod content_parser;
pub mod chord_resolver;
pub mod transposition;

// Helper modules
pub mod element_parser;
pub mod chord_memory;

// Re-export commonly used types from types module
pub use types::{ChartSection, Measure, ChordInstance, RootNote, KeyChange, Position};

// Re-export Chart from chart module
pub use chart::Chart;

// Re-export traits
pub use metadata_parser::MetadataParser;
pub use section_parser::SectionParser;
pub use content_parser::ContentParser;
pub use chord_resolver::ChordResolver;
pub use transposition::Transposition;

// Re-export helper types
pub use element_parser::{ChartElementParser, ChartElement};
pub use chord_memory::ChordMemory;
