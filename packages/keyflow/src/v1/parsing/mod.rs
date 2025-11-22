//! Musical parsing infrastructure
//! 
//! This module provides a unified parsing system for different musical elements
//! with shared infrastructure and type-specific parsers.

pub mod common;
pub mod parser_protocol;
pub mod chord;
pub mod key;
pub mod scale;

// Re-export common types
pub use common::{ParserError, ParserErrors, CommonParserError};
pub use parser_protocol::{ParseResult, MiniParser};

// Re-export specific parsers
pub use chord::{ChordMiniParser, ChordParserError};
pub use key::{KeyParser, KeyParserError, KeySignatureMiniParser};
pub use scale::{ScaleParser, ScaleParserError};
