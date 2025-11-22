//! Sections Module
//!
//! Song section management and numbering

pub mod numbering;
pub mod section;
pub mod section_type;

pub use numbering::SectionNumberer;
pub use section::Section;
pub use section_type::SectionType;
