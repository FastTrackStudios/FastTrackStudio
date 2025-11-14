//! Orchestral Instruments
//!
//! Traditional orchestral instruments organized by family

pub mod strings;
pub mod brass;
pub mod woodwinds;
pub mod harp;

pub use strings::StringInstrument;
pub use brass::BrassInstrument;
pub use woodwinds::WoodwindInstrument;
pub use harp::HarpInstrument;
