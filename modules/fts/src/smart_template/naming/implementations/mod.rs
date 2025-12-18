//! Naming implementations
//!
//! Default implementations of parser and formatter traits.

pub mod drum_kit;
pub mod bass;
pub mod guitar_electric;
pub mod guitar_acoustic;
pub mod keys;
pub mod synths;
pub mod vocals;

pub use drum_kit::*;
pub use bass::*;
pub use guitar_electric::*;
pub use guitar_acoustic::*;
pub use keys::*;
pub use synths::*;
pub use vocals::*;
