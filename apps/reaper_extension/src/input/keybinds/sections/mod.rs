//! Composable Action Set Sections
//!
//! Each section defines a group of related keybindings that can be mixed and matched
//! to create custom presets. Sections implement the [`ActionSet`] trait.
//!
//! ## Available Sections
//!
//! - [`scrolling`] - Scroll and zoom behaviors (mouse wheel, keyboard zoom)
//! - [`transport`] - Play, stop, record, navigation controls
//! - [`navigation`] - Cursor and track movement
//! - [`mouse_modifiers`] - Click+drag behaviors (edge resize, fades, etc.)
//! - [`editing`] - Cut, copy, paste, split, delete

pub mod editing;
pub mod mouse_modifiers;
pub mod navigation;
pub mod scrolling;
pub mod transport;

pub use editing::*;
pub use mouse_modifiers::*;
pub use navigation::*;
pub use scrolling::*;
pub use transport::*;
