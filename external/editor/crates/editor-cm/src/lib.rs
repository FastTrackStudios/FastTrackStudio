//! Umbrella crate. Downstream apps depend on `editor` and get
//! the state + view surface from one place.

pub use editor_state::*;
pub use editor_typst;
pub use editor_view::{self, Editor};
pub use editor_vim;

/// Built-in commands. Re-exported as `editor::commands::*`.
pub mod commands {
    pub use editor_state::commands::*;
}
