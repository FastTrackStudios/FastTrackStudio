//! Dioxus components. Mirrors `frontend.components.*`.

mod block_node;
mod editable;
mod page_view;
mod shell;
mod sidebar;
mod slash_menu;

pub use shell::EditorApp;

// Re-exports for downstream use / tests.
pub use block_node::BlockNode;
pub use editable::EditableBlock;
pub use page_view::PageView;
pub use sidebar::Sidebar;
pub use slash_menu::{SlashMenu, move_selection as slash_move, run_selected as slash_run};
