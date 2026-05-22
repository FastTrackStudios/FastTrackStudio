//! User-facing actions. Mirrors Logseq's `frontend.handler.*`
//! namespaces (`editor`, `block`, `page`, ...). Every public fn
//! here is the operation a UI component invokes — they take
//! `AppState` plus the action's args, mutate the in-memory
//! `Vault`, and write the affected page back to disk.

pub mod block;
pub mod commands;
pub mod editor;
pub mod format;
pub mod page;
pub mod persist;

pub use block::{
    delete_block, indent_block, move_block_down, move_block_up, next_block_in_outline,
    outdent_block, prev_block_in_outline, split_block, update_block_content,
};
pub use commands::{
    CommandEntry, CommandKind, CommandResult, DateRef, TaskMarker, all_commands, cycle_task_marker,
    filter as filter_commands, leading_task_marker, run_command, set_heading_level,
    set_task_marker, strip_heading_marker, strip_task_marker,
};
pub use editor::{enter_edit, exit_edit};
pub use format::{FormatResult, format_link, format_text};
pub use page::{create_page, open_page};
pub use persist::save_page;
