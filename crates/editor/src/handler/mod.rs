//! User-facing actions. Mirrors Logseq's `frontend.handler.*`
//! namespaces (`editor`, `block`, `page`, ...). Every public fn
//! here is the operation a UI component invokes — they take
//! `AppState` plus the action's args, mutate the in-memory
//! `Vault`, and write the affected page back to disk.

pub mod block;
pub mod editor;
pub mod page;
pub mod persist;

pub use block::{delete_block, indent_block, outdent_block, split_block, update_block_content};
pub use editor::{enter_edit, exit_edit};
pub use page::{create_page, open_page};
pub use persist::save_page;
