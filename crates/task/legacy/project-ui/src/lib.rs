//! Project feature UI.
//!
//! Single layer for the vertical-slice reset: a "live" wrapper
//! [`TasksByProjectLive`] that owns a local `CrdtDoc`, subscribes
//! to the server's `WorkspaceSync` stream, and renders tasks
//! grouped by project. Mutations (toggle done) commit locally and
//! propagate via `apply_update`. Mount with one prop:
//!
//! ```ignore
//! rsx! { project_ui::TasksByProjectLive { vox_url: my_url } }
//! ```

pub mod live;

pub use live::{Snapshot, TasksByProjectLive, TasksByProjectView};
