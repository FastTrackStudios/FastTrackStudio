//! Obsidian-style top-level view mode for the knowledge route.
//!
//! - **Edit** — default. Outliner with view/edit-per-block swap
//!   driven by vim's Normal/Insert.
//! - **View** — read-only render. No textareas anywhere. Vim
//!   navigation (`j/k/gg/G`) still moves the active block; insert
//!   commands are inert.
//! - **Source** — single textarea showing the full markdown source
//!   for the page (YAML frontmatter + serialized blocks). Edits
//!   are draft-only until the user clicks Apply.
//!
//! The active mode is shared across the route via a Dioxus
//! context provider.

use dioxus::prelude::*;

#[derive(Clone, Copy, Debug, Eq, PartialEq, Hash)]
pub enum ViewMode {
    Edit,
    View,
    Source,
}

impl Default for ViewMode {
    fn default() -> Self {
        Self::Edit
    }
}

impl ViewMode {
    pub fn label(self) -> &'static str {
        match self {
            ViewMode::Edit => "Edit",
            ViewMode::View => "View",
            ViewMode::Source => "Source",
        }
    }

    pub fn next(self) -> Self {
        match self {
            ViewMode::Edit => ViewMode::View,
            ViewMode::View => ViewMode::Source,
            ViewMode::Source => ViewMode::Edit,
        }
    }
}

/// Read the current view mode from context. Defaults to Edit if
/// no provider was set up (e.g., the component is rendered
/// outside the knowledge route).
pub fn use_view_mode() -> Signal<ViewMode> {
    try_use_context::<Signal<ViewMode>>().unwrap_or_else(|| use_signal(|| ViewMode::Edit))
}
