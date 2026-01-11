//! Editing Action Sets
//!
//! Defines cut, copy, paste, split, delete, and other editing commands.

use crate::input::keybinds::{ActionSet, Keybind};

/// Standard REAPER editing (Ctrl+X/C/V)
pub struct ReaperEditing;

impl ActionSet for ReaperEditing {
    fn name(&self) -> &'static str {
        "ReaperEditing"
    }

    fn keybinds(&self) -> Vec<Keybind> {
        // Minimal - let REAPER handle Ctrl+X/C/V defaults
        vec![]
    }
}

/// Logic Pro style editing
pub struct LogicEditing;

impl ActionSet for LogicEditing {
    fn name(&self) -> &'static str {
        "LogicEditing"
    }

    fn keybinds(&self) -> Vec<Keybind> {
        vec![
            // Standard Cmd+X/C/V (handled by REAPER)
            // T = Split at playhead (Logic style)
            Keybind::new("t", "40757").with_description("Split items at edit cursor"),
            // Delete = Delete selected
            Keybind::new("<delete>", "40006").with_description("Delete selected items"),
            Keybind::new("<backspace>", "40006").with_description("Delete selected items"),
        ]
    }
}

/// FastTrackStudio editing (vim-inspired)
pub struct FtsEditing;

impl ActionSet for FtsEditing {
    fn name(&self) -> &'static str {
        "FtsEditing"
    }

    fn keybinds(&self) -> Vec<Keybind> {
        vec![
            // Vim-style editing
            Keybind::new("s", "40757").with_description("Split items at cursor"),
            Keybind::new("d", "40006").with_description("Delete selected items"),
            Keybind::new("x", "40059").with_description("Cut selected items"),
            Keybind::new("c", "40060").with_description("Copy selected items"),
            Keybind::new("p", "40058").with_description("Paste items"),
            Keybind::new("u", "40029").with_description("Undo"),
            Keybind::new("<C-r>", "40030").with_description("Redo"),
            // Selection mode toggle
            Keybind::new("v", "40769").with_description("Toggle item selection follows cursor"),
            // Track management
            Keybind::new("n", "40001").with_description("Insert new track"),
            Keybind::new("<S-n>", "40702").with_description("Duplicate tracks"),
        ]
    }
}
