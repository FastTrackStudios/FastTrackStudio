//! Scrolling and Zoom Action Sets
//!
//! Defines scroll and zoom behaviors for different profiles.

use crate::input::keybinds::{ActionSet, Keybind, WheelBind};

/// Standard REAPER scrolling behavior
pub struct ReaperScrolling;

impl ActionSet for ReaperScrolling {
    fn name(&self) -> &'static str {
        "ReaperScrolling"
    }

    fn wheel_binds(&self) -> Vec<WheelBind> {
        let mut bindings = vec![
            // Vertical scroll with wheel (no modifiers)
            WheelBind::new("", "989").with_description("Scroll view vertically"),
            // Horizontal scroll with Shift+wheel
            WheelBind::new("<S->", "40140").with_description("Scroll horizontally (Shift+wheel)"),
        ];

        // Zoom with Ctrl/Cmd+wheel
        #[cfg(target_os = "macos")]
        bindings.push(
            WheelBind::new("<M->", "40111").with_description("Zoom horizontally (Cmd+wheel)"),
        );

        #[cfg(not(target_os = "macos"))]
        bindings.push(
            WheelBind::new("<C->", "40111").with_description("Zoom horizontally (Ctrl+wheel)"),
        );

        bindings
    }

    fn keybinds(&self) -> Vec<Keybind> {
        vec![
            Keybind::new("<plus>", "1012").with_description("Zoom in horizontal"),
            Keybind::new("<minus>", "1011").with_description("Zoom out horizontal"),
            Keybind::new("<S-plus>", "40111").with_description("Zoom in vertical"),
            Keybind::new("<S-minus>", "40112").with_description("Zoom out vertical"),
            Keybind::new("z", "40031").with_description("Zoom to fit project"),
        ]
    }
}

/// Logic Pro style scrolling (Option+wheel for zoom)
pub struct LogicScrolling;

impl ActionSet for LogicScrolling {
    fn name(&self) -> &'static str {
        "LogicScrolling"
    }

    fn wheel_binds(&self) -> Vec<WheelBind> {
        vec![
            // Vertical scroll with wheel
            WheelBind::new("", "989").with_description("Scroll view vertically"),
            // Horizontal scroll with Shift+wheel
            WheelBind::new("<S->", "40140").with_description("Scroll horizontally"),
            // Zoom with Option+wheel (Logic style)
            WheelBind::new("<A->", "40111").with_description("Zoom horizontally (Option+wheel)"),
        ]
    }

    fn keybinds(&self) -> Vec<Keybind> {
        vec![
            // Logic uses Cmd+arrow for zoom
            Keybind::new("z", "40031").with_description("Zoom to fit project"),
        ]
    }
}

/// FastTrackStudio scrolling (vim-inspired + standard wheel)
pub struct FtsScrolling;

impl ActionSet for FtsScrolling {
    fn name(&self) -> &'static str {
        "FtsScrolling"
    }

    fn wheel_binds(&self) -> Vec<WheelBind> {
        let mut bindings = vec![
            WheelBind::new("", "989").with_description("Scroll view vertically"),
            WheelBind::new("<S->", "40140").with_description("Scroll horizontally (Shift+wheel)"),
        ];

        #[cfg(target_os = "macos")]
        bindings.push(
            WheelBind::new("<M->", "40111").with_description("Zoom horizontally (Cmd+wheel)"),
        );

        #[cfg(not(target_os = "macos"))]
        bindings.push(
            WheelBind::new("<C->", "40111").with_description("Zoom horizontally (Ctrl+wheel)"),
        );

        bindings
    }

    fn keybinds(&self) -> Vec<Keybind> {
        vec![
            Keybind::new("<plus>", "1012").with_description("Zoom in horizontal"),
            Keybind::new("<minus>", "1011").with_description("Zoom out horizontal"),
            Keybind::new("<S-plus>", "40111").with_description("Zoom in vertical"),
            Keybind::new("<S-minus>", "40112").with_description("Zoom out vertical"),
            Keybind::new("z", "40031").with_description("Zoom to fit project"),
        ]
    }
}
