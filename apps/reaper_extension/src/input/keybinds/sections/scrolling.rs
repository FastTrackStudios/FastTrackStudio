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
            // === Horizontal Zoom ===
            Keybind::new("<plus>", "1012").with_description("Zoom in horizontal"),
            Keybind::new("=", "1012").with_description("Zoom in horizontal"),
            Keybind::new("<minus>", "1011").with_description("Zoom out horizontal"),
            Keybind::new("<kp_+>", "1012").with_description("Zoom in horizontal (numpad)"),
            Keybind::new("<kp_->", "1011").with_description("Zoom out horizontal (numpad)"),

            // === Vertical Zoom ===
            Keybind::new("<S-plus>", "40111").with_description("Zoom in vertical"),
            Keybind::new("<S-=>", "40111").with_description("Zoom in vertical"),
            Keybind::new("<S-minus>", "40112").with_description("Zoom out vertical"),
            Keybind::new("<C-up>", "40111").with_description("Zoom in vertical"),
            Keybind::new("<C-down>", "40112").with_description("Zoom out vertical"),

            // === Quick Zoom ===
            Keybind::new("z", "40295").with_description("Zoom to fit project in arrange"),
            Keybind::new("<C-kp_+>", "40295").with_description("Zoom time selection"),

            // === View Scroll ===
            Keybind::new("<C-home>", "40042").with_description("Scroll to start of project"),
            Keybind::new("<C-end>", "40043").with_description("Scroll to end of project"),
            Keybind::new("<C-pgup>", "40631").with_description("Scroll view up"),
            Keybind::new("<C-pgdn>", "40630").with_description("Scroll view down"),

            // === Track Height ===
            Keybind::new("<C-S-up>", "40110").with_description("Increase track height"),
            Keybind::new("<C-S-down>", "40113").with_description("Decrease track height"),
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
            // === Logic-style Zoom (Cmd+Arrow) ===
            Keybind::new("<D-right>", "1012").with_description("Zoom in horizontal"),
            Keybind::new("<D-left>", "1011").with_description("Zoom out horizontal"),
            Keybind::new("<D-up>", "40111").with_description("Zoom in vertical"),
            Keybind::new("<D-down>", "40112").with_description("Zoom out vertical"),

            // === Quick Zoom ===
            Keybind::new("z", "40295").with_description("Zoom to fit project in arrange"),
            Keybind::new("<M-z>", "40031").with_description("Zoom time selection"),
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
