//! Tempo Map Override Layer
//!
//! Keybindings active during tempo mapping workflow.
//! Enable with `FTS_KEYBIND_TOGGLE_TEMPO_MAP_OVERLAY` action.

use crate::input::keybinds::{Keybind, KeybindOverride, WheelBind};

/// Get wheel bindings for the tempo map overlay
fn tempo_map_wheel_bindings() -> Vec<WheelBind> {
    let mut bindings = vec![
        // === Scroll with Wheel ===
        // Action 989: View: Scroll vertically (MIDI CC relative/mousewheel)
        WheelBind::new("", "989").with_description("Scroll view vertically"),
        // Shift + wheel / horizwheel = horizontal scroll reversed (action 977)
        // Action 977: View: Scroll horizontally reversed (MIDI CC relative/mousewheel)
        WheelBind::new("<S->", "977")
            .with_description("Scroll horizontally reversed (Shift+wheel)"),
        WheelBind::new("<S->", "977")
            .with_horizontal()
            .with_description("Scroll horizontally reversed (Shift+horizwheel)"),
        WheelBind::new("", "977")
            .with_horizontal()
            .with_description("Scroll horizontally reversed (horizwheel)"),
        // Option + wheel = vertical zoom reversed (action 1001)
        // Action 1001: View: Zoom vertically reversed (MIDI CC relative/mousewheel)
        WheelBind::new("<A->", "1001").with_description("Zoom vertically reversed (Option+wheel)"),
        WheelBind::new("<A->", "1001")
            .with_horizontal()
            .with_description("Zoom vertically reversed (Option+wheel, horizontal event)"),
        // Shift + Option + wheel = horizontal zoom reversed (action 979)
        // Action 979: View: Zoom horizontally reversed (MIDI CC relative/mousewheel)
        WheelBind::new("<S-A->", "979")
            .with_description("Zoom horizontally reversed (Shift+Option+wheel)"),
        WheelBind::new("<S-A->", "979")
            .with_horizontal()
            .with_description("Zoom horizontally reversed (Shift+Option+wheel)"),
    ];

    // Action 40111: View: Zoom horizontally (MIDI CC relative/mousewheel)
    // Cmd/Ctrl + wheel = horizontal zoom
    #[cfg(target_os = "macos")]
    bindings
        .push(WheelBind::new("<M->", "40111").with_description("Zoom horizontally (Cmd+wheel)"));

    #[cfg(not(target_os = "macos"))]
    bindings
        .push(WheelBind::new("<C->", "40111").with_description("Zoom horizontally (Ctrl+wheel)"));

    bindings
}

/// Create the Tempo Map override layer
pub fn tempo_map_override() -> KeybindOverride {
    KeybindOverride::new(
        "tempo_map",
        "Tempo mapping workflow keybindings - grid and transient manipulation",
    )
    .with_priority(100) // High priority to override base bindings
    .with_bindings(vec![
        // === Move Grid to Mouse ===
        Keybind::new("g", "FTS_TEMPO_MOVE_MEASURE_GRID_TO_MOUSE")
            .with_description("Move closest measure grid line to mouse cursor"),
        Keybind::new("<S-g>", "FTS_TEMPO_MOVE_MEASURE_GRID_TO_MOUSE_CONSTRAINED")
            .with_description("Move grid (constrained - anchors measure before)"),
        Keybind::new(
            "<A-g>",
            "FTS_TEMPO_MOVE_MEASURE_GRID_TO_MOUSE_FULLY_CONSTRAINED",
        )
        .with_description("Move grid (fully constrained - anchors both sides)"),
        // === Move Grid (any grid division) ===
        Keybind::new("<C-g>", "FTS_TEMPO_MOVE_GRID_TO_MOUSE")
            .with_description("Move closest grid line (any division) to mouse cursor"),
        // === Move Tempo Marker ===
        Keybind::new("<C-S-g>", "FTS_TEMPO_MOVE_MARKER_TO_MOUSE")
            .with_description("Move closest tempo marker to mouse cursor"),
        // === Snap to Transient ===
        Keybind::new("t", "FTS_TEMPO_SNAP_GRID_TO_TRANSIENT")
            .with_description("Snap closest measure grid line to next transient"),
        Keybind::new("<S-t>", "FTS_TEMPO_SNAP_GRID_TO_TRANSIENT_CONSTRAINED")
            .with_description("Snap to transient (constrained - anchors measure before)"),
        Keybind::new(
            "<A-t>",
            "FTS_TEMPO_SNAP_GRID_TO_TRANSIENT_FULLY_CONSTRAINED",
        )
        .with_description("Snap to transient (fully constrained - anchors both sides)"),
    ])
    .with_wheel_bindings(tempo_map_wheel_bindings())
}
