//! The Dioxus editor for the WGPU REAPER Demo plugin.
//!
//! This demonstrates a Dioxus UI with shared state between windowed and embedded editors.

use atomic_float::AtomicF32;
use nih_plug::prelude::Editor;
use nih_plug_dioxus::prelude::*;
use std::sync::atomic::Ordering;
use std::sync::Arc;

use crate::{DemoParams, SharedUiState};

/// Native CSS styles
const NATIVE_CSS: &str = r#"
* {
    box-sizing: border-box;
    margin: 0;
    padding: 0;
}

:root {
    --bg-primary: #1a1a1e;
    --bg-card: #242428;
    --bg-hover: #2a2a2e;
    --text-primary: #ffffff;
    --text-secondary: #a0a0a0;
    --accent: #3b82f6;
    --accent-hover: #2563eb;
    --danger: #ef4444;
    --danger-hover: #dc2626;
    --success: #22c55e;
    --border: #3a3a3e;
    --radius: 8px;
}

body, html {
    width: 100%;
    height: 100%;
    font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif;
    font-size: 14px;
    background: var(--bg-primary);
    color: var(--text-primary);
}

.container {
    width: 100%;
    height: 100%;
    display: flex;
    flex-direction: column;
    overflow: hidden;
}

.header {
    padding: 12px 16px;
    text-align: center;
    border-bottom: 1px solid var(--border);
    background: var(--bg-card);
}

.header h1 {
    font-size: 18px;
    font-weight: 600;
    margin-bottom: 4px;
}

.header p {
    font-size: 12px;
    color: var(--text-secondary);
}

.content {
    flex: 1;
    padding: 16px;
    overflow-y: auto;
    display: flex;
    flex-direction: column;
    gap: 16px;
}

.card {
    background: var(--bg-card);
    border: 1px solid var(--border);
    border-radius: var(--radius);
    padding: 16px;
}

.card h2 {
    font-size: 14px;
    font-weight: 600;
    margin-bottom: 12px;
}

.counter-display {
    font-size: 48px;
    font-weight: 700;
    text-align: center;
    color: var(--accent);
    margin-bottom: 12px;
}

.button-row {
    display: flex;
    gap: 8px;
    justify-content: center;
    flex-wrap: wrap;
}

.btn {
    padding: 8px 16px;
    border: none;
    border-radius: 6px;
    font-size: 14px;
    font-weight: 500;
    cursor: pointer;
}

.btn-primary {
    background: var(--accent);
    color: white;
}

.btn-primary:hover {
    background: var(--accent-hover);
}

.btn-danger {
    background: var(--danger);
    color: white;
}

.btn-danger:hover {
    background: var(--danger-hover);
}

.btn-outline {
    background: transparent;
    border: 1px solid var(--border);
    color: var(--text-primary);
}

.btn-outline:hover {
    background: var(--bg-hover);
}

.btn-secondary {
    background: var(--bg-hover);
    color: var(--text-primary);
}

.btn-secondary:hover {
    background: var(--border);
}

.toggle-row {
    display: flex;
    align-items: center;
    justify-content: space-between;
    margin-bottom: 12px;
}

.toggle-label {
    font-size: 14px;
}

.switch {
    width: 44px;
    height: 24px;
    background: var(--border);
    border-radius: 12px;
    position: relative;
    cursor: pointer;
}

.switch.on {
    background: var(--accent);
}

.switch-knob {
    width: 20px;
    height: 20px;
    background: white;
    border-radius: 50%;
    position: absolute;
    top: 2px;
    left: 2px;
}

.switch.on .switch-knob {
    left: 22px;
}

.checkbox-row {
    display: flex;
    align-items: center;
    gap: 8px;
    margin-bottom: 8px;
}

.checkbox {
    width: 18px;
    height: 18px;
    border: 2px solid var(--border);
    border-radius: 4px;
    cursor: pointer;
    display: flex;
    align-items: center;
    justify-content: center;
}

.checkbox.checked {
    background: var(--accent);
    border-color: var(--accent);
}

.checkbox.checked::after {
    content: "✓";
    color: white;
    font-size: 12px;
    font-weight: bold;
}

.status-text {
    font-size: 12px;
    color: var(--text-secondary);
    margin-top: 8px;
}

.progress-bar {
    width: 100%;
    height: 8px;
    background: var(--border);
    border-radius: 4px;
    overflow: hidden;
    margin-bottom: 12px;
}

.progress-fill {
    height: 100%;
    background: var(--accent);
}

.footer {
    padding: 8px 16px;
    text-align: center;
    font-size: 12px;
    color: var(--text-secondary);
    border-top: 1px solid var(--border);
}
"#;

/// Creates the default editor state with a fixed size (16:9 aspect ratio).
pub fn default_state() -> Arc<DioxusState> {
    DioxusState::new(|| (640, 360))
}

/// Creates the Dioxus editor for the plugin.
pub fn create(
    _params: Arc<DemoParams>,
    _peak_meter: Arc<AtomicF32>,
    editor_state: Arc<DioxusState>,
    ui_state: Arc<SharedUiState>,
) -> Option<Box<dyn Editor>> {
    create_dioxus_editor_with_state(editor_state, ui_state, App)
}

/// Size presets (width, height)
/// Note: Max logical size ~2048 to stay within Vello's 4096 physical pixel limit at 2x scale
const SIZE_PRESETS: &[(u32, u32)] = &[
    (320, 180),   // Mini
    (480, 270),   // Small
    (640, 360),   // Medium (default)
    (960, 540),   // Large
    (1280, 720),  // Very Large
    (1600, 900),  // Extra Large
    (1920, 1080), // Full HD
];

/// Size preset names
const SIZE_NAMES: &[&str] = &[
    "Mini",
    "Small",
    "Medium",
    "Large",
    "Very Large",
    "Extra Large",
    "Full HD",
];

/// Scale options (percentage)
const SCALE_OPTIONS: &[u32] = &[100, 125, 150, 175, 200, 225, 250, 275, 300];

/// The main app component with dropdowns
#[component]
pub fn App() -> Element {
    // Get DioxusState for resizing
    let dioxus_state = try_use_context::<Arc<DioxusState>>();

    // State for dropdowns
    let mut selected_size = use_signal(|| 2usize); // Medium (index 2)
    let mut selected_scale = use_signal(|| 0usize); // 100% (index 0)

    // Clone state for closures
    let state_for_size = dioxus_state.clone();
    let state_for_scale = dioxus_state.clone();

    // Handle size change
    let on_size_change = move |idx: usize| {
        selected_size.set(idx);
        if let Some(state) = &state_for_size {
            let (base_w, base_h) = SIZE_PRESETS[idx];
            let scale = SCALE_OPTIONS[*selected_scale.read()] as f64 / 100.0;
            let new_w = (base_w as f64 * scale) as u32;
            let new_h = (base_h as f64 * scale) as u32;
            state.request_resize(new_w, new_h);
        }
    };

    // Handle scale change
    let on_scale_change = move |idx: usize| {
        selected_scale.set(idx);
        if let Some(state) = &state_for_scale {
            let (base_w, base_h) = SIZE_PRESETS[*selected_size.read()];
            let scale = SCALE_OPTIONS[idx] as f64 / 100.0;
            let new_w = (base_w as f64 * scale) as u32;
            let new_h = (base_h as f64 * scale) as u32;
            state.request_resize(new_w, new_h);
        }
    };

    // Build scale option strings
    let scale_strings: Vec<String> = SCALE_OPTIONS.iter().map(|s| format!("{}%", s)).collect();

    // Get current size for display
    let current_size = dioxus_state
        .as_ref()
        .map(|s| s.size())
        .unwrap_or((640, 360));

    rsx! {
        div {
            style: "width: 100%; height: 100%; background: #1a1a1a; display: flex; flex-direction: column; padding: 20px; gap: 16px; font-family: system-ui, -apple-system, sans-serif;",

            // Header
            div {
                style: "color: white; font-size: 18px; font-weight: 600;",
                "WGPU Demo"
            }

            // Current size display
            div {
                style: "color: #888; font-size: 13px;",
                "Current size: {current_size.0} x {current_size.1}"
            }

            // Dropdowns in a row
            div {
                style: "display: flex; gap: 16px; align-items: flex-start;",

                Dropdown {
                    options: SIZE_NAMES.to_vec(),
                    selected: *selected_size.read(),
                    on_change: on_size_change,
                    label: "Window Size",
                    width: "140px",
                }

                Dropdown {
                    options: scale_strings.clone(),
                    selected: *selected_scale.read(),
                    on_change: on_scale_change,
                    label: "Scale",
                    width: "100px",
                }
            }

            // Spacer
            div { style: "flex: 1;", }

            // Resize handle in bottom-right corner
            ResizeHandle {
                min_width: 320,
                min_height: 180,
                size: 20,
                color: "rgba(255, 255, 255, 0.4)",
            }
        }
    }
}
