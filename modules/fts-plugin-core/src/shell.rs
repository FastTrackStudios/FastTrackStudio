//! Plugin shell/wrapper component with top bar and size controls.

use nih_plug::nih_log;
use nih_plug_dioxus::TAILWIND_CSS;
use nih_plug_dioxus::dioxus_native::prelude::*;
use nih_plug_dioxus::prelude::*;
use std::sync::Arc;

/// Size preset names displayed in the dropdown.
pub const SIZE_PRESET_NAMES: &[&str] = &[
    "Tiny",
    "Small",
    "Medium",
    "Large",
    "Very Large",
    "Fullscreen",
];

/// Size preset dimensions (width, height).
pub const SIZE_PRESETS: &[(u32, u32)] = &[
    (320, 180),   // Tiny
    (480, 270),   // Small
    (640, 360),   // Medium (default)
    (960, 540),   // Large
    (1280, 720),  // Very Large
    (1920, 1080), // Fullscreen
];

/// Default size preset index (Medium).
pub const DEFAULT_SIZE_INDEX: usize = 2;

/// Top bar height in pixels.
pub const TOP_BAR_HEIGHT: u32 = 36;

// CSS for the shell - using explicit positioning like the working Blitz examples
const SHELL_CSS: &str = r#"
* {
    box-sizing: border-box;
    margin: 0;
    padding: 0;
}

html, body {
    width: 100%;
    height: 100%;
    overflow: hidden;
}

.shell-root {
    width: 100%;
    height: 100%;
    display: flex;
    flex-direction: column;
    background-color: #09090b;
    font-family: system-ui, sans-serif;
    position: relative;
}

.top-bar {
    height: 36px;
    min-height: 36px;
    background-color: #18181b;
    border-bottom: 1px solid #27272a;
    display: flex;
    align-items: center;
    padding: 0 12px;
    justify-content: space-between;
    user-select: none;
    position: relative;
    z-index: 100;
}

.top-bar-left {
    display: flex;
    align-items: center;
    gap: 8px;
}

.plugin-name {
    color: white;
    font-weight: 600;
    font-size: 14px;
}

.plugin-version {
    color: #71717a;
    font-size: 12px;
}

.top-bar-right {
    display: flex;
    align-items: center;
    gap: 8px;
}

.size-buttons {
    display: flex;
    align-items: center;
    gap: 2px;
    background-color: #27272a;
    border-radius: 4px;
    padding: 2px;
}

.size-btn {
    padding: 4px 8px;
    font-size: 12px;
    font-weight: 500;
    border-radius: 3px;
    cursor: pointer;
    color: #a1a1aa;
    background: transparent;
}

.size-btn:hover {
    color: #e4e4e7;
    background-color: #3f3f46;
}

.size-btn.selected {
    background-color: #2563eb;
    color: white;
}

.dropdown-container {
    position: relative;
}

.dropdown-trigger {
    display: flex;
    align-items: center;
    gap: 6px;
    padding: 4px 10px;
    background-color: #27272a;
    border: 1px solid #3f3f46;
    border-radius: 4px;
    color: #d4d4d8;
    font-size: 12px;
    font-weight: 500;
    cursor: pointer;
}

.dropdown-trigger:hover {
    background-color: #3f3f46;
}

.dropdown-arrow {
    color: #71717a;
}

/* Dropdown menu - absolutely positioned with explicit z-index */
.dropdown-menu {
    position: absolute;
    top: 100%;
    right: 0;
    margin-top: 4px;
    background-color: #27272a;
    border: 1px solid #3f3f46;
    border-radius: 6px;
    min-width: 150px;
    padding: 4px;
    z-index: 1000;
    width: 160px;
}

.dropdown-item {
    padding: 8px 12px;
    cursor: pointer;
    border-radius: 4px;
    color: #d4d4d8;
}

.dropdown-item:hover {
    background-color: #3f3f46;
}

.dropdown-item.selected {
    background-color: #2563eb;
    color: white;
}

.dropdown-item-name {
    font-size: 14px;
    font-weight: 500;
}

.dropdown-item-size {
    font-size: 11px;
    color: #71717a;
    margin-top: 2px;
}

.dropdown-item.selected .dropdown-item-size {
    color: #93c5fd;
}

.content-area {
    flex: 1;
    overflow: hidden;
}

/* Dropdown overlay - covers the entire screen to catch clicks outside */
.dropdown-overlay {
    position: absolute;
    top: 0;
    left: 0;
    right: 0;
    bottom: 0;
    z-index: 999;
    background: transparent;
}

/* Dropdown menu at root level - positioned in top-right */
.dropdown-menu-root {
    position: absolute;
    top: 40px;
    right: 12px;
    background-color: #27272a;
    border: 1px solid #3f3f46;
    border-radius: 6px;
    min-width: 150px;
    padding: 4px;
    z-index: 1000;
    width: 160px;
}
"#;

/// Helper to request resize via DioxusState from context
fn request_resize(idx: usize) {
    let (width, height) = SIZE_PRESETS[idx];
    if let Some(state) = try_use_context::<Arc<DioxusState>>() {
        state.request_resize(width, height);
    }
}

/// Plugin shell/wrapper component.
#[component]
pub fn PluginShell(
    plugin_name: String,
    #[props(default = None)] version: Option<String>,
    #[props(default = DEFAULT_SIZE_INDEX)] initial_size: usize,
    children: Element,
) -> Element {
    let mut selected_size = use_signal(|| initial_size);
    let mut dropdown_open = use_signal(|| false);

    let version_display = version.map(|v| format!("v{}", v)).unwrap_or_default();
    let is_open = *dropdown_open.read();
    let current_size = *selected_size.read();

    rsx! {
        style { {TAILWIND_CSS} }
        style { {SHELL_CSS} }

        div {
            class: "shell-root",

            // Top bar
            div {
                class: "top-bar",

                // Left side
                div {
                    class: "top-bar-left",
                    span { class: "plugin-name", "{plugin_name}" }
                    if !version_display.is_empty() {
                        span { class: "plugin-version", "{version_display}" }
                    }
                }

                // Right side
                div {
                    class: "top-bar-right",

                    // Size buttons
                    SizeButtons {
                        selected: current_size,
                        selected_size: selected_size,
                    }

                    // Dropdown trigger only
                    div {
                        class: "dropdown-trigger",
                        onclick: move |_| {
                            nih_log!("[DROPDOWN] Trigger clicked, open={}", !is_open);
                            dropdown_open.set(!is_open);
                        },
                        span { "{SIZE_PRESET_NAMES[current_size]}" }
                        span { class: "dropdown-arrow", if is_open { "▲" } else { "▼" } }
                    }
                }
            }

            // Content area
            div {
                class: "content-area",
                {children}
            }

            // Dropdown menu rendered at root level (sibling to content-area)
            // This ensures it's in the same stacking context and painted last
            if is_open {
                div {
                    class: "dropdown-overlay",
                    onclick: move |_| {
                        // Click on overlay closes dropdown
                        dropdown_open.set(false);
                    },

                    div {
                        class: "dropdown-menu-root",
                        onclick: move |e| {
                            // Prevent clicks inside menu from closing it via overlay
                            e.stop_propagation();
                        },

                        for idx in 0..SIZE_PRESET_NAMES.len() {
                            DropdownItem {
                                idx: idx,
                                selected: current_size,
                                selected_size: selected_size,
                                dropdown_open: dropdown_open,
                            }
                        }
                    }
                }
            }
        }
    }
}

#[component]
fn SizeButtons(selected: usize, mut selected_size: Signal<usize>) -> Element {
    rsx! {
        div {
            class: "size-buttons",
            for idx in 0..SIZE_PRESET_NAMES.len() {
                SizeButton {
                    idx: idx,
                    selected: selected,
                    selected_size: selected_size,
                }
            }
        }
    }
}

#[component]
fn SizeButton(idx: usize, selected: usize, mut selected_size: Signal<usize>) -> Element {
    let is_selected = idx == selected;
    let label = match idx {
        0 => "T",
        1 => "S",
        2 => "M",
        3 => "L",
        4 => "XL",
        5 => "F",
        _ => "?",
    };

    let class = if is_selected {
        "size-btn selected"
    } else {
        "size-btn"
    };

    rsx! {
        div {
            class: class,
            onclick: move |_| {
                let (width, height) = SIZE_PRESETS[idx];
                nih_log!("[BUTTON] Size {} clicked: {}x{}", idx, width, height);
                selected_size.set(idx);
                request_resize(idx);
            },
            "{label}"
        }
    }
}

#[component]
fn DropdownItem(
    idx: usize,
    selected: usize,
    mut selected_size: Signal<usize>,
    mut dropdown_open: Signal<bool>,
) -> Element {
    let name = SIZE_PRESET_NAMES.get(idx).unwrap_or(&"?");
    let (width, height) = SIZE_PRESETS.get(idx).copied().unwrap_or((0, 0));
    let is_selected = idx == selected;

    let class = if is_selected {
        "dropdown-item selected"
    } else {
        "dropdown-item"
    };

    // Single flat div with text content - no nested divs that might intercept clicks
    let label = format!("{} - {}x{}", name, width, height);

    rsx! {
        div {
            class: class,
            onclick: move |_| {
                nih_log!("[DROPDOWN] Item {} clicked: {}x{}", idx, width, height);
                selected_size.set(idx);
                dropdown_open.set(false);
                request_resize(idx);
            },
            "{label}"
        }
    }
}

/// Calculate the content area height given a total window height.
#[inline]
pub fn content_height(window_height: u32) -> u32 {
    window_height.saturating_sub(TOP_BAR_HEIGHT)
}
