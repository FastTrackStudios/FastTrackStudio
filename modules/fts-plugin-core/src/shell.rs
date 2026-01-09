//! Plugin shell/wrapper component with top bar and size controls.

use crate::sizing::{AspectRatio, SizeTier, MAX_WINDOW_SIZE};
use nih_plug::nih_log;
use nih_plug_dioxus::TAILWIND_CSS;
use nih_plug_dioxus::dioxus_native::prelude::*;
use nih_plug_dioxus::prelude::*;
use std::sync::Arc;

/// Top bar height in pixels.
pub const TOP_BAR_HEIGHT: u32 = 36;

// Legacy constants for backward compatibility
#[deprecated(since = "0.2.0", note = "Use sizing::SizeTier instead")]
pub const SIZE_PRESET_NAMES: &[&str] = &[
    "Tiny",
    "Small",
    "Medium",
    "Large",
    "Very Large",
    "Fullscreen",
];

#[deprecated(since = "0.2.0", note = "Use sizing::AspectRatio::dimensions() instead")]
pub const SIZE_PRESETS: &[(u32, u32)] = &[
    (320, 180),
    (480, 270),
    (640, 360),
    (960, 540),
    (1280, 720),
    (1920, 1080),
];

#[deprecated(since = "0.2.0", note = "Use sizing::SizeTier::Medium instead")]
pub const DEFAULT_SIZE_INDEX: usize = 2;

// CSS for the shell
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
    width: 180px;
    max-height: 400px;
    overflow-y: auto;
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

.dropdown-overlay {
    position: absolute;
    top: 0;
    left: 0;
    right: 0;
    bottom: 0;
    z-index: 999;
    background: transparent;
}

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
    width: 180px;
    max-height: 400px;
    overflow-y: auto;
}

.aspect-dropdown-root {
    position: absolute;
    top: 40px;
    right: 100px;
    background-color: #27272a;
    border: 1px solid #3f3f46;
    border-radius: 6px;
    min-width: 150px;
    padding: 4px;
    z-index: 1000;
    width: 160px;
    max-height: 400px;
    overflow-y: auto;
}

.dropdown-section-header {
    padding: 4px 12px;
    font-size: 10px;
    font-weight: 600;
    color: #71717a;
    text-transform: uppercase;
    letter-spacing: 0.5px;
}
"#;

/// Helper to request resize via DioxusState from context
fn do_resize(aspect: AspectRatio, tier: SizeTier) {
    let (width, height) = aspect.dimensions_with_max(tier, MAX_WINDOW_SIZE);
    if let Some(state) = try_use_context::<Arc<DioxusState>>() {
        nih_log!("[RESIZE] Requesting {}x{} ({} {})", width, height, aspect.name(), tier.name());
        state.request_resize(width, height);
    }
}

/// Plugin shell/wrapper component.
///
/// Provides a consistent wrapper with top bar, size controls, and aspect ratio selection.
#[component]
pub fn PluginShell(
    /// Plugin display name shown in top bar
    plugin_name: String,
    /// Optional version string (will be prefixed with "v")
    #[props(default = None)]
    version: Option<String>,
    /// Default aspect ratio
    #[props(default = AspectRatio::Widescreen)]
    default_aspect: AspectRatio,
    /// Default size tier
    #[props(default = SizeTier::Medium)]
    default_size: SizeTier,
    /// Whether to show the aspect ratio selector (false to lock aspect ratio)
    #[props(default = true)]
    show_aspect_selector: bool,
    /// Child content to render in the main area
    children: Element,
) -> Element {
    let current_aspect = use_signal(|| default_aspect);
    let current_tier = use_signal(|| default_size);
    let mut size_dropdown_open = use_signal(|| false);
    let mut aspect_dropdown_open = use_signal(|| false);

    let version_display = version.map(|v| format!("v{}", v)).unwrap_or_default();
    let is_size_open = *size_dropdown_open.read();
    let is_aspect_open = *aspect_dropdown_open.read();
    let aspect = *current_aspect.read();
    let tier = *current_tier.read();

    // Get current dimensions for display
    let (cur_w, cur_h) = aspect.dimensions_with_max(tier, MAX_WINDOW_SIZE);

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

                    // Aspect ratio selector (optional)
                    if show_aspect_selector {
                        div {
                            class: "dropdown-trigger",
                            onclick: move |_| {
                                aspect_dropdown_open.set(!is_aspect_open);
                                size_dropdown_open.set(false);
                            },
                            span { "{aspect.short_name()}" }
                            span { class: "dropdown-arrow", if is_aspect_open { "^" } else { "v" } }
                        }
                    }

                    // Size tier buttons
                    SizeTierButtons {
                        aspect: aspect,
                        selected_tier: tier,
                        current_aspect: current_aspect,
                        current_tier: current_tier,
                    }

                    // Size dropdown trigger
                    div {
                        class: "dropdown-trigger",
                        onclick: move |_| {
                            size_dropdown_open.set(!is_size_open);
                            aspect_dropdown_open.set(false);
                        },
                        span { "{cur_w}x{cur_h}" }
                        span { class: "dropdown-arrow", if is_size_open { "^" } else { "v" } }
                    }
                }
            }

            // Content area
            div {
                class: "content-area",
                {children}
            }

            // Aspect ratio dropdown menu
            if is_aspect_open {
                div {
                    class: "dropdown-overlay",
                    onclick: move |_| {
                        aspect_dropdown_open.set(false);
                    },

                    div {
                        class: "aspect-dropdown-root",
                        onclick: move |e| {
                            e.stop_propagation();
                        },

                        // Standard section
                        div { class: "dropdown-section-header", "Standard" }
                        AspectDropdownItem {
                            aspect_option: AspectRatio::Widescreen,
                            current_aspect: current_aspect,
                            current_tier: current_tier,
                            dropdown_open: aspect_dropdown_open,
                        }
                        AspectDropdownItem {
                            aspect_option: AspectRatio::Square,
                            current_aspect: current_aspect,
                            current_tier: current_tier,
                            dropdown_open: aspect_dropdown_open,
                        }

                        // Rack section
                        div { class: "dropdown-section-header", "Rack" }
                        AspectDropdownItem {
                            aspect_option: AspectRatio::Rack1U,
                            current_aspect: current_aspect,
                            current_tier: current_tier,
                            dropdown_open: aspect_dropdown_open,
                        }
                        AspectDropdownItem {
                            aspect_option: AspectRatio::Rack2U,
                            current_aspect: current_aspect,
                            current_tier: current_tier,
                            dropdown_open: aspect_dropdown_open,
                        }
                        AspectDropdownItem {
                            aspect_option: AspectRatio::Rack3U,
                            current_aspect: current_aspect,
                            current_tier: current_tier,
                            dropdown_open: aspect_dropdown_open,
                        }
                        AspectDropdownItem {
                            aspect_option: AspectRatio::Rack4U,
                            current_aspect: current_aspect,
                            current_tier: current_tier,
                            dropdown_open: aspect_dropdown_open,
                        }

                        // Half Rack section
                        div { class: "dropdown-section-header", "Half Rack" }
                        AspectDropdownItem {
                            aspect_option: AspectRatio::HalfRack1U,
                            current_aspect: current_aspect,
                            current_tier: current_tier,
                            dropdown_open: aspect_dropdown_open,
                        }
                        AspectDropdownItem {
                            aspect_option: AspectRatio::HalfRack2U,
                            current_aspect: current_aspect,
                            current_tier: current_tier,
                            dropdown_open: aspect_dropdown_open,
                        }
                        AspectDropdownItem {
                            aspect_option: AspectRatio::HalfRack3U,
                            current_aspect: current_aspect,
                            current_tier: current_tier,
                            dropdown_open: aspect_dropdown_open,
                        }
                        AspectDropdownItem {
                            aspect_option: AspectRatio::HalfRack4U,
                            current_aspect: current_aspect,
                            current_tier: current_tier,
                            dropdown_open: aspect_dropdown_open,
                        }

                        // 500 Series section
                        div { class: "dropdown-section-header", "500 Series" }
                        AspectDropdownItem {
                            aspect_option: AspectRatio::Series500Single,
                            current_aspect: current_aspect,
                            current_tier: current_tier,
                            dropdown_open: aspect_dropdown_open,
                        }
                        AspectDropdownItem {
                            aspect_option: AspectRatio::Series500Double,
                            current_aspect: current_aspect,
                            current_tier: current_tier,
                            dropdown_open: aspect_dropdown_open,
                        }
                    }
                }
            }

            // Size tier dropdown menu
            if is_size_open {
                div {
                    class: "dropdown-overlay",
                    onclick: move |_| {
                        size_dropdown_open.set(false);
                    },

                    div {
                        class: "dropdown-menu-root",
                        onclick: move |e| {
                            e.stop_propagation();
                        },

                        for tier_option in SizeTier::all().iter().copied() {
                            SizeDropdownItem {
                                aspect: aspect,
                                tier_option: tier_option,
                                selected_tier: tier,
                                current_aspect: current_aspect,
                                current_tier: current_tier,
                                dropdown_open: size_dropdown_open,
                            }
                        }
                    }
                }
            }
        }
    }
}

/// Size tier button strip (T, S, M, L, XL, F)
#[component]
fn SizeTierButtons(
    aspect: AspectRatio,
    selected_tier: SizeTier,
    mut current_aspect: Signal<AspectRatio>,
    mut current_tier: Signal<SizeTier>,
) -> Element {
    rsx! {
        div {
            class: "size-buttons",
            for tier in SizeTier::all().iter().copied() {
                SizeTierButton {
                    aspect: aspect,
                    tier: tier,
                    is_selected: tier == selected_tier,
                    current_aspect: current_aspect,
                    current_tier: current_tier,
                }
            }
        }
    }
}

/// Individual size tier button
#[component]
fn SizeTierButton(
    aspect: AspectRatio,
    tier: SizeTier,
    is_selected: bool,
    mut current_aspect: Signal<AspectRatio>,
    mut current_tier: Signal<SizeTier>,
) -> Element {
    let class = if is_selected {
        "size-btn selected"
    } else {
        "size-btn"
    };

    rsx! {
        div {
            class: class,
            onclick: move |_| {
                current_tier.set(tier);
                do_resize(aspect, tier);
            },
            "{tier.label()}"
        }
    }
}

/// Aspect ratio dropdown item
#[component]
fn AspectDropdownItem(
    aspect_option: AspectRatio,
    mut current_aspect: Signal<AspectRatio>,
    mut current_tier: Signal<SizeTier>,
    mut dropdown_open: Signal<bool>,
) -> Element {
    let current = *current_aspect.read();
    let tier = *current_tier.read();
    let is_selected = std::mem::discriminant(&aspect_option) == std::mem::discriminant(&current);

    let class = if is_selected {
        "dropdown-item selected"
    } else {
        "dropdown-item"
    };

    let (ratio_w, ratio_h) = aspect_option.ratio();
    let label = format!("{} ({}:{})", aspect_option.name(), ratio_w, ratio_h);

    rsx! {
        div {
            class: class,
            onclick: move |_| {
                current_aspect.set(aspect_option);
                dropdown_open.set(false);
                do_resize(aspect_option, tier);
            },
            "{label}"
        }
    }
}

/// Size dropdown item showing tier name and dimensions
#[component]
fn SizeDropdownItem(
    aspect: AspectRatio,
    tier_option: SizeTier,
    selected_tier: SizeTier,
    mut current_aspect: Signal<AspectRatio>,
    mut current_tier: Signal<SizeTier>,
    mut dropdown_open: Signal<bool>,
) -> Element {
    let is_selected = tier_option == selected_tier;
    let (width, height) = aspect.dimensions_with_max(tier_option, MAX_WINDOW_SIZE);

    let class = if is_selected {
        "dropdown-item selected"
    } else {
        "dropdown-item"
    };

    let label = format!("{} - {}x{}", tier_option.name(), width, height);

    rsx! {
        div {
            class: class,
            onclick: move |_| {
                current_tier.set(tier_option);
                dropdown_open.set(false);
                do_resize(aspect, tier_option);
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
