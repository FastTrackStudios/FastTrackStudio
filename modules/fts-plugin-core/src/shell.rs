//! Plugin shell/wrapper component with top bar and size controls.

use crate::sizing::{AspectRatio, SizeTier, MAX_WINDOW_SIZE};
use lumen_blocks::components::button::{Button, ButtonSize, ButtonVariant};
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

// Shell-specific CSS that can't be done with Tailwind alone
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
            class: "dark w-full h-full flex flex-col bg-background font-sans relative",

            // Top bar
            div {
                class: "h-9 min-h-9 bg-card border-b border-border flex items-center px-3 justify-between select-none relative z-50",

                // Left side
                div {
                    class: "flex items-center gap-2",
                    span { class: "text-foreground font-semibold text-sm", "{plugin_name}" }
                    if !version_display.is_empty() {
                        span { class: "text-muted-foreground text-xs", "{version_display}" }
                    }
                }

                // Right side
                div {
                    class: "flex items-center gap-2",

                    // Aspect ratio selector (optional)
                    if show_aspect_selector {
                        div {
                            class: "flex items-center gap-1.5 px-2.5 py-1 bg-secondary border border-border rounded text-secondary-foreground text-xs font-medium cursor-pointer hover:bg-muted",
                            onclick: move |_| {
                                aspect_dropdown_open.set(!is_aspect_open);
                                size_dropdown_open.set(false);
                            },
                            span { "{aspect.short_name()}" }
                            span { class: "text-muted-foreground", if is_aspect_open { "^" } else { "v" } }
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
                        class: "flex items-center gap-1.5 px-2.5 py-1 bg-secondary border border-border rounded text-secondary-foreground text-xs font-medium cursor-pointer hover:bg-muted",
                        onclick: move |_| {
                            size_dropdown_open.set(!is_size_open);
                            aspect_dropdown_open.set(false);
                        },
                        span { "{cur_w}x{cur_h}" }
                        span { class: "text-muted-foreground", if is_size_open { "^" } else { "v" } }
                    }
                }
            }

            // Content area
            div {
                class: "flex-1 overflow-hidden",
                {children}
            }

            // Aspect ratio dropdown menu
            if is_aspect_open {
                div {
                    class: "absolute inset-0 z-40 bg-transparent",
                    onclick: move |_| {
                        aspect_dropdown_open.set(false);
                    },

                    div {
                        class: "absolute top-10 right-24 bg-popover border border-border rounded-lg min-w-40 p-1 z-50 w-40 max-h-96 overflow-y-auto",
                        onclick: move |e| {
                            e.stop_propagation();
                        },

                        // Standard section
                        div { class: "px-3 py-1 text-xs font-semibold text-muted-foreground uppercase tracking-wide", "Standard" }
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
                        div { class: "px-3 py-1 text-xs font-semibold text-muted-foreground uppercase tracking-wide", "Rack" }
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
                        div { class: "px-3 py-1 text-xs font-semibold text-muted-foreground uppercase tracking-wide", "Half Rack" }
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
                        div { class: "px-3 py-1 text-xs font-semibold text-muted-foreground uppercase tracking-wide", "500 Series" }
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
                    class: "absolute inset-0 z-40 bg-transparent",
                    onclick: move |_| {
                        size_dropdown_open.set(false);
                    },

                    div {
                        class: "absolute top-10 right-3 bg-popover border border-border rounded-lg min-w-40 p-1 z-50 w-44 max-h-96 overflow-y-auto",
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
            class: "flex items-center gap-0.5 bg-secondary rounded p-0.5",
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
    let variant = if is_selected {
        ButtonVariant::Primary
    } else {
        ButtonVariant::Ghost
    };

    let label = tier.label();

    rsx! {
        Button {
            variant: variant,
            size: ButtonSize::Small,
            on_click: move |_| {
                current_tier.set(tier);
                do_resize(aspect, tier);
            },
            "{label}"
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
        "px-3 py-2 cursor-pointer rounded bg-primary text-primary-foreground text-sm"
    } else {
        "px-3 py-2 cursor-pointer rounded text-popover-foreground hover:bg-muted text-sm"
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
        "px-3 py-2 cursor-pointer rounded bg-primary text-primary-foreground text-sm"
    } else {
        "px-3 py-2 cursor-pointer rounded text-popover-foreground hover:bg-muted text-sm"
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
