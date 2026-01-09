//! Native dropdown component for Blitz/dioxus-native.
//!
//! This dropdown uses the "render at root" technique to ensure proper hit testing
//! in Blitz. The overlay and menu are rendered as siblings to the main content,
//! not nested inside other elements.

use nih_plug_dioxus::dioxus_native::prelude::*;

/// CSS for the dropdown - using explicit styles for Blitz compatibility.
pub const DROPDOWN_CSS: &str = r#"
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
    font-size: 10px;
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

.dropdown-menu {
    position: absolute;
    background-color: #18181b;
    border: 1px solid #3f3f46;
    border-radius: 6px;
    min-width: 150px;
    padding: 4px;
    z-index: 1000;
}

.dropdown-item {
    padding: 8px 12px;
    cursor: pointer;
    border-radius: 4px;
    color: #d4d4d8;
    font-size: 14px;
}

.dropdown-item:hover {
    background-color: #27272a;
}

.dropdown-item-selected {
    background-color: #2563eb;
    color: white;
}

.dropdown-item-selected:hover {
    background-color: #1d4ed8;
}

.dropdown-item-label {
    font-weight: 500;
}

.dropdown-item-description {
    font-size: 11px;
    color: #71717a;
    margin-top: 2px;
}

.dropdown-item-selected .dropdown-item-description {
    color: #93c5fd;
}

.dropdown-separator {
    height: 1px;
    background-color: #3f3f46;
    margin: 4px 0;
}
"#;

/// Props for a dropdown item.
#[derive(Clone, PartialEq)]
pub struct DropdownItemData {
    /// Unique identifier/value for this item.
    pub value: String,
    /// Display label.
    pub label: String,
    /// Optional description shown below the label.
    pub description: Option<String>,
}

impl DropdownItemData {
    pub fn new(value: impl Into<String>, label: impl Into<String>) -> Self {
        Self {
            value: value.into(),
            label: label.into(),
            description: None,
        }
    }

    pub fn with_description(mut self, desc: impl Into<String>) -> Self {
        self.description = Some(desc.into());
        self
    }
}

/// Dropdown trigger button.
///
/// This just renders the button - the actual menu is rendered separately at root level.
#[component]
pub fn DropdownTrigger(
    /// Current display text.
    label: String,
    /// Whether the dropdown is open.
    is_open: bool,
    /// Click handler.
    on_click: EventHandler<MouseEvent>,
) -> Element {
    rsx! {
        div {
            class: "dropdown-trigger",
            onclick: move |e| on_click.call(e),
            span { "{label}" }
            span { class: "dropdown-arrow", if is_open { "▲" } else { "▼" } }
        }
    }
}

/// Dropdown menu overlay and content.
///
/// **Important**: This must be rendered at the ROOT level of your component tree,
/// as a sibling to other content, NOT nested inside other elements.
/// This ensures proper hit testing in Blitz.
///
/// Example:
/// ```ignore
/// rsx! {
///     div { class: "my-app",
///         // Your app content...
///         DropdownTrigger { ... }
///     }
///     
///     // Render dropdown at root level when open
///     if is_open {
///         DropdownMenu {
///             top: 40,
///             right: 12,
///             items: items,
///             selected: selected_value,
///             on_select: move |value| { ... },
///             on_close: move |_| dropdown_open.set(false),
///         }
///     }
/// }
/// ```
#[component]
pub fn DropdownMenu(
    /// Top position in pixels.
    #[props(default = 40)]
    top: i32,
    /// Right position in pixels (use negative for left positioning).
    #[props(default = 12)]
    right: i32,
    /// Width in pixels.
    #[props(default = 160)]
    width: i32,
    /// Items to display.
    items: Vec<DropdownItemData>,
    /// Currently selected value.
    selected: String,
    /// Called when an item is selected.
    on_select: EventHandler<String>,
    /// Called when the dropdown should close (clicking outside).
    on_close: EventHandler<MouseEvent>,
) -> Element {
    let menu_style = format!("top: {}px; right: {}px; width: {}px;", top, right, width);

    rsx! {
        // Overlay catches clicks outside the menu
        div {
            class: "dropdown-overlay",
            onclick: move |e| on_close.call(e),
        }

        // Menu content
        div {
            class: "dropdown-menu",
            style: "{menu_style}",
            onclick: move |e| {
                // Prevent clicks inside menu from closing via overlay
                e.stop_propagation();
            },

            for item in items.iter() {
                DropdownMenuItem {
                    item: item.clone(),
                    is_selected: item.value == selected,
                    on_select: on_select,
                }
            }
        }
    }
}

/// Individual dropdown menu item.
#[component]
fn DropdownMenuItem(
    item: DropdownItemData,
    is_selected: bool,
    on_select: EventHandler<String>,
) -> Element {
    let class = if is_selected {
        "dropdown-item dropdown-item-selected"
    } else {
        "dropdown-item"
    };

    let value = item.value.clone();

    rsx! {
        div {
            class: "{class}",
            onclick: move |_| on_select.call(value.clone()),

            div { class: "dropdown-item-label", "{item.label}" }
            if let Some(desc) = &item.description {
                div { class: "dropdown-item-description", "{desc}" }
            }
        }
    }
}

/// A simple select dropdown that manages its own open state.
///
/// This is a convenience wrapper that combines trigger + menu.
/// The menu is rendered via a callback that should be placed at the root level.
#[component]
pub fn Select(
    /// Items to choose from.
    items: Vec<DropdownItemData>,
    /// Currently selected value.
    value: Signal<String>,
    /// Called when selection changes.
    #[props(default)]
    on_change: Option<EventHandler<String>>,
    /// Top position for the menu.
    #[props(default = 40)]
    menu_top: i32,
    /// Right position for the menu.
    #[props(default = 12)]
    menu_right: i32,
    /// Render function for the menu - call this at root level.
    render_menu: EventHandler<Element>,
) -> Element {
    let mut is_open = use_signal(|| false);
    let current_value = value.read().clone();

    // Find current label
    let current_label = items
        .iter()
        .find(|i| i.value == current_value)
        .map(|i| i.label.clone())
        .unwrap_or_else(|| current_value.clone());

    let open = *is_open.read();

    // Render the menu element for the parent to place at root
    if open {
        let menu = rsx! {
            DropdownMenu {
                top: menu_top,
                right: menu_right,
                items: items.clone(),
                selected: current_value.clone(),
                on_select: move |val: String| {
                    value.set(val.clone());
                    is_open.set(false);
                    if let Some(handler) = &on_change {
                        handler.call(val);
                    }
                },
                on_close: move |_| is_open.set(false),
            }
        };
        render_menu.call(menu);
    }

    rsx! {
        DropdownTrigger {
            label: current_label,
            is_open: open,
            on_click: move |_| is_open.set(!open),
        }
    }
}
