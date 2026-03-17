//! Dropdown — wraps lumen-blocks dropdown with FTS z-index fix.
//!
//! Re-exports all lumen-blocks dropdown components, but overrides
//! `DropdownContent` to use `z-[100]` so it renders above navbars,
//! sticky headers, and other layered UI.

// Re-export everything except DropdownContent (we override it below)
pub use lumen_blocks::components::dropdown::{
    Dropdown, DropdownCheckboxItem, DropdownCheckboxItemProps, DropdownItem, DropdownItemProps,
    DropdownLabel, DropdownLabelProps, DropdownProps, DropdownRadioGroup, DropdownRadioGroupProps,
    DropdownRadioItem, DropdownRadioItemProps, DropdownSeparator, DropdownSeparatorProps,
    DropdownSize, DropdownTrigger, DropdownTriggerProps,
};

use dioxus::prelude::*;
use dioxus_primitives::dropdown_menu::DropdownMenuContent;

/// Dropdown content panel — uses `z-[100]` instead of lumen-blocks' `z-50`
/// so dropdowns render above navbars, sticky headers, and chart overlays.
#[derive(Props, Clone, PartialEq)]
pub struct DropdownContentProps {
    /// Alignment of the dropdown menu.
    #[props(default = String::from("start"))]
    pub align: String,

    /// Width of the dropdown menu (e.g. "w-56", "w-72").
    #[props(default = String::from("w-56"))]
    pub width: String,

    /// Optional ID for the content.
    #[props(default)]
    pub id: Option<String>,

    /// Children elements.
    pub children: Element,
}

#[component]
pub fn DropdownContent(props: DropdownContentProps) -> Element {
    let align_class = match props.align.as_str() {
        "end" => "right-0 origin-top-right",
        "center" => "left-1/2 -translate-x-1/2 origin-top",
        _ => "left-0 origin-top-left",
    };

    let content_classes = format!(
        "absolute mt-2 rounded bg-popover shadow-md \
         border border-border p-1 text-popover-foreground \
         animate-in fade-in-80 data-[side=bottom]:slide-in-from-top-2 \
         data-[side=left]:slide-in-from-right-2 data-[side=right]:slide-in-from-left-2 \
         data-[side=top]:slide-in-from-bottom-2 z-[100] \
         {align_class} {}",
        props.width
    );

    rsx! {
        DropdownMenuContent {
            class: content_classes,
            id: props.id.unwrap_or_default(),
            {props.children}
        }
    }
}
