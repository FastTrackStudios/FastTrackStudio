//! NavTab — standalone navigation pill button.
//!
//! A pill-shaped button with active/inactive styling for primary navigation bars.
//! Unlike `Tabs` (context-based content switcher) or `SegmentedControl` (grouped toggle),
//! NavTab is a standalone button meant for top-level route navigation.

use dioxus::prelude::*;

#[derive(Props, Clone, PartialEq)]
pub struct NavTabProps {
    /// Display text.
    pub label: String,
    /// Identifier passed to callback on click.
    pub tab_id: String,
    /// Whether this tab is currently active.
    pub is_active: bool,
    /// Click handler — receives `tab_id`.
    #[props(default)]
    pub on_click: Option<Callback<String>>,
    /// Extra CSS classes.
    #[props(default)]
    pub class: String,
}

/// Standalone navigation pill button.
#[component]
pub fn NavTab(props: NavTabProps) -> Element {
    let active_class = if props.is_active {
        "bg-primary text-primary-foreground"
    } else {
        "text-muted-foreground hover:text-foreground hover:bg-accent"
    };

    rsx! {
        button {
            class: crate::cn::merge_slice(&["px-4 py-2 rounded-md font-medium text-sm transition-colors", active_class, props.class.as_str()]),
            onclick: {
                let on_click = props.on_click;
                let tab_id = props.tab_id.clone();
                move |_| {
                    if let Some(callback) = &on_click {
                        callback.call(tab_id.clone());
                    }
                }
            },
            "{props.label}"
        }
    }
}
