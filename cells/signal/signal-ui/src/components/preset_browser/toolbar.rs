//! Browser toolbar — bottom bar with actions.

use crate::prelude::*;

/// Props for the browser toolbar.
#[derive(Props, Clone, PartialEq)]
pub struct BrowserToolbarProps {
    /// Number of results shown.
    pub result_count: usize,
    /// Total preset count.
    pub total_count: usize,
    /// Close handler.
    pub on_close: EventHandler<()>,
}

/// Bottom toolbar for the preset browser.
#[component]
pub fn BrowserToolbar(props: BrowserToolbarProps) -> Element {
    rsx! {
        div {
            class: "browser-toolbar",
            style: "display: flex; align-items: center; justify-content: space-between; padding: 8px 16px; border-top: 1px solid #333; background: #1a1a1a; font-size: 12px;",

            // Left: result count
            span {
                style: "color: #888;",
                "{props.result_count} of {props.total_count} presets"
            }

            // Right: close button
            button {
                style: "padding: 4px 16px; background: #333; border: 1px solid #444; border-radius: 4px; color: #ddd; cursor: pointer; font-size: 12px;",
                onclick: {
                    let on_close = props.on_close.clone();
                    move |_| on_close.call(())
                },
                "Close"
            }
        }
    }
}
