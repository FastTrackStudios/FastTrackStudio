//! Row of small ghost icon buttons that appear on message hover.
//!
//! Visibility is parent-controlled — wrap the parent in `group/message`
//! and this component sits inside a `opacity-0 group-hover/message:opacity-100`
//! container. The component itself just renders the buttons.

use dioxus::prelude::*;
use fts_ui::lucide_dioxus::{Copy, GitBranch, Pencil, Pin, RefreshCw, Trash2};
use fts_ui::prelude::*;
use uuid::Uuid;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum MessageAction {
    Copy,
    Edit,
    Regenerate,
    Branch,
    Pin,
    Delete,
}

#[derive(Props, Clone, PartialEq)]
pub struct MessageActionsProps {
    pub message_id: Uuid,
    pub role: String,
    /// Body text — used for the inline clipboard write on Copy.
    pub body: String,
    pub on_action: EventHandler<(Uuid, MessageAction)>,
}

#[component]
pub fn MessageActions(props: MessageActionsProps) -> Element {
    let role = props.role.clone();
    let id = props.message_id;
    let body_copy = props.body.clone();

    let on_copy_click = move |_| {
        copy_to_clipboard(&body_copy);
        props.on_action.call((id, MessageAction::Copy));
    };

    rsx! {
        div { class: "flex items-center gap-0.5",
            ActionButton {
                tooltip: "Copy",
                on_click: on_copy_click,
                Copy { size: 14 }
            }
            if role == "assistant" {
                ActionButton {
                    tooltip: "Regenerate",
                    on_click: move |_| props.on_action.call((id, MessageAction::Regenerate)),
                    RefreshCw { size: 14 }
                }
                ActionButton {
                    tooltip: "Branch",
                    on_click: move |_| props.on_action.call((id, MessageAction::Branch)),
                    GitBranch { size: 14 }
                }
                ActionButton {
                    tooltip: "Pin",
                    on_click: move |_| props.on_action.call((id, MessageAction::Pin)),
                    Pin { size: 14 }
                }
            }
            if role == "user" {
                ActionButton {
                    tooltip: "Edit",
                    on_click: move |_| props.on_action.call((id, MessageAction::Edit)),
                    Pencil { size: 14 }
                }
            }
            ActionButton {
                tooltip: "Delete",
                on_click: move |_| props.on_action.call((id, MessageAction::Delete)),
                Trash2 { size: 14 }
            }
        }
    }
}

#[derive(Props, Clone, PartialEq)]
struct ActionButtonProps {
    tooltip: &'static str,
    on_click: EventHandler<MouseEvent>,
    children: Element,
}

#[component]
fn ActionButton(props: ActionButtonProps) -> Element {
    rsx! {
        Tooltip {
            TooltipTrigger {
                Button {
                    variant: ButtonVariant::Ghost,
                    size: ButtonSize::Small,
                    on_click: move |e| props.on_click.call(e),
                    {props.children}
                }
            }
            TooltipContent { side: TooltipSide::Top, "{props.tooltip}" }
        }
    }
}

fn copy_to_clipboard(text: &str) {
    #[cfg(target_arch = "wasm32")]
    {
        if let Some(win) = web_sys::window() {
            let _ = win.navigator().clipboard().write_text(text);
        }
    }
    #[cfg(not(target_arch = "wasm32"))]
    {
        tracing::info!(bytes = text.len(), "clipboard copy (native stub)");
    }
}
