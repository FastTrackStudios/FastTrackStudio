//! Notification bell — dropdown showing recent notifications with unread count.

use dioxus::prelude::*;
use serde::Deserialize;
use crate::UserAvatar;

/// A notification entry for display.
#[derive(Clone, PartialEq, Deserialize)]
pub struct NotificationItem {
    pub id: String,
    pub message: String,
    pub actor: Option<String>,
    pub time_ago: String,
    pub read: bool,
    pub kind: String,
}

/// Notification bell with unread badge and dropdown.
#[component]
pub fn NotificationBell(
    #[props(default)]
    notifications: Vec<NotificationItem>,
    #[props(default)]
    on_mark_read: Option<Callback<String>>,
) -> Element {
    let mut open = use_signal(|| false);
    let unread = notifications.iter().filter(|n| !n.read).count();

    rsx! {
        div { class: "relative",
            // Bell button
            button {
                class: "relative p-1.5 rounded-lg hover:bg-accent transition-colors",
                onclick: move |_| { let v = *open.read(); open.set(!v); },
                svg {
                    class: "size-4 text-muted-foreground",
                    xmlns: "http://www.w3.org/2000/svg",
                    view_box: "0 0 24 24",
                    fill: "none",
                    stroke: "currentColor",
                    stroke_width: "2",
                    stroke_linecap: "round",
                    stroke_linejoin: "round",
                    path { d: "M6 8a6 6 0 0 1 12 0c0 7 3 9 3 9H3s3-2 3-9" }
                    path { d: "M10.3 21a1.94 1.94 0 0 0 3.4 0" }
                }
                if unread > 0 {
                    span { class: "absolute -top-0.5 -right-0.5 flex size-4 items-center justify-center rounded-full bg-destructive text-[9px] font-bold text-destructive-foreground",
                        "{unread}"
                    }
                }
            }

            // Dropdown
            if *open.read() {
                // Backdrop
                div {
                    class: "fixed inset-0 z-40",
                    onclick: move |_| open.set(false),
                }
                // Panel
                div { class: "absolute right-0 top-full mt-2 w-80 max-h-96 overflow-y-auto rounded-xl border border-border bg-card shadow-lg z-50",
                    div { class: "px-4 py-2 border-b border-border",
                        span { class: "text-xs font-medium text-muted-foreground",
                            { format!("Notifications ({})", notifications.len()) }
                        }
                    }
                    if notifications.is_empty() {
                        div { class: "px-4 py-6 text-center text-xs text-muted-foreground",
                            "No notifications"
                        }
                    } else {
                        div { class: "divide-y divide-border",
                            for notif in notifications.iter() {
                                {
                                    let id = notif.id.clone();
                                    let on_mark_read = on_mark_read.clone();
                                    rsx! {
                                        div {
                                            class: if notif.read {
                                                "flex items-start gap-3 px-4 py-3 hover:bg-accent/30 transition-colors"
                                            } else {
                                                "flex items-start gap-3 px-4 py-3 hover:bg-accent/30 transition-colors bg-primary/5"
                                            },
                                            onclick: move |_| {
                                                if let Some(ref cb) = on_mark_read {
                                                    cb.call(id.clone());
                                                }
                                            },
                                            if let Some(ref actor) = notif.actor {
                                                UserAvatar { name: actor.clone(), size: "size-6".to_string() }
                                            }
                                            div { class: "flex-1 min-w-0",
                                                p { class: "text-xs", "{notif.message}" }
                                                span { class: "text-[10px] text-muted-foreground", "{notif.time_ago}" }
                                            }
                                            if !notif.read {
                                                span { class: "size-2 rounded-full bg-primary shrink-0 mt-1.5" }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }
}
