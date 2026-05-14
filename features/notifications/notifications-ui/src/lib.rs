//! Notifications UI — bell, inbox, toast.
//!
//! Dumb components. Data + callbacks in, RSX out. The task-ui route
//! crate subscribes to `notifications::ToastBus` server-side over WS
//! and pumps `current_toast` / `inbox_items` props on each event.

use dioxus::prelude::*;
use fts_ui::lucide_dioxus::{Bell, CircleAlert, CircleCheck, CircleX, Info, X as XIcon};
use fts_ui::prelude::*;
use notifications_proto::Notification;
use uuid::Uuid;

pub fn severity_variant(severity: &str) -> StatusBadgeVariant {
    match severity {
        "error" => StatusBadgeVariant::Danger,
        "warning" => StatusBadgeVariant::Warning,
        _ => StatusBadgeVariant::Neutral,
    }
}

fn severity_icon(severity: &str) -> Element {
    match severity {
        "error" => rsx! { CircleX { size: 16 } },
        "warning" => rsx! { CircleAlert { size: 16 } },
        "info" => rsx! { CircleCheck { size: 16 } },
        _ => rsx! { Info { size: 16 } },
    }
}

// ── NotificationBell ──────────────────────────────────────────────────

#[component]
pub fn NotificationBell(unread_count: u32, on_click: EventHandler<()>) -> Element {
    let badge_visible = unread_count > 0;
    let count_text = if unread_count > 99 {
        "99+".to_string()
    } else {
        unread_count.to_string()
    };
    rsx! {
        button {
            class: "relative inline-flex items-center justify-center p-2 rounded-md hover:bg-muted",
            onclick: move |_| on_click.call(()),
            Bell { size: 18 }
            if badge_visible {
                span {
                    class: "absolute -top-0.5 -right-0.5 min-w-[16px] h-[16px] px-1 rounded-full bg-rose-500 text-white text-[10px] font-medium leading-[16px] text-center",
                    "{count_text}"
                }
            }
        }
    }
}

// ── NotificationInbox ─────────────────────────────────────────────────

#[component]
pub fn NotificationInbox(
    items: Vec<Notification>,
    on_mark_read: EventHandler<Uuid>,
    on_dismiss: EventHandler<Uuid>,
    on_mark_all_read: EventHandler<()>,
    on_open: EventHandler<Uuid>,
) -> Element {
    let any_unread = items.iter().any(|n| n.read_at.is_none());
    rsx! {
        VStack { class: "gap-3",
            HStack { class: "items-center justify-between",
                HStack { class: "gap-2 items-center",
                    Bell { size: 18 }
                    Heading { level: HeadingLevel::H2, "Notifications" }
                }
                if any_unread {
                    Button {
                        variant: ButtonVariant::Outline,
                        size: ButtonSize::Small,
                        on_click: move |_| on_mark_all_read.call(()),
                        "Mark all read"
                    }
                }
            }
            if items.is_empty() {
                EmptyState {
                    message: "No notifications.",
                    icon: rsx! { Bell { size: 32 } },
                }
            } else {
                div { class: "rounded-md border border-border bg-card overflow-hidden",
                    for n in items.iter().cloned() {
                        NotificationRow {
                            key: "{n.id}",
                            notification: n.clone(),
                            on_mark_read,
                            on_dismiss,
                            on_open,
                        }
                    }
                }
            }
        }
    }
}

#[component]
fn NotificationRow(
    notification: Notification,
    on_mark_read: EventHandler<Uuid>,
    on_dismiss: EventHandler<Uuid>,
    on_open: EventHandler<Uuid>,
) -> Element {
    let id = notification.id;
    let title = notification.title.clone();
    let body = notification.body.clone();
    let kind = notification.kind.clone();
    let severity = notification.severity.clone();
    let variant = severity_variant(&severity);
    let unread = notification.read_at.is_none();
    let bg = if unread { "bg-muted/30" } else { "" };
    let created = notification.created_at.format("%b %d %H:%M").to_string();
    let class = format!(
        "flex items-start gap-3 px-3 py-2 border-b border-border last:border-b-0 cursor-pointer hover:bg-muted/50 {bg}"
    );
    rsx! {
        div {
            class,
            onclick: move |_| {
                if unread {
                    on_mark_read.call(id);
                }
                on_open.call(id);
            },
            div { class: "mt-1", {severity_icon(&severity)} }
            div { class: "flex-1 min-w-0",
                HStack { class: "items-center gap-2",
                    span { class: "font-medium text-sm truncate", "{title}" }
                    StatusBadge { variant, label: kind }
                }
                if !body.is_empty() {
                    div { class: "text-xs text-muted-foreground mt-0.5 line-clamp-2", "{body}" }
                }
                div { class: "text-[10px] text-muted-foreground/70 mt-0.5", "{created}" }
            }
            Button {
                variant: ButtonVariant::Ghost,
                size: ButtonSize::Small,
                on_click: move |ev: MouseEvent| {
                    ev.stop_propagation();
                    on_dismiss.call(id);
                },
                XIcon { size: 14 }
            }
        }
    }
}

// ── ToastStack ─────────────────────────────────────────────────────────

#[component]
pub fn ToastStack(
    items: Vec<Notification>,
    on_dismiss: EventHandler<Uuid>,
    on_open: EventHandler<Uuid>,
) -> Element {
    rsx! {
        div { class: "pointer-events-none fixed top-4 right-4 z-50 flex flex-col gap-2",
            for n in items.iter().cloned() {
                NotificationToast {
                    key: "{n.id}",
                    notification: n.clone(),
                    on_dismiss,
                    on_open,
                }
            }
        }
    }
}

#[component]
fn NotificationToast(
    notification: Notification,
    on_dismiss: EventHandler<Uuid>,
    on_open: EventHandler<Uuid>,
) -> Element {
    let id = notification.id;
    let title = notification.title.clone();
    let body = notification.body.clone();
    let severity = notification.severity.clone();
    let border = match severity.as_str() {
        "error" => "border-l-rose-500",
        "warning" => "border-l-amber-500",
        _ => "border-l-emerald-500",
    };
    let class = format!(
        "pointer-events-auto flex items-start gap-3 rounded-md border border-border border-l-4 {border} bg-card shadow-lg px-3 py-2 min-w-[280px] max-w-md cursor-pointer"
    );
    rsx! {
        div {
            class,
            onclick: move |_| on_open.call(id),
            div { class: "mt-0.5", {severity_icon(&severity)} }
            div { class: "flex-1 min-w-0",
                div { class: "font-medium text-sm", "{title}" }
                if !body.is_empty() {
                    div { class: "text-xs text-muted-foreground line-clamp-2", "{body}" }
                }
            }
            Button {
                variant: ButtonVariant::Ghost,
                size: ButtonSize::Small,
                on_click: move |ev: MouseEvent| {
                    ev.stop_propagation();
                    on_dismiss.call(id);
                },
                XIcon { size: 14 }
            }
        }
    }
}

// ── Browser Notification API ──────────────────────────────────────────

/// Request permission for the browser Notification API. Idempotent —
/// the browser caches the decision. Returns true when permission is
/// granted (now or already). No-op on native targets.
#[cfg(target_arch = "wasm32")]
pub async fn request_browser_permission() -> bool {
    use wasm_bindgen::JsCast;
    use wasm_bindgen_futures::JsFuture;
    let Ok(perm) = web_sys::Notification::request_permission() else {
        return false;
    };
    match JsFuture::from(perm).await {
        Ok(v) => v.as_string().as_deref() == Some("granted"),
        Err(_) => false,
    }
}

#[cfg(not(target_arch = "wasm32"))]
pub async fn request_browser_permission() -> bool {
    false
}

/// Fire a browser Notification for a `Notification`. Requires
/// permission has already been granted (call `request_browser_permission`
/// once on first user interaction). No-op on native.
#[cfg(target_arch = "wasm32")]
pub fn fire_browser_notification(n: &Notification) {
    let mut opts = web_sys::NotificationOptions::new();
    opts.set_body(&n.body);
    let _ = web_sys::Notification::new_with_options(&n.title, &opts);
}

#[cfg(not(target_arch = "wasm32"))]
pub fn fire_browser_notification(_n: &Notification) {}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn severity_variant_mapping() {
        assert!(matches!(
            severity_variant("error"),
            StatusBadgeVariant::Danger
        ));
        assert!(matches!(
            severity_variant("warning"),
            StatusBadgeVariant::Warning
        ));
        assert!(matches!(
            severity_variant("info"),
            StatusBadgeVariant::Neutral
        ));
        assert!(matches!(
            severity_variant("unknown"),
            StatusBadgeVariant::Neutral
        ));
    }
}
