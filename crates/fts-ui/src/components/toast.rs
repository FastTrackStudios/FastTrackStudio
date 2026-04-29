//! Toast — standalone toast notification system, shadcn v4 maia style.

use dioxus::prelude::*;

// ---------------------------------------------------------------------------
// Types
// ---------------------------------------------------------------------------

/// Visual variant for a toast notification.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ToastType {
    Success,
    Error,
    Warning,
    Info,
}

/// A single toast notification.
#[derive(Debug, Clone, PartialEq)]
pub struct Toast {
    pub id: u64,
    pub message: String,
    pub toast_type: ToastType,
    pub duration_ms: Option<u64>,
}

// ---------------------------------------------------------------------------
// Global state
// ---------------------------------------------------------------------------

static TOASTS: GlobalSignal<Vec<Toast>> = Signal::global(Vec::new);
static NEXT_ID: GlobalSignal<u64> = Signal::global(|| 0);

/// Handle returned by [`use_toast`] to show / dismiss toasts.
#[derive(Clone, Copy)]
pub struct Toasts;

impl Toasts {
    /// Push a new toast. Returns the assigned id.
    pub fn show(&self, message: impl Into<String>, toast_type: ToastType) -> u64 {
        self.show_with_duration(message, toast_type, Some(5000))
    }

    /// Push a new toast with an explicit duration (`None` = sticky).
    pub fn show_with_duration(
        &self,
        message: impl Into<String>,
        toast_type: ToastType,
        duration_ms: Option<u64>,
    ) -> u64 {
        let id = *NEXT_ID.read();
        *NEXT_ID.write() += 1;

        TOASTS.write().push(Toast {
            id,
            message: message.into(),
            toast_type,
            duration_ms,
        });

        id
    }

    /// Remove a toast by id.
    pub fn dismiss(&self, id: u64) {
        TOASTS.write().retain(|t| t.id != id);
    }
}

/// Obtain a [`Toasts`] handle that can show / dismiss toasts.
pub fn use_toast() -> Toasts {
    Toasts
}

// ---------------------------------------------------------------------------
// Components
// ---------------------------------------------------------------------------

/// Render at app root to display active toasts in the bottom-right corner.
#[component]
pub fn ToastProvider() -> Element {
    let toasts = TOASTS.read().clone();

    rsx! {
        div {
            class: "fixed z-50 bottom-4 right-4 flex flex-col gap-2 pointer-events-auto",
            for toast in toasts.iter() {
                ToastItem { key: "{toast.id}", toast: toast.clone() }
            }
        }
    }
}

// ---- single toast item ----------------------------------------------------

#[derive(Props, Clone, PartialEq)]
struct ToastItemProps {
    toast: Toast,
}

#[component]
fn ToastItem(props: ToastItemProps) -> Element {
    let toast = props.toast.clone();
    let id = toast.id;
    let handle = Toasts;

    // Auto-dismiss: currently toasts stay until manually dismissed.
    // (No async timer dependency available.)

    let border_class = match toast.toast_type {
        ToastType::Success => "border-l-4 border-l-green-500",
        ToastType::Error => "border-l-4 border-l-red-500",
        ToastType::Warning => "border-l-4 border-l-yellow-500",
        ToastType::Info => "",
    };

    rsx! {
        div {
            class: format!(
                "flex items-center gap-3 rounded-lg border border-border bg-popover text-popover-foreground p-4 shadow-md text-sm {border_class}"
            ),
            span { {toast.message.clone()} }
            button {
                class: "ml-auto opacity-70 hover:opacity-100 cursor-pointer",
                onclick: move |_| { handle.dismiss(id); },
                // Simple X glyph.
                svg {
                    xmlns: "http://www.w3.org/2000/svg",
                    width: "16",
                    height: "16",
                    view_box: "0 0 24 24",
                    fill: "none",
                    stroke: "currentColor",
                    stroke_width: "2",
                    stroke_linecap: "round",
                    stroke_linejoin: "round",
                    line { x1: "18", y1: "6", x2: "6", y2: "18" }
                    line { x1: "6", y1: "6", x2: "18", y2: "18" }
                }
            }
        }
    }
}
