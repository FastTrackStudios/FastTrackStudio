//! `Priority` enum + `PriorityBadge` component.
//!
//! The wire type (`Task::priority`) is a free-form `String`; this
//! module normalises it at the UI boundary so the badge can switch
//! purely on the typed enum rather than scattered `match` arms.
//!
//! Icon choices (we map each level to one existing lucide icon —
//! ASCII glyphs + tint do the rest of the visual):
//!  - Urgent → `OctagonAlert`  (red, filled-look via bg tint)
//!  - High   → `SignalHigh`    (orange)
//!  - Medium → `SignalMedium`  (amber)
//!  - Low    → `SignalLow`     (muted)
//!  - None   → `Minus`         (very muted)
//!
//! These are the closest lucide approximations to the Linear-style
//! 4-bar signal glyphs the spec references.

use dioxus::prelude::*;
use fts_ui::lucide_dioxus::{Minus, OctagonAlert, SignalHigh, SignalLow, SignalMedium};
use fts_ui::prelude::*;

/// Typed priority level — produced via `TryFrom<&str>` (case-
/// insensitive). Unknown values are preserved so we never lose data
/// on round-trip.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Priority {
    Urgent,
    High,
    Medium,
    Low,
    None,
    Unknown(String),
}

impl Priority {
    pub fn as_wire(&self) -> String {
        match self {
            Self::Urgent => "urgent".into(),
            Self::High => "high".into(),
            Self::Medium => "medium".into(),
            Self::Low => "low".into(),
            Self::None => "none".into(),
            Self::Unknown(s) => s.clone(),
        }
    }

    pub fn label(&self) -> &str {
        match self {
            Self::Urgent => "Urgent",
            Self::High => "High",
            Self::Medium => "Medium",
            Self::Low => "Low",
            Self::None => "None",
            Self::Unknown(s) => s.as_str(),
        }
    }

    /// Tailwind text-color class for the icon glyph.
    pub fn tint_class(&self) -> &'static str {
        match self {
            Self::Urgent => "text-red-500",
            Self::High => "text-orange-500",
            Self::Medium => "text-amber-500",
            Self::Low => "text-muted-foreground",
            Self::None => "text-muted-foreground/60",
            Self::Unknown(_) => "text-muted-foreground",
        }
    }

    pub fn all() -> [Priority; 5] {
        [
            Self::Urgent,
            Self::High,
            Self::Medium,
            Self::Low,
            Self::None,
        ]
    }
}

impl<'a> TryFrom<&'a str> for Priority {
    type Error = std::convert::Infallible;
    fn try_from(s: &'a str) -> Result<Self, Self::Error> {
        let t = s.trim().to_lowercase();
        Ok(match t.as_str() {
            "" => Self::None,
            "urgent" | "p0" => Self::Urgent,
            "high" | "p1" => Self::High,
            "medium" | "med" | "p2" => Self::Medium,
            "low" | "p3" => Self::Low,
            "none" | "no-priority" | "p4" => Self::None,
            _ => Self::Unknown(s.to_string()),
        })
    }
}

/// Render the sigil icon for a `Priority` at a uniform 14px size.
#[component]
fn PriorityIcon(priority: Priority) -> Element {
    let cls = priority.tint_class();
    rsx! {
        span { class: "inline-flex items-center justify-center {cls}",
            match priority {
                Priority::Urgent => rsx! { OctagonAlert { size: 14 } },
                Priority::High => rsx! { SignalHigh { size: 14 } },
                Priority::Medium => rsx! { SignalMedium { size: 14 } },
                Priority::Low => rsx! { SignalLow { size: 14 } },
                Priority::None => rsx! { Minus { size: 14 } },
                Priority::Unknown(_) => rsx! { Minus { size: 14 } },
            }
        }
    }
}

/// Linear-style priority sigil.
///
/// When `editable` is true, the badge is a `Popover` trigger that opens
/// a 5-row picker. When false, it's a plain inline `span`.
#[component]
pub fn PriorityBadge(
    priority: String,
    #[props(default = false)] show_label: bool,
    #[props(default = false)] editable: bool,
    on_change: EventHandler<String>,
) -> Element {
    let parsed: Priority = Priority::try_from(priority.as_str()).unwrap();
    let mut open = use_signal(|| false);

    let display = rsx! {
        span { class: "inline-flex items-center gap-1.5",
            PriorityIcon { priority: parsed.clone() }
            if show_label {
                span { class: "text-xs text-foreground", "{parsed.label()}" }
            }
        }
    };

    if !editable {
        return rsx! { {display} };
    }

    rsx! {
        Popover {
            open: open(),
            is_modal: false,
            on_open_change: move |o| open.set(o),
            PopoverTrigger {
                class: "inline-flex".to_string(),
                button {
                    r#type: "button",
                    class: "inline-flex items-center gap-1.5 rounded-md px-1.5 py-0.5 hover:bg-accent",
                    onclick: move |_| {
                        let v = !*open.read();
                        open.set(v);
                    },
                    {display}
                }
            }
            PopoverContent {
                class: "w-44 p-1".to_string(),
                div { class: "flex flex-col",
                    for p in Priority::all() {
                        {
                            let selected = p == parsed;
                            let wire = p.as_wire();
                            let p_for_icon = p.clone();
                            let p_for_label = p.clone();
                            rsx! {
                                button {
                                    key: "{wire}",
                                    r#type: "button",
                                    class: if selected {
                                        "flex items-center gap-2 rounded-md px-2 py-1.5 text-sm bg-accent text-accent-foreground"
                                    } else {
                                        "flex items-center gap-2 rounded-md px-2 py-1.5 text-sm hover:bg-accent hover:text-accent-foreground"
                                    },
                                    onclick: move |_| {
                                        on_change.call(wire.clone());
                                        open.set(false);
                                    },
                                    PriorityIcon { priority: p_for_icon }
                                    span { "{p_for_label.label()}" }
                                }
                            }
                        }
                    }
                }
            }
        }
    }
}
