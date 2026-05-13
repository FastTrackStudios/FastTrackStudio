//! `ProjectThemeSettings` — a grid of fts-ui preset preview cards used
//! to choose a project-local theme override.
//!
//! Dumb component. The host owns the override state (typically a
//! `Signal<HashMap<Uuid, String>>` provided via context). The card emits
//! `Some(name)` to apply a preset, or `None` to clear and fall back to
//! the organization theme.

use dioxus::prelude::*;
use fts_ui::lucide_dioxus::CircleCheck;
use fts_ui::prelude::*;

#[component]
pub fn ProjectThemeSettings(
    /// Currently active project-local preset name, or `None` when the
    /// project is inheriting its org's theme.
    current: Option<String>,
    on_change: EventHandler<Option<String>>,
) -> Element {
    let presets = theme_presets();
    let cur = current.clone();

    rsx! {
        div { class: "flex flex-col gap-3",
            div { class: "flex items-center justify-between gap-2",
                span { class: "text-xs font-semibold uppercase tracking-widest text-muted-foreground",
                    "Project theme"
                }
                Button {
                    variant: if cur.is_none() { ButtonVariant::Ghost } else { ButtonVariant::Outline },
                    size: ButtonSize::Small,
                    on_click: move |_| on_change.call(None),
                    "Use organization theme"
                }
            }

            div { class: "grid grid-cols-2 gap-2 md:grid-cols-3",
                for preset in presets.iter().cloned() {
                    {
                        let name = preset.name.clone();
                        let label = preset.label.clone();
                        let selected = cur.as_deref() == Some(name.as_str());
                        let style = preset.styles.light;
                        let bg = style.get("background").unwrap_or("#ffffff").to_string();
                        let border = style.get("border").unwrap_or("#e5e7eb").to_string();
                        let primary = style.get("primary").unwrap_or("#0ea5e9").to_string();
                        let secondary = style.get("secondary").unwrap_or("#64748b").to_string();
                        let accent = style.get("accent").unwrap_or("#f59e0b").to_string();
                        let destructive = style.get("destructive").unwrap_or("#ef4444").to_string();
                        let foreground = style.get("foreground").unwrap_or("#0f172a").to_string();
                        let card_style = format!(
                            "background-color:{bg};border-color:{border};color:{foreground};",
                        );
                        let ring = if selected {
                            "ring-2 ring-primary"
                        } else {
                            "ring-0"
                        };
                        let name_for_click = name.clone();
                        rsx! {
                            button {
                                key: "{name}",
                                r#type: "button",
                                class: "relative flex flex-col gap-2 rounded-lg border p-3 text-left transition-shadow hover:shadow-md {ring}",
                                style: card_style,
                                onclick: move |_| on_change.call(Some(name_for_click.clone())),
                                if selected {
                                    span {
                                        class: "absolute right-2 top-2 text-primary",
                                        CircleCheck { size: 16 }
                                    }
                                }
                                div { class: "flex items-center gap-1",
                                    span {
                                        class: "h-4 w-4 rounded-full",
                                        style: format!("background-color:{primary};"),
                                    }
                                    span {
                                        class: "h-4 w-4 rounded-full",
                                        style: format!("background-color:{secondary};"),
                                    }
                                    span {
                                        class: "h-4 w-4 rounded-full",
                                        style: format!("background-color:{accent};"),
                                    }
                                    span {
                                        class: "h-4 w-4 rounded-full",
                                        style: format!("background-color:{destructive};"),
                                    }
                                }
                                span { class: "text-xs font-medium", "{label}" }
                            }
                        }
                    }
                }
            }

            Text { variant: TextVariant::Muted,
                "Only applies inside this project. Falls back to the organization's theme when unset."
            }
        }
    }
}
