//! `/settings` — per-user preferences, backed by the server-side
//! `UserPrefs` entity (see [`crate::prefs`]): every control here
//! writes through `PrefsCtx::update`, so choices follow the account
//! to any device.

use dioxus::prelude::*;
use fts_ui::prelude::*;

use crate::prefs::PrefsCtx;
use crate::theming::use_org_theme_switcher_state;

/// The routes worth landing on. `""` = the app default (`/`).
const PAGE_CHOICES: &[(&str, &str)] = &[
    ("", "Tasks (default)"),
    ("/home", "Active work dashboard"),
    ("/inbox", "Inbox"),
    ("/projects", "Projects"),
    ("/schedule", "Schedule"),
    ("/vault", "Vault"),
];

#[component]
pub fn SettingsView() -> Element {
    let prefs_ctx = use_context::<PrefsCtx>();
    let prefs = prefs_ctx.prefs.read().clone();

    // Same active-org ↔ overrides bridge as the shell's theme pickers;
    // persistence rides the App-root `use_theme_prefs_sync` effect.
    let theme_state = use_org_theme_switcher_state();

    rsx! {
        div { class: "mx-auto flex max-w-5xl flex-col gap-6 p-4 sm:p-6 lg:p-10",
            Heading { level: HeadingLevel::H1, "Settings" }

            section { class: "flex flex-col gap-3",
                Heading { level: HeadingLevel::H3, "Appearance" }
                Text {
                    variant: TextVariant::Muted,
                    "Theme preset and light/dark mode for the active organization. Follows your account on every device."
                }
                ThemeSwitcher { state: theme_state, class: "max-w-md" }
            }

            section { class: "flex flex-col gap-3",
                Heading { level: HeadingLevel::H3, "Start page" }
                Text {
                    variant: TextVariant::Muted,
                    "Where the app opens. Follows your account on every device."
                }
                select {
                    class: "w-64 rounded-lg border border-border bg-card px-3 py-1.5 text-sm text-foreground outline-none focus:ring-2 focus:ring-primary/50",
                    value: "{prefs.default_page}",
                    onchange: move |e| prefs_ctx.update(|p| p.default_page = e.value()),
                    for (value, label) in PAGE_CHOICES {
                        option {
                            key: "{value}",
                            value: "{value}",
                            selected: prefs.default_page == *value,
                            "{label}"
                        }
                    }
                }
            }

            section { class: "flex flex-col gap-3",
                Heading { level: HeadingLevel::H3, "Task board defaults" }
                Text {
                    variant: TextVariant::Muted,
                    "The filters the board opens with (also toggleable inline on the board)."
                }
                div { class: "flex items-center gap-4",
                    label { class: "flex items-center gap-2 text-sm text-foreground",
                        input {
                            r#type: "checkbox",
                            checked: prefs.tasks_active,
                            onchange: move |e| prefs_ctx.update(|p| p.tasks_active = e.checked()),
                        }
                        "Active only"
                    }
                    label { class: "flex items-center gap-2 text-sm text-foreground",
                        input {
                            r#type: "checkbox",
                            checked: prefs.tasks_relevant,
                            onchange: move |e| prefs_ctx.update(|p| p.tasks_relevant = e.checked()),
                        }
                        "Relevant only"
                    }
                }
            }
        }
    }
}
