//! Reaper-Dioxus UI Component Test Panel
//!
//! Tests fts-ui components (shadcn-ui clone) in the native
//! Dioxus/Blitz renderer inside REAPER's docker system.

use daw::module::{ActionDef, DockPosition, PanelComponent, PanelDef};
use reaper_dioxus::prelude::*;

use fts_ui::prelude::*;

// Embed Tailwind CSS + FTS theme at compile time
const TAILWIND_CSS: &str = include_str!("../assets/tailwind.css");
const FTS_THEME_CSS: &str = include_str!("../assets/fts-theme.css");

// Blitz CSS workarounds
const BLITZ_FIXES: &str = r#"
input, textarea, select, button { cursor: auto !important; }
input:disabled, textarea:disabled, button:disabled { cursor: not-allowed !important; }
:root { color-scheme: dark; }
"#;

/// Root component for the UI test panel.
#[component]
pub fn UiTestPanel() -> Element {
    let name = use_signal(|| String::new());
    let email = use_signal(|| String::new());
    let selected_role = use_signal(|| String::new());
    let mut submitted = use_signal(|| false);

    rsx! {
        document::Style { {TAILWIND_CSS} }
        document::Style { {FTS_THEME_CSS} }
        document::Style { {BLITZ_FIXES} }

        div { class: "dark min-h-full bg-background text-foreground p-6 font-sans flex flex-col items-center",

            div { class: "w-full max-w-md",

                Heading { level: HeadingLevel::H2, "FTS UI Component Test" }

                div { class: "mt-4",
                    Card {
                        div { class: "p-6 flex flex-col gap-4",
                        Heading { level: HeadingLevel::H3, "Registration Form" }

                        Text { variant: TextVariant::Muted,
                            "Test card with form inputs, dropdown, and button."
                        }

                        div { class: "flex flex-col gap-2",
                            Label { "Name" }
                            Input {
                                value: name,
                                placeholder: "Enter your name",
                            }
                        }

                        div { class: "flex flex-col gap-2",
                            Label { "Email" }
                            Input {
                                value: email,
                                placeholder: "you@example.com",
                            }
                        }

                        div { class: "flex flex-col gap-2",
                            Label { "Role" }
                            Select {
                                value: selected_role,
                                placeholder: "Select a role...",
                                SelectTrigger {}
                                SelectContent {
                                    SelectItem { value: "engineer", "Engineer" }
                                    SelectItem { value: "designer", "Designer" }
                                    SelectItem { value: "producer", "Producer" }
                                    SelectItem { value: "musician", "Musician" }
                                }
                            }
                        }

                        Button {
                            variant: ButtonVariant::Primary,
                            on_click: move |_| {
                                tracing::info!(
                                    name = %name.read(),
                                    email = %email.read(),
                                    role = %selected_role.read(),
                                    "Form submitted"
                                );
                                submitted.set(true);
                            },
                            "Submit"
                        }

                        if *submitted.read() {
                            Alert {
                                variant: AlertVariant::Default,
                                "Submitted: {name} ({email}) — Role: {selected_role}"
                            }
                        }
                    }
                }
            }

                div { class: "mt-4 flex gap-2 flex-wrap",
                    Button { variant: ButtonVariant::Primary, "Primary" }
                    Button { variant: ButtonVariant::Secondary, "Secondary" }
                    Button { variant: ButtonVariant::Outline, "Outline" }
                    Button { variant: ButtonVariant::Destructive, "Destructive" }
                    Button { variant: ButtonVariant::Ghost, "Ghost" }
                }

                div { class: "mt-4 flex gap-2 items-center flex-wrap",
                    Badge { variant: BadgeVariant::Default, "Default" }
                    Badge { variant: BadgeVariant::Secondary, "Secondary" }
                    Badge { variant: BadgeVariant::Outline, "Outline" }
                    Badge { variant: BadgeVariant::Destructive, "Error" }
                }
            }
        }
    }
}

/// Panel definition for the UI test panel.
pub fn panel_def() -> PanelDef {
    PanelDef {
        id: "FTS_UI_TEST",
        title: "UI Component Test",
        component: PanelComponent::from_fn_ptr(UiTestPanel as fn() -> _ as *const ()),
        default_dock: DockPosition::Floating,
        default_size: (600.0, 600.0),
        toggle_action: Some("FTS_UI_TEST_TOGGLE"),
    }
}

/// Action definition for toggling the UI test panel.
pub fn action_def() -> ActionDef {
    ActionDef::new(
        "FTS_UI_TEST_TOGGLE",
        "FTS: Toggle UI Component Test Panel",
        || {
            reaper_dioxus::toggle_panel("FTS_UI_TEST");
        },
    )
    .in_menu()
}
