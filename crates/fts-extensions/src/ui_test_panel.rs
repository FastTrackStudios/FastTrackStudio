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

// Blitz CSS workarounds:
// - Force dark mode vars (Blitz has no @media prefers-color-scheme)
// - Fix disabled cursor applied unconditionally
const BLITZ_FIXES: &str = r#"
:root {
    --background: oklch(0.145 0 0);
    --foreground: oklch(0.985 0 0);
    --card: oklch(0.145 0 0);
    --card-foreground: oklch(0.985 0 0);
    --popover: oklch(0.145 0 0);
    --popover-foreground: oklch(0.985 0 0);
    --primary: oklch(0.985 0 0);
    --primary-foreground: oklch(0.205 0 0);
    --secondary: oklch(0.269 0 0);
    --secondary-foreground: oklch(0.985 0 0);
    --muted: oklch(0.269 0 0);
    --muted-foreground: oklch(0.708 0 0);
    --accent: oklch(0.269 0 0);
    --accent-foreground: oklch(0.985 0 0);
    --destructive: oklch(0.5058 0.2066 27.85);
    --border: oklch(0.269 0 0);
    --input: oklch(0.269 0 0);
    --ring: oklch(0.439 0 0);
    color-scheme: dark;
}
input, textarea, select, button { cursor: auto !important; }
input:disabled, textarea:disabled, button:disabled { cursor: not-allowed !important; }
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

        div { class: "dark min-h-full bg-background text-foreground p-4 font-sans",

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

            div { class: "mt-4 flex gap-2",
                Button { variant: ButtonVariant::Primary, "Primary" }
                Button { variant: ButtonVariant::Secondary, "Secondary" }
                Button { variant: ButtonVariant::Outline, "Outline" }
                Button { variant: ButtonVariant::Destructive, "Destructive" }
                Button { variant: ButtonVariant::Ghost, "Ghost" }
            }

            div { class: "mt-4 flex gap-2 items-center",
                Badge { variant: BadgeVariant::Default, "Default" }
                Badge { variant: BadgeVariant::Secondary, "Secondary" }
                Badge { variant: BadgeVariant::Outline, "Outline" }
                Badge { variant: BadgeVariant::Destructive, "Error" }
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
