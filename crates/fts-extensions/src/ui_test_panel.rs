//! Reaper-Dioxus UI Component Test Panel
//!
//! Stress-tests fts-ui components (shadcn-ui clone) in the native
//! Dioxus/Blitz renderer inside REAPER's docker system.

use daw::module::{ActionDef, DockPosition, PanelComponent, PanelDef};
use reaper_dioxus::prelude::*;

use fts_ui::prelude::*;

/// Root component for the UI test panel.
#[component]
pub fn UiTestPanel() -> Element {
    let mut name = use_signal(|| String::new());
    let mut email = use_signal(|| String::new());
    let mut submitted = use_signal(|| false);

    rsx! {
        div {
            style: "padding: 16px; font-family: sans-serif; background: var(--background, #0a0a0a); color: var(--foreground, #fafafa); min-height: 100%;",

            Heading { level: HeadingLevel::H2, "FTS UI Component Test" }

            div { style: "margin-top: 16px;",
                Card {
                    div { style: "padding: 24px; display: flex; flex-direction: column; gap: 16px;",
                        Heading { level: HeadingLevel::H3, "Registration Form" }

                        Text { variant: TextVariant::Muted,
                            "Test card with form inputs, labels, and button."
                        }

                        div { style: "display: flex; flex-direction: column; gap: 8px;",
                            Label { "Name" }
                            Input {
                                value: name,
                                placeholder: "Enter your name",
                            }
                        }

                        div { style: "display: flex; flex-direction: column; gap: 8px;",
                            Label { "Email" }
                            Input {
                                value: email,
                                placeholder: "you@example.com",
                            }
                        }

                        Button {
                            variant: ButtonVariant::Primary,
                            on_click: move |_| {
                                tracing::info!(name = %name.read(), email = %email.read(), "Form submitted");
                                submitted.set(true);
                            },
                            "Submit"
                        }

                        if *submitted.read() {
                            Alert {
                                variant: AlertVariant::Default,
                                "Submitted: {name} ({email})"
                            }
                        }
                    }
                }
            }

            div { style: "margin-top: 16px; display: flex; gap: 8px;",
                Button { variant: ButtonVariant::Primary, "Primary" }
                Button { variant: ButtonVariant::Secondary, "Secondary" }
                Button { variant: ButtonVariant::Outline, "Outline" }
                Button { variant: ButtonVariant::Destructive, "Destructive" }
                Button { variant: ButtonVariant::Ghost, "Ghost" }
            }

            div { style: "margin-top: 16px; display: flex; gap: 8px; align-items: center;",
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
        default_size: (600.0, 500.0),
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
