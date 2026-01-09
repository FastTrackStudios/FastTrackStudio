//! Native test app for Blitz renderer with Lumen Blocks components
//!
//! This app demonstrates using lumen-blocks components in Dioxus with the native (Blitz) renderer.

use dioxus_native::prelude::*;
use lumen_blocks::components::{
    button::{Button, ButtonSize, ButtonVariant},
    checkbox::Checkbox,
    input::{Input, InputSize, InputVariant},
    progress::Progress,
    switch::Switch,
};
use lucide_dioxus::{Heart, Mail, Search, Settings};

fn main() {
    dioxus_native::launch(app);
}

// Tailwind CSS - included at compile time
const TAILWIND_CSS: &str = include_str!("../assets/tailwind.compiled.css");

fn app() -> Element {
    let mut count = use_signal(|| 0);
    let mut input_value = use_signal(|| String::new());
    let switch_enabled = use_signal(|| false);
    let checkbox_checked = use_signal(|| false);
    let mut loading = use_signal(|| false);

    // Progress values (as signals)
    let progress_25 = use_signal(|| 25.0);
    let progress_50 = use_signal(|| 50.0);
    let progress_75 = use_signal(|| 75.0);
    let progress_100 = use_signal(|| 100.0);

    rsx! {
        style { {TAILWIND_CSS} }

        div {
            class: "min-h-screen bg-background p-8",

            // Header
            div {
                class: "max-w-4xl mx-auto",

                h1 {
                    class: "text-3xl font-bold text-foreground mb-2",
                    "Lumen Blocks Demo"
                }

                p {
                    class: "text-muted-foreground mb-8",
                    "Accessible, styled components for Dioxus inspired by shadcn/ui"
                }

                // Button Section
                div {
                    class: "mb-8 p-6 rounded-lg border border-border bg-card",

                    h2 {
                        class: "text-xl font-semibold text-card-foreground mb-4",
                        "Buttons"
                    }

                    // Button variants
                    div {
                        class: "flex flex-wrap gap-3 mb-4",

                        Button {
                            variant: ButtonVariant::Primary,
                            on_click: move |_| count += 1,
                            "Primary ({count})"
                        }

                        Button {
                            variant: ButtonVariant::Secondary,
                            "Secondary"
                        }

                        Button {
                            variant: ButtonVariant::Outline,
                            "Outline"
                        }

                        Button {
                            variant: ButtonVariant::Ghost,
                            "Ghost"
                        }

                        Button {
                            variant: ButtonVariant::Link,
                            "Link"
                        }

                        Button {
                            variant: ButtonVariant::Destructive,
                            "Destructive"
                        }
                    }

                    // Button sizes
                    div {
                        class: "flex flex-wrap items-center gap-3 mb-4",

                        Button {
                            size: ButtonSize::Small,
                            "Small"
                        }

                        Button {
                            size: ButtonSize::Medium,
                            "Medium"
                        }

                        Button {
                            size: ButtonSize::Large,
                            "Large"
                        }
                    }

                    // Button with icons
                    div {
                        class: "flex flex-wrap gap-3 mb-4",

                        Button {
                            icon_left: rsx! { Mail { class: "w-4 h-4" } },
                            "Login with Email"
                        }

                        Button {
                            variant: ButtonVariant::Outline,
                            icon_right: rsx! { Settings { class: "w-4 h-4" } },
                            "Settings"
                        }

                        Button {
                            is_icon_button: true,
                            aria_label: "Favorite",
                            Heart { class: "w-4 h-4" }
                        }
                    }

                    // Loading button
                    div {
                        class: "flex gap-3",

                        Button {
                            loading: loading(),
                            on_click: move |_| {
                                loading.set(true);
                                // In a real app, you'd reset after async operation
                            },
                            "Submit"
                        }

                        Button {
                            variant: ButtonVariant::Outline,
                            disabled: loading(),
                            on_click: move |_| loading.set(false),
                            "Reset"
                        }
                    }
                }

                // Input Section
                div {
                    class: "mb-8 p-6 rounded-lg border border-border bg-card",

                    h2 {
                        class: "text-xl font-semibold text-card-foreground mb-4",
                        "Inputs"
                    }

                    div {
                        class: "space-y-4 max-w-md",

                        // Basic input
                        Input {
                            placeholder: "Enter your name...",
                            value: input_value(),
                            on_input: move |e: FormEvent| input_value.set(e.value()),
                            full_width: true,
                        }

                        // Input with icon
                        Input {
                            placeholder: "Search...",
                            icon_left: rsx! { Search { class: "w-4 h-4" } },
                            full_width: true,
                        }

                        // Input sizes
                        div {
                            class: "flex flex-col gap-2",

                            Input {
                                size: InputSize::Small,
                                placeholder: "Small input",
                                full_width: true,
                            }

                            Input {
                                size: InputSize::Medium,
                                placeholder: "Medium input",
                                full_width: true,
                            }

                            Input {
                                size: InputSize::Large,
                                placeholder: "Large input",
                                full_width: true,
                            }
                        }

                        // Error variant
                        Input {
                            variant: InputVariant::Error,
                            placeholder: "Error state",
                            full_width: true,
                        }

                        // Disabled
                        Input {
                            disabled: true,
                            placeholder: "Disabled input",
                            full_width: true,
                        }
                    }
                }

                // Toggle Controls Section
                div {
                    class: "mb-8 p-6 rounded-lg border border-border bg-card",

                    h2 {
                        class: "text-xl font-semibold text-card-foreground mb-4",
                        "Toggle Controls"
                    }

                    div {
                        class: "space-y-4",

                        // Switch
                        div {
                            class: "flex items-center gap-3",

                            Switch {
                                checked: switch_enabled,
                            }

                            span {
                                class: "text-foreground",
                                if switch_enabled() { "Enabled" } else { "Disabled" }
                            }
                        }

                        // Checkbox
                        div {
                            class: "flex items-center gap-3",

                            Checkbox {
                                checked: checkbox_checked,
                            }

                            span {
                                class: "text-foreground",
                                if checkbox_checked() { "Checked" } else { "Unchecked" }
                            }
                        }
                    }
                }

                // Progress Section
                div {
                    class: "mb-8 p-6 rounded-lg border border-border bg-card",

                    h2 {
                        class: "text-xl font-semibold text-card-foreground mb-4",
                        "Progress"
                    }

                    div {
                        class: "space-y-4",

                        Progress {
                            value: progress_25,
                        }

                        Progress {
                            value: progress_50,
                        }

                        Progress {
                            value: progress_75,
                        }

                        Progress {
                            value: progress_100,
                        }
                    }
                }

                // State Display
                div {
                    class: "p-6 rounded-lg border border-border bg-muted",

                    h2 {
                        class: "text-xl font-semibold text-foreground mb-4",
                        "Current State"
                    }

                    div {
                        class: "font-mono text-sm text-muted-foreground space-y-1",

                        p { "count: {count}" }
                        p { "input_value: \"{input_value}\"" }
                        p { "switch_enabled: {switch_enabled}" }
                        p { "checkbox_checked: {checkbox_checked}" }
                        p { "loading: {loading}" }
                    }
                }
            }
        }
    }
}
