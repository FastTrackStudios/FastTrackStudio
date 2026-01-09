use dioxus_native::prelude::*;
use dioxus_native::{use_wgpu, use_post_processor};
use lumen_blocks::components::accordion::{
    Accordion, AccordionContent, AccordionItem, AccordionTrigger,
};
use lumen_blocks::components::avatar::{Avatar, AvatarFallback, AvatarImage};
use lumen_blocks::components::button::{Button, ButtonSize, ButtonVariant};
use lumen_blocks::components::checkbox::{Checkbox, CheckboxSize};
use lumen_blocks::components::collapsible::{Collapsible, CollapsibleContent, CollapsibleTrigger};
use lumen_blocks::components::input::{Input, InputSize, InputVariant};
use lumen_blocks::components::progress::{Progress, ProgressSize, ProgressVariant};
use lumen_blocks::components::switch::{Switch, SwitchSize};
use lumen_blocks::components::toast::{use_toast, ToastOptions, ToastProvider};
use lucide_dioxus::{Heart, Mail, Plus, Settings, Trash2, User};

mod bunnymark;
mod glow;
mod interactive;

use bunnymark::{BunnyMessage, BunnymarkPaintSource};
use glow::{GlowPostProcessor, GlowState};
use interactive::{InteractiveMessage, InteractivePaintSource};

fn main() {
    dioxus_native::launch(app);
}

fn app() -> Element {
    let mut selected_tab = use_signal(|| 0);

    // Bloom effect state
    let mut bloom_enabled = use_signal(|| true);
    let mut bloom_threshold = use_signal(|| 0.3f32);  // Lower = more bloom
    let mut bloom_intensity = use_signal(|| 2.0f32);  // Bloom strength
    let mut bloom_radius = use_signal(|| 10.0f32);    // Affects soft knee

    // Create shared bloom state that can be updated dynamically
    let bloom_state = use_hook(|| GlowState::new());

    // Apply bloom post-processor with shared state
    {
        let state = bloom_state.clone();
        use_post_processor(move || GlowPostProcessor::with_state(state));
    }

    // Update bloom state whenever signals change
    use_effect(move || {
        bloom_state.set_enabled(bloom_enabled());
        bloom_state.set_threshold(bloom_threshold());
        bloom_state.set_intensity(bloom_intensity());
        bloom_state.set_radius(bloom_radius());
    });

    // Forms tab state - all signals defined at app level
    let mut name_value = use_signal(|| String::new());
    let mut email_value = use_signal(|| String::new());
    let mut password_value = use_signal(|| String::new());
    let mut checkbox_terms = use_signal(|| false);
    let mut checkbox_newsletter = use_signal(|| true);
    let checkbox_disabled = use_signal(|| false);
    let checkbox_disabled_checked = use_signal(|| true);
    let mut switch_notifications = use_signal(|| false);
    let mut switch_dark_mode = use_signal(|| true);
    let checkbox_small = use_signal(|| false);
    let checkbox_medium = use_signal(|| false);
    let checkbox_large = use_signal(|| false);
    let switch_small = use_signal(|| false);
    let switch_medium = use_signal(|| true);

    // Interactive tab state
    let mut count = use_signal(|| 0);
    let mut loading = use_signal(|| false);
    let mut progress = use_signal(|| 33.0);
    let progress_success = use_signal(|| 66.0);
    let progress_warning = use_signal(|| 45.0);
    let progress_destructive = use_signal(|| 25.0);

    rsx! {
        style { {TAILWIND_CSS} }
        ToastProvider {
            div { class: "min-h-screen bg-background text-foreground p-8",
                div { class: "max-w-4xl mx-auto",
                    // Header
                    h1 { class: "text-4xl font-bold mb-2", "Lumen Blocks Demo" }
                    p { class: "text-muted-foreground mb-4", "Testing shadcn-inspired components with dioxus-native" }

                    // Bloom Effect Controls
                    div { class: "flex flex-wrap items-center gap-4 mb-6 p-4 rounded-lg bg-card border border-border",
                        div { class: "flex items-center gap-2 shrink-0",
                            Switch {
                                checked: bloom_enabled,
                                on_checked_change: move |new_state| bloom_enabled.set(new_state),
                                size: SwitchSize::Small,
                            }
                            span { class: "text-sm font-medium", "Bloom Effect" }
                        }
                        div { class: "flex flex-wrap items-center gap-4 pl-4 border-l border-border",
                            div { class: "flex items-center gap-2",
                                label { class: "text-sm text-muted-foreground whitespace-nowrap", "Threshold" }
                                Input {
                                    size: InputSize::Small,
                                    value: format!("{:.2}", bloom_threshold()),
                                    on_input: move |e: FormEvent| {
                                        if let Ok(v) = e.value().parse::<f32>() {
                                            bloom_threshold.set(v.clamp(0.0, 1.0));
                                        }
                                    },
                                }
                            }
                            div { class: "flex items-center gap-2",
                                label { class: "text-sm text-muted-foreground whitespace-nowrap", "Intensity" }
                                Input {
                                    size: InputSize::Small,
                                    value: format!("{:.2}", bloom_intensity()),
                                    on_input: move |e: FormEvent| {
                                        if let Ok(v) = e.value().parse::<f32>() {
                                            bloom_intensity.set(v.clamp(0.0, 10.0));
                                        }
                                    },
                                }
                            }
                            div { class: "flex items-center gap-2",
                                label { class: "text-sm text-muted-foreground whitespace-nowrap", "Softness" }
                                Input {
                                    size: InputSize::Small,
                                    value: format!("{:.0}", bloom_radius()),
                                    on_input: move |e: FormEvent| {
                                        if let Ok(v) = e.value().parse::<f32>() {
                                            bloom_radius.set(v.clamp(1.0, 20.0));
                                        }
                                    },
                                }
                            }
                        }
                    }

                    // Tabs
                    div { class: "flex gap-2 mb-6 border-b border-border pb-2",
                        button {
                            class: if selected_tab() == 0 { "tab tab-active" } else { "tab" },
                            onclick: move |_| selected_tab.set(0),
                            "Buttons"
                        }
                        button {
                            class: if selected_tab() == 1 { "tab tab-active" } else { "tab" },
                            onclick: move |_| selected_tab.set(1),
                            "Forms"
                        }
                        button {
                            class: if selected_tab() == 2 { "tab tab-active" } else { "tab" },
                            onclick: move |_| selected_tab.set(2),
                            "Interactive"
                        }
                        button {
                            class: if selected_tab() == 3 { "tab tab-active" } else { "tab" },
                            onclick: move |_| selected_tab.set(3),
                            "Disclosure"
                        }
                        button {
                            class: if selected_tab() == 4 { "tab tab-active" } else { "tab" },
                            onclick: move |_| selected_tab.set(4),
                            "Feedback"
                        }
                        button {
                            class: if selected_tab() == 5 { "tab tab-active" } else { "tab" },
                            onclick: move |_| selected_tab.set(5),
                            "WGPU"
                        }
                    }

                    // Tab Content - Buttons
                    if selected_tab() == 0 {
                        div { class: "space-y-8",
                            // Button Variants
                            div { class: "card",
                                h2 { class: "text-2xl font-semibold mb-4", "Button Variants" }
                                div { class: "flex flex-wrap gap-3",
                                    Button { variant: ButtonVariant::Primary, "Primary" }
                                    Button { variant: ButtonVariant::Secondary, "Secondary" }
                                    Button { variant: ButtonVariant::Outline, "Outline" }
                                    Button { variant: ButtonVariant::Ghost, "Ghost" }
                                    Button { variant: ButtonVariant::Link, "Link" }
                                    Button { variant: ButtonVariant::Destructive, "Destructive" }
                                }
                            }

                            // Button Sizes
                            div { class: "card",
                                h2 { class: "text-2xl font-semibold mb-4", "Button Sizes" }
                                div { class: "flex items-center gap-3",
                                    Button { variant: ButtonVariant::Primary, size: ButtonSize::Small, "Small" }
                                    Button { variant: ButtonVariant::Primary, size: ButtonSize::Medium, "Medium" }
                                    Button { variant: ButtonVariant::Primary, size: ButtonSize::Large, "Large" }
                                }
                            }

                            // Buttons with Icons
                            div { class: "card",
                                h2 { class: "text-2xl font-semibold mb-4", "Buttons with Icons" }
                                div { class: "flex flex-wrap gap-3",
                                    Button {
                                        variant: ButtonVariant::Primary,
                                        icon_left: rsx! { Mail { class: "w-4 h-4" } },
                                        "Email"
                                    }
                                    Button {
                                        variant: ButtonVariant::Secondary,
                                        icon_right: rsx! { Plus { class: "w-4 h-4" } },
                                        "Add Item"
                                    }
                                    Button {
                                        variant: ButtonVariant::Outline,
                                        icon_left: rsx! { Settings { class: "w-4 h-4" } },
                                        "Settings"
                                    }
                                    Button {
                                        variant: ButtonVariant::Destructive,
                                        icon_left: rsx! { Trash2 { class: "w-4 h-4" } },
                                        "Delete"
                                    }
                                }
                            }

                            // Icon Buttons
                            div { class: "card",
                                h2 { class: "text-2xl font-semibold mb-4", "Icon Buttons" }
                                div { class: "flex gap-3",
                                    Button {
                                        variant: ButtonVariant::Primary,
                                        is_icon_button: true,
                                        aria_label: "Add",
                                        Plus { class: "w-4 h-4" }
                                    }
                                    Button {
                                        variant: ButtonVariant::Secondary,
                                        is_icon_button: true,
                                        aria_label: "User",
                                        User { class: "w-4 h-4" }
                                    }
                                    Button {
                                        variant: ButtonVariant::Outline,
                                        is_icon_button: true,
                                        aria_label: "Like",
                                        Heart { class: "w-4 h-4" }
                                    }
                                    Button {
                                        variant: ButtonVariant::Ghost,
                                        is_icon_button: true,
                                        aria_label: "Settings",
                                        Settings { class: "w-4 h-4" }
                                    }
                                }
                            }

                            // Disabled & Full Width
                            div { class: "card",
                                h2 { class: "text-2xl font-semibold mb-4", "States" }
                                div { class: "space-y-3",
                                    Button { variant: ButtonVariant::Primary, disabled: true, "Disabled Button" }
                                    Button { variant: ButtonVariant::Primary, full_width: true, "Full Width Button" }
                                }
                            }
                        }
                    }

                    // Tab Content - Forms
                    if selected_tab() == 1 {
                        div { class: "space-y-8",
                            // Registration Form
                            div { class: "card",
                                h2 { class: "text-2xl font-semibold mb-4", "Registration Form" }
                                div { class: "space-y-4",
                                    // Name field
                                    div { class: "form-group",
                                        label { class: "label", "Full Name" }
                                        Input {
                                            placeholder: "John Doe",
                                            value: name_value(),
                                            on_input: move |e: FormEvent| name_value.set(e.value()),
                                        }
                                    }

                                    // Email field
                                    div { class: "form-group",
                                        label { class: "label", "Email Address" }
                                        Input {
                                            input_type: "email".to_string(),
                                            placeholder: "you@example.com",
                                            value: email_value(),
                                            on_input: move |e: FormEvent| email_value.set(e.value()),
                                        }
                                    }

                                    // Password field
                                    div { class: "form-group",
                                        label { class: "label", "Password" }
                                        Input {
                                            input_type: "password".to_string(),
                                            placeholder: "••••••••",
                                            value: password_value(),
                                            on_input: move |e: FormEvent| password_value.set(e.value()),
                                        }
                                    }

                                    // Checkboxes for terms and newsletter
                                    div { class: "space-y-3 mt-4",
                                        div { class: "flex items-center gap-2",
                                            Checkbox {
                                                checked: checkbox_terms,
                                                on_checked_change: move |new_state| checkbox_terms.set(new_state),
                                                aria_label: Some(String::from("Accept terms")),
                                            }
                                            label { class: "text-sm", "I agree to the Terms of Service and Privacy Policy" }
                                        }
                                        div { class: "flex items-center gap-2",
                                            Checkbox {
                                                checked: checkbox_newsletter,
                                                on_checked_change: move |new_state| checkbox_newsletter.set(new_state),
                                                aria_label: Some(String::from("Subscribe newsletter")),
                                            }
                                            label { class: "text-sm", "Subscribe to newsletter for updates" }
                                        }
                                    }

                                    // Submit button
                                    div { class: "mt-6",
                                        Button {
                                            variant: ButtonVariant::Primary,
                                            full_width: true,
                                            disabled: !checkbox_terms(),
                                            "Create Account"
                                        }
                                    }

                                    // Form state display
                                    if !name_value().is_empty() || !email_value().is_empty() {
                                        div { class: "mt-4 p-3 rounded bg-muted text-sm",
                                            p { class: "font-medium mb-2", "Form State:" }
                                            p { "Name: {name_value}" }
                                            p { "Email: {email_value}" }
                                            p { "Terms accepted: {checkbox_terms}" }
                                            p { "Newsletter: {checkbox_newsletter}" }
                                        }
                                    }
                                }
                            }

                            // Input Variants
                            div { class: "card",
                                h2 { class: "text-2xl font-semibold mb-4", "Input Variants & Sizes" }
                                div { class: "space-y-4",
                                    h3 { class: "text-lg font-medium", "Sizes" }
                                    div { class: "space-y-3",
                                        Input { size: InputSize::Small, placeholder: "Small input" }
                                        Input { size: InputSize::Medium, placeholder: "Medium input (default)" }
                                        Input { size: InputSize::Large, placeholder: "Large input" }
                                    }

                                    h3 { class: "text-lg font-medium mt-6", "States" }
                                    div { class: "space-y-3",
                                        Input { placeholder: "Default input" }
                                        Input { variant: InputVariant::Error, placeholder: "Error state input" }
                                        Input { disabled: true, placeholder: "Disabled input" }
                                        Input { readonly: true, value: "Read-only input".to_string() }
                                    }
                                }
                            }

                            // Checkboxes
                            div { class: "card",
                                h2 { class: "text-2xl font-semibold mb-4", "Checkboxes" }
                                div { class: "space-y-4",
                                    h3 { class: "text-lg font-medium", "Sizes" }
                                    div { class: "flex items-center gap-4",
                                        div { class: "flex items-center gap-2",
                                            Checkbox {
                                                checked: checkbox_small,
                                                size: CheckboxSize::Small,
                                                aria_label: Some(String::from("Small")),
                                            }
                                            label { class: "text-sm", "Small" }
                                        }
                                        div { class: "flex items-center gap-2",
                                            Checkbox {
                                                checked: checkbox_medium,
                                                size: CheckboxSize::Medium,
                                                aria_label: Some(String::from("Medium")),
                                            }
                                            label { class: "text-sm", "Medium" }
                                        }
                                        div { class: "flex items-center gap-2",
                                            Checkbox {
                                                checked: checkbox_large,
                                                size: CheckboxSize::Large,
                                                aria_label: Some(String::from("Large")),
                                            }
                                            label { class: "text-sm", "Large" }
                                        }
                                    }

                                    h3 { class: "text-lg font-medium mt-4", "Disabled States" }
                                    div { class: "flex items-center gap-4",
                                        div { class: "flex items-center gap-2",
                                            Checkbox {
                                                checked: checkbox_disabled,
                                                disabled: true,
                                                aria_label: Some(String::from("Disabled unchecked")),
                                            }
                                            label { class: "text-sm text-muted-foreground", "Disabled unchecked" }
                                        }
                                        div { class: "flex items-center gap-2",
                                            Checkbox {
                                                checked: checkbox_disabled_checked,
                                                disabled: true,
                                                aria_label: Some(String::from("Disabled checked")),
                                            }
                                            label { class: "text-sm text-muted-foreground", "Disabled checked" }
                                        }
                                    }
                                }
                            }

                            // Switch
                            div { class: "card",
                                h2 { class: "text-2xl font-semibold mb-4", "Switch" }
                                div { class: "space-y-4",
                                    div { class: "flex items-center justify-between",
                                        div {
                                            p { class: "font-medium", "Push Notifications" }
                                            p { class: "text-sm text-muted-foreground", "Receive notifications about updates" }
                                        }
                                        Switch {
                                            checked: switch_notifications,
                                            on_checked_change: move |new_state| switch_notifications.set(new_state),
                                        }
                                    }
                                    div { class: "flex items-center justify-between",
                                        div {
                                            p { class: "font-medium", "Dark Mode" }
                                            p { class: "text-sm text-muted-foreground", "Use dark theme for the interface" }
                                        }
                                        Switch {
                                            checked: switch_dark_mode,
                                            on_checked_change: move |new_state| switch_dark_mode.set(new_state),
                                        }
                                    }

                                    h3 { class: "text-lg font-medium mt-4", "Sizes" }
                                    div { class: "flex items-center gap-6",
                                        div { class: "flex items-center gap-2",
                                            Switch {
                                                checked: switch_small,
                                                size: SwitchSize::Small,
                                            }
                                            span { class: "text-sm", "Small" }
                                        }
                                        div { class: "flex items-center gap-2",
                                            Switch {
                                                checked: switch_medium,
                                                size: SwitchSize::Medium,
                                            }
                                            span { class: "text-sm", "Medium" }
                                        }
                                        div { class: "flex items-center gap-2",
                                            Switch {
                                                checked: switch_dark_mode,
                                                on_checked_change: move |new_state| switch_dark_mode.set(new_state),
                                                size: SwitchSize::Large,
                                            }
                                            span { class: "text-sm", "Large" }
                                        }
                                    }
                                }
                            }
                        }
                    }

                    // Tab Content - Interactive
                    if selected_tab() == 2 {
                        div { class: "space-y-8",
                            // Counter
                            div { class: "card",
                                h2 { class: "text-2xl font-semibold mb-4", "Interactive Counter" }
                                div { class: "flex items-center gap-4",
                                    Button {
                                        variant: ButtonVariant::Outline,
                                        on_click: move |_| count -= 1,
                                        "-"
                                    }
                                    span { class: "text-4xl font-bold min-w-16 text-center", "{count}" }
                                    Button {
                                        variant: ButtonVariant::Outline,
                                        on_click: move |_| count += 1,
                                        "+"
                                    }
                                    Button {
                                        variant: ButtonVariant::Ghost,
                                        on_click: move |_| count.set(0),
                                        "Reset"
                                    }
                                }
                            }

                            // Loading Button
                            div { class: "card",
                                h2 { class: "text-2xl font-semibold mb-4", "Loading State" }
                                div { class: "flex gap-3",
                                    Button {
                                        variant: ButtonVariant::Primary,
                                        loading: loading(),
                                        on_click: move |_| loading.set(!loading()),
                                        if loading() { "Loading..." } else { "Toggle Loading" }
                                    }
                                    Button {
                                        variant: ButtonVariant::Secondary,
                                        loading: true,
                                        "Always Loading"
                                    }
                                }
                            }

                            // Progress Bar
                            div { class: "card",
                                h2 { class: "text-2xl font-semibold mb-4", "Progress Bar" }
                                div { class: "space-y-4",
                                    Progress { value: ReadSignal::from(progress) }

                                    div { class: "flex gap-2",
                                        Button {
                                            variant: ButtonVariant::Outline,
                                            size: ButtonSize::Small,
                                            on_click: move |_| {
                                                let new_val = (progress() - 10.0).max(0.0);
                                                progress.set(new_val);
                                            },
                                            "-10%"
                                        }
                                        Button {
                                            variant: ButtonVariant::Outline,
                                            size: ButtonSize::Small,
                                            on_click: move |_| {
                                                let new_val = (progress() + 10.0).min(100.0);
                                                progress.set(new_val);
                                            },
                                            "+10%"
                                        }
                                        Button {
                                            variant: ButtonVariant::Ghost,
                                            size: ButtonSize::Small,
                                            on_click: move |_| progress.set(33.0),
                                            "Reset"
                                        }
                                    }

                                    h3 { class: "text-lg font-medium mt-4", "Progress Sizes" }
                                    div { class: "space-y-2",
                                        Progress { value: ReadSignal::from(progress), size: ProgressSize::Small }
                                        Progress { value: ReadSignal::from(progress), size: ProgressSize::Medium }
                                        Progress { value: ReadSignal::from(progress), size: ProgressSize::Large }
                                    }

                                    h3 { class: "text-lg font-medium mt-4", "Progress Variants" }
                                    div { class: "space-y-2",
                                        Progress { value: ReadSignal::from(progress), variant: ProgressVariant::Default }
                                        Progress { value: ReadSignal::from(progress_success), variant: ProgressVariant::Success }
                                        Progress { value: ReadSignal::from(progress_warning), variant: ProgressVariant::Warning }
                                        Progress { value: ReadSignal::from(progress_destructive), variant: ProgressVariant::Destructive }
                                    }

                                    h3 { class: "text-lg font-medium mt-4", "With Percentage" }
                                    Progress { value: ReadSignal::from(progress), show_percentage: true, aria_label: Some("Upload progress".to_string()) }

                                    p { class: "text-sm text-muted-foreground mt-2", "Progress: {progress:.0}%" }
                                }
                            }

                            // Avatars
                            div { class: "card",
                                h2 { class: "text-2xl font-semibold mb-4", "Avatars" }
                                div { class: "flex items-center gap-4",
                                    Avatar {
                                        AvatarImage { src: "https://github.com/shadcn.png", alt: "User avatar" }
                                        AvatarFallback { "CN" }
                                    }
                                    Avatar {
                                        AvatarFallback { "JD" }
                                    }
                                    Avatar {
                                        AvatarFallback { "AB" }
                                    }
                                    Avatar { class: "h-12 w-12",
                                        AvatarImage { src: "https://github.com/vercel.png", alt: "Vercel" }
                                        AvatarFallback { "VC" }
                                    }
                                }
                                p { class: "text-sm text-muted-foreground mt-2",
                                    "Avatars with image fallback support"
                                }
                            }
                        }
                    }

                    // Tab Content - Disclosure
                    if selected_tab() == 3 {
                        div { class: "space-y-8",
                            // Accordion
                            div { class: "card",
                                h2 { class: "text-2xl font-semibold mb-4", "Accordion" }
                                Accordion {
                                    AccordionItem { index: 0,
                                        AccordionTrigger { "Is it accessible?" }
                                        AccordionContent {
                                            "Yes. It adheres to the WAI-ARIA design pattern."
                                        }
                                    }
                                    AccordionItem { index: 1,
                                        AccordionTrigger { "Is it styled?" }
                                        AccordionContent {
                                            "Yes. It comes with default styles that match the other components' aesthetic."
                                        }
                                    }
                                    AccordionItem { index: 2,
                                        AccordionTrigger { "Is it animated?" }
                                        AccordionContent {
                                            "Yes. It's animated by default, but you can disable it if you prefer."
                                        }
                                    }
                                }
                            }

                            // Accordion with multiple open
                            div { class: "card",
                                h2 { class: "text-2xl font-semibold mb-4", "Accordion (Multiple Open)" }
                                Accordion { allow_multiple_open: true,
                                    AccordionItem { index: 0,
                                        AccordionTrigger { "What is Dioxus?" }
                                        AccordionContent {
                                            "Dioxus is a portable, performant, and ergonomic framework for building cross-platform user interfaces in Rust."
                                        }
                                    }
                                    AccordionItem { index: 1,
                                        AccordionTrigger { "What is Lumen Blocks?" }
                                        AccordionContent {
                                            "Lumen Blocks is a shadcn-inspired component library for Dioxus, providing beautiful, accessible components."
                                        }
                                    }
                                    AccordionItem { index: 2,
                                        AccordionTrigger { "How do I use it?" }
                                        AccordionContent {
                                            "Add lumen-blocks as a dependency and import the components you need. Check the documentation for detailed examples."
                                        }
                                    }
                                }
                            }

                            // Collapsible
                            div { class: "card",
                                h2 { class: "text-2xl font-semibold mb-4", "Collapsible" }
                                Collapsible {
                                    CollapsibleTrigger { "Click to expand" }
                                    CollapsibleContent {
                                        p {
                                            "This is the collapsible content. It can contain any elements you want."
                                        }
                                        p { class: "mt-2",
                                            "Collapsibles are great for FAQs, settings panels, and more."
                                        }
                                    }
                                }
                            }
                        }
                    }

                    // Tab Content - Feedback
                    if selected_tab() == 4 {
                        {feedback_content()}
                    }

                    // Tab Content - WGPU
                    if selected_tab() == 5 {
                        div { class: "space-y-8",
                            // Interactive Canvas Demo
                            div { class: "card",
                                h2 { class: "text-2xl font-semibold mb-4", "Interactive Canvas" }
                                p { class: "text-muted-foreground mb-4",
                                    "Click the canvas to focus, then use WASD/Arrow keys to move. Hold Space to emit particles. Click anywhere in the canvas to spawn particles."
                                }
                                InteractiveCanvas {}
                            }

                            // Bunnymark Demo
                            div { class: "card",
                                h2 { class: "text-2xl font-semibold mb-4", "Bunnymark Demo" }
                                p { class: "text-muted-foreground mb-4",
                                    "Classic GPU benchmark. Click the button to spawn more bouncing bunnies!"
                                }
                                BunnymarkCanvas {}
                            }
                        }
                    }
                }
            }
        }
    }
}

#[component]
fn InteractiveCanvas() -> Element {
    // Use use_hook to create paint source and sender ONLY ONCE
    // This avoids the bug where re-renders create new senders to unregistered paint sources
    let sender = use_hook(|| {
        std::rc::Rc::new(std::cell::RefCell::new(None::<std::sync::mpsc::Sender<InteractiveMessage>>))
    });

    let paint_source_id = {
        let sender_cell = sender.clone();
        use_wgpu(move || {
            let paint_source = InteractivePaintSource::new();
            *sender_cell.borrow_mut() = Some(paint_source.sender());
            paint_source
        })
    };

    // Get the sender (it's populated after use_wgpu runs)
    let sender = sender.borrow().clone().expect("Sender should be initialized");

    // Track focus state for visual feedback
    let mut is_focused = use_signal(|| false);

    // Store mounted element for programmatic focus
    let mut canvas_element: Signal<Option<std::rc::Rc<MountedData>>> = use_signal(|| None);

    // Trigger to request focus (set to true to request focus)
    let mut request_focus = use_signal(|| false);

    // Render trigger - increment to force re-render and process messages
    let mut render_trigger = use_signal(|| 0u64);

    // Effect to handle focus requests outside of event handlers
    use_effect(move || {
        if request_focus() {
            request_focus.set(false); // Reset immediately
            if let Some(ref element) = *canvas_element.read() {
                let element = element.clone();
                spawn(async move {
                    println!("[Effect] Calling set_focus(true)");
                    let _ = element.set_focus(true).await;
                });
            }
        }
    });

    // Clone sender for keyboard/mouse events
    let keydown_sender = sender.clone();
    let keyup_sender = sender.clone();
    let click_sender = sender.clone();

    // Additional senders for button controls
    let up_sender = sender.clone();
    let down_sender = sender.clone();
    let left_sender = sender.clone();
    let right_sender = sender.clone();
    let spawn_sender = sender;

    rsx!(
        div { class: "space-y-4",
            // Interactive canvas container - focusable for keyboard input
            div {
                class: if is_focused() {
                    "rounded-lg overflow-hidden border-2 border-primary"
                } else {
                    "rounded-lg overflow-hidden border border-border"
                },
                style: "width: 800px; height: 400px; display: grid; outline: none;",
                tabindex: "0",
                // Store element reference for programmatic focus
                onmounted: move |evt| {
                    canvas_element.set(Some(evt.data()));
                },
                // Track focus state for visual feedback (use onfocus/onblur, not focusin/focusout)
                onfocus: move |_| {
                    println!("[Canvas] onfocus fired!");
                    is_focused.set(true);
                },
                onblur: move |_| {
                    println!("[Canvas] onblur fired!");
                    is_focused.set(false);
                },
                // Keyboard events - use key() which IS implemented
                // Convert Key enum to normalized string for the paint source
                onkeydown: move |evt| {
                    let key_str = key_to_string(evt.key());
                    println!("[Canvas] onkeydown: key={:?}, normalized=\"{}\"", evt.key(), key_str);
                    if !key_str.is_empty() {
                        let _ = keydown_sender.send(InteractiveMessage::KeyDown(key_str));
                        render_trigger.set(render_trigger() + 1); // Force re-render to process message
                    }
                },
                onkeyup: move |evt| {
                    let key_str = key_to_string(evt.key());
                    println!("[Canvas] onkeyup: key={:?}, normalized=\"{}\"", evt.key(), key_str);
                    if !key_str.is_empty() {
                        let _ = keyup_sender.send(InteractiveMessage::KeyUp(key_str));
                        render_trigger.set(render_trigger() + 1); // Force re-render to process message
                    }
                },
                // Mouse click - use client_coordinates() which IS implemented
                onclick: move |evt| {
                    println!("[Canvas] onclick fired at ({}, {})", evt.client_coordinates().x, evt.client_coordinates().y);

                    // Request focus via effect (avoids RefCell borrow conflict)
                    request_focus.set(true);
                    is_focused.set(true);

                    let coords = evt.client_coordinates();
                    let _ = click_sender.send(InteractiveMessage::Click {
                        x: coords.x as f32,
                        y: coords.y as f32,
                    });
                    render_trigger.set(render_trigger() + 1); // Force re-render to process message
                },
                canvas {
                    id: "interactive-canvas",
                    "src": "{paint_source_id}"
                }
            }

            // Fallback button controls (for testing if keyboard events don't work)
            div { class: "flex gap-2 items-center",
                span { class: "text-sm text-muted-foreground mr-2", "Controls:" }
                // Focus button for testing
                button {
                    class: "px-3 py-2 text-sm font-medium rounded bg-secondary text-secondary-foreground cursor-pointer",
                    onclick: move |_| {
                        println!("[Focus Button] clicked");
                        // Request focus via effect (avoids RefCell borrow conflict)
                        request_focus.set(true);
                        is_focused.set(true);
                    },
                    "Focus Canvas"
                }
                button {
                    class: "px-3 py-2 text-sm font-medium rounded border border-border bg-transparent text-foreground cursor-pointer",
                    onclick: move |_| {
                        let _ = up_sender.send(InteractiveMessage::MoveUp);
                        render_trigger.set(render_trigger() + 1);
                    },
                    "↑"
                }
                button {
                    class: "px-3 py-2 text-sm font-medium rounded border border-border bg-transparent text-foreground cursor-pointer",
                    onclick: move |_| {
                        let _ = down_sender.send(InteractiveMessage::MoveDown);
                        render_trigger.set(render_trigger() + 1);
                    },
                    "↓"
                }
                button {
                    class: "px-3 py-2 text-sm font-medium rounded border border-border bg-transparent text-foreground cursor-pointer",
                    onclick: move |_| {
                        let _ = left_sender.send(InteractiveMessage::MoveLeft);
                        render_trigger.set(render_trigger() + 1);
                    },
                    "←"
                }
                button {
                    class: "px-3 py-2 text-sm font-medium rounded border border-border bg-transparent text-foreground cursor-pointer",
                    onclick: move |_| {
                        let _ = right_sender.send(InteractiveMessage::MoveRight);
                        render_trigger.set(render_trigger() + 1);
                    },
                    "→"
                }
                button {
                    class: "px-3 py-2 text-sm font-medium rounded bg-primary text-primary-foreground cursor-pointer",
                    onclick: move |_| {
                        let _ = spawn_sender.send(InteractiveMessage::SpawnAtPlayer);
                        render_trigger.set(render_trigger() + 1);
                    },
                    "Spawn"
                }
            }

            // Debug info - render_trigger is read here to make dioxus track it
            div { class: "flex gap-4 text-sm",
                span { class: "font-mono",
                    "Focus: "
                    span { class: if is_focused() { "text-green-500 font-bold" } else { "text-red-500" },
                        if is_focused() { "YES" } else { "NO" }
                    }
                }
                span { class: "font-mono text-muted-foreground",
                    "Frame: {render_trigger}"
                }
            }

            p { class: "text-sm text-muted-foreground",
                if is_focused() {
                    "Canvas focused! Use WASD or Arrow keys to move. Hold Space to emit particles. Click to spawn particles."
                } else {
                    "Click on canvas or 'Focus Canvas' button, then use keyboard. Check terminal for logs."
                }
            }
        }
    )
}

/// Convert Key enum to a normalized lowercase string for easier matching
fn key_to_string(key: Key) -> String {
    match key {
        Key::Character(c) => {
            // Handle space explicitly (space has no lowercase equivalent issue)
            if c == " " {
                " ".to_string()
            } else {
                c.to_lowercase()
            }
        }
        Key::ArrowUp => "arrowup".to_string(),
        Key::ArrowDown => "arrowdown".to_string(),
        Key::ArrowLeft => "arrowleft".to_string(),
        Key::ArrowRight => "arrowright".to_string(),
        Key::Enter => "enter".to_string(),
        Key::Escape => "escape".to_string(),
        Key::Tab => "tab".to_string(),
        Key::Backspace => "backspace".to_string(),
        Key::Delete => "delete".to_string(),
        Key::Shift => "shift".to_string(),
        Key::Control => "control".to_string(),
        Key::Alt => "alt".to_string(),
        Key::Meta => "meta".to_string(),
        _ => String::new(), // Ignore other keys
    }
}

#[component]
fn BunnymarkCanvas() -> Element {
    // Create the bunnymark paint source and register it
    let paint_source = BunnymarkPaintSource::new();
    let sender = paint_source.sender();
    let paint_source_id = use_wgpu(move || paint_source);

    rsx!(
        div { class: "space-y-4",
            div {
                class: "flex gap-2 mb-4",
                button {
                    class: "btn btn-primary",
                    onclick: move |_| {
                        let _ = sender.send(BunnyMessage::SpawnBunnies);
                    },
                    "Spawn More Bunnies (+64)"
                }
            }
            div {
                class: "rounded-lg overflow-hidden border border-border",
                style: "width: 800px; height: 400px; display: grid;",
                canvas {
                    id: "bunnymark-canvas",
                    "src": "{paint_source_id}"
                }
            }
            p { class: "text-sm text-muted-foreground",
                "Each bunny is rendered using WGPU with physics simulation (gravity and bouncing)."
            }
        }
    )
}

#[component]
fn feedback_content() -> Element {
    let toast = use_toast();

    rsx! {
        div { class: "space-y-8",
            // Toast Demo
            div { class: "card",
                h2 { class: "text-2xl font-semibold mb-4", "Toast Notifications" }
                p { class: "text-muted-foreground mb-4",
                    "Click the buttons below to trigger toast notifications."
                }
                div { class: "flex flex-wrap gap-3",
                    Button {
                        variant: ButtonVariant::Primary,
                        on_click: move |_| {
                            toast.success(
                                "Success!".to_string(),
                                Some(ToastOptions {
                                    description: Some("Your changes have been saved.".to_string()),
                                    ..Default::default()
                                }),
                            );
                        },
                        "Success Toast"
                    }
                    Button {
                        variant: ButtonVariant::Destructive,
                        on_click: move |_| {
                            toast.error(
                                "Error!".to_string(),
                                Some(ToastOptions {
                                    description: Some("Something went wrong.".to_string()),
                                    ..Default::default()
                                }),
                            );
                        },
                        "Error Toast"
                    }
                    Button {
                        variant: ButtonVariant::Secondary,
                        on_click: move |_| {
                            toast.warning(
                                "Warning".to_string(),
                                Some(ToastOptions {
                                    description: Some("Please review before continuing.".to_string()),
                                    ..Default::default()
                                }),
                            );
                        },
                        "Warning Toast"
                    }
                    Button {
                        variant: ButtonVariant::Outline,
                        on_click: move |_| {
                            toast.info(
                                "Information".to_string(),
                                Some(ToastOptions {
                                    description: Some("Here's some helpful info.".to_string()),
                                    ..Default::default()
                                }),
                            );
                        },
                        "Info Toast"
                    }
                }
            }

            // Cards Showcase
            div { class: "card",
                h2 { class: "text-2xl font-semibold mb-4", "Card Examples" }
                div { class: "grid grid-cols-2 gap-4",
                    div { class: "p-4 rounded-lg border border-border bg-card",
                        h3 { class: "font-semibold mb-2", "Simple Card" }
                        p { class: "text-sm text-muted-foreground", "A basic card with content." }
                    }
                    div { class: "p-4 rounded-lg bg-primary text-primary-foreground",
                        h3 { class: "font-semibold mb-2", "Primary Card" }
                        p { class: "text-sm opacity-80", "Card with primary colors." }
                    }
                    div { class: "p-4 rounded-lg bg-secondary text-secondary-foreground",
                        h3 { class: "font-semibold mb-2", "Secondary Card" }
                        p { class: "text-sm opacity-80", "Card with secondary colors." }
                    }
                    div { class: "p-4 rounded-lg bg-destructive text-destructive-foreground",
                        h3 { class: "font-semibold mb-2", "Destructive Card" }
                        p { class: "text-sm opacity-80", "Card for warnings/errors." }
                    }
                }
            }
        }
    }
}

const TAILWIND_CSS: &str = r#"
/* ============================================
   Shadcn/Tailwind CSS for Dioxus Native
   Compatible with Lumen Blocks components
   ============================================ */

/* CSS Variables - Dark Theme (default) */
:root {
    --background: #09090b;
    --foreground: #fafafa;
    --card: #18181b;
    --card-foreground: #fafafa;
    --popover: #18181b;
    --popover-foreground: #fafafa;
    --primary: #fafafa;
    --primary-foreground: #18181b;
    --secondary: #27272a;
    --secondary-foreground: #fafafa;
    --muted: #27272a;
    --muted-foreground: #a1a1aa;
    --accent: #27272a;
    --accent-foreground: #fafafa;
    --destructive: #ef4444;
    --destructive-foreground: #fafafa;
    --border: #27272a;
    --input: #27272a;
    --ring: #52525b;
    --radius: 0.5rem;
}

/* Base Reset */
*, *::before, *::after {
    box-sizing: border-box;
    margin: 0;
    padding: 0;
}

html, body {
    font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, sans-serif;
    font-size: 16px;
    line-height: 1.5;
    background-color: var(--background);
    color: var(--foreground);
}

/* ============================================
   Tailwind Utility Classes
   ============================================ */

/* Layout */
.min-h-screen { min-height: 100vh; }
.max-w-4xl { max-width: 56rem; }
.mx-auto { margin-left: auto; margin-right: auto; }
.w-full { width: 100%; }
.w-auto { width: auto; }
.w-4 { width: 1rem; }
.h-4 { height: 1rem; }
.h-2 { height: 0.5rem; }
.h-3 { height: 0.75rem; }
.h-10 { height: 2.5rem; }
.w-10 { width: 2.5rem; }
.h-12 { height: 3rem; }
.w-12 { width: 3rem; }
.min-w-16 { min-width: 4rem; }
.aspect-square { aspect-ratio: 1/1; }
.shrink-0 { flex-shrink: 0; }
.overflow-hidden { overflow: hidden; }
.relative { position: relative; }
.absolute { position: absolute; }
.fixed { position: fixed; }
.inset-0 { top: 0; right: 0; bottom: 0; left: 0; }
.inset-y-0 { top: 0; bottom: 0; }
.left-0 { left: 0; }
.right-0 { right: 0; }
.top-4 { top: 1rem; }
.right-2 { right: 0.5rem; }
.top-2 { top: 0.5rem; }
.z-50 { z-index: 50; }
.z-1000 { z-index: 1000; }

/* Spacing */
.p-0 { padding: 0; }
.p-1\.5 { padding: 0.375rem; }
.p-2 { padding: 0.5rem; }
.p-3 { padding: 0.75rem; }
.p-4 { padding: 1rem; }
.p-6 { padding: 1.5rem; }
.p-8 { padding: 2rem; }
.px-2 { padding-left: 0.5rem; padding-right: 0.5rem; }
.px-2\.5 { padding-left: 0.625rem; padding-right: 0.625rem; }
.px-3 { padding-left: 0.75rem; padding-right: 0.75rem; }
.px-4 { padding-left: 1rem; padding-right: 1rem; }
.px-5 { padding-left: 1.25rem; padding-right: 1.25rem; }
.px-6 { padding-left: 1.5rem; padding-right: 1.5rem; }
.py-1 { padding-top: 0.25rem; padding-bottom: 0.25rem; }
.py-1\.5 { padding-top: 0.375rem; padding-bottom: 0.375rem; }
.py-2 { padding-top: 0.5rem; padding-bottom: 0.5rem; }
.py-3 { padding-top: 0.75rem; padding-bottom: 0.75rem; }
.py-4 { padding-top: 1rem; padding-bottom: 1rem; }
.pl-2 { padding-left: 0.5rem; }
.pr-2 { padding-right: 0.5rem; }
.pb-4 { padding-bottom: 1rem; }
.pt-0 { padding-top: 0; }
.mb-1 { margin-bottom: 0.25rem; }
.mb-2 { margin-bottom: 0.5rem; }
.mb-4 { margin-bottom: 1rem; }
.mb-6 { margin-bottom: 1.5rem; }
.mb-8 { margin-bottom: 2rem; }
.mt-1 { margin-top: 0.25rem; }
.mt-2 { margin-top: 0.5rem; }
.mt-4 { margin-top: 1rem; }
.mr-1 { margin-right: 0.25rem; }
.mr-2 { margin-right: 0.5rem; }
.ml-2 { margin-left: 0.5rem; }

/* Flexbox */
.flex { display: flex; }
.inline-flex { display: inline-flex; }
.flex-col { flex-direction: column; }
.flex-wrap { flex-wrap: wrap; }
.flex-1 { flex: 1 1 0%; }
.items-center { align-items: center; }
.items-end { align-items: flex-end; }
.justify-center { justify-content: center; }
.justify-between { justify-content: space-between; }
.gap-2 { gap: 0.5rem; }
.gap-3 { gap: 0.75rem; }
.gap-4 { gap: 1rem; }
.gap-6 { gap: 1.5rem; }

/* Grid */
.grid { display: grid; }
.grid-cols-2 { grid-template-columns: repeat(2, minmax(0, 1fr)); }

/* Spacing utilities */
.space-x-2 > * + * { margin-left: 0.5rem; }
.space-x-3 > * + * { margin-left: 0.75rem; }
.space-x-4 > * + * { margin-left: 1rem; }
.space-y-1 > * + * { margin-top: 0.25rem; }
.space-y-2 > * + * { margin-top: 0.5rem; }
.space-y-3 > * + * { margin-top: 0.75rem; }
.space-y-4 > * + * { margin-top: 1rem; }
.space-y-8 > * + * { margin-top: 2rem; }

/* Typography */
.text-xs { font-size: 0.75rem; line-height: 1rem; }
.text-sm { font-size: 0.875rem; line-height: 1.25rem; }
.text-base { font-size: 1rem; line-height: 1.5rem; }
.text-lg { font-size: 1.125rem; line-height: 1.75rem; }
.text-xl { font-size: 1.25rem; line-height: 1.75rem; }
.text-2xl { font-size: 1.5rem; line-height: 2rem; }
.text-4xl { font-size: 2.25rem; line-height: 2.5rem; }
.font-medium { font-weight: 500; }
.font-semibold { font-weight: 600; }
.font-bold { font-weight: 700; }
.font-mono { font-family: ui-monospace, SFMono-Regular, Menlo, Monaco, Consolas, monospace; }
.text-center { text-align: center; }
.text-left { text-align: left; }
.underline { text-decoration: underline; }
.underline-offset-4 { text-underline-offset: 4px; }
.leading-none { line-height: 1; }
.tracking-tight { letter-spacing: -0.025em; }

/* Colors */
.bg-background { background-color: var(--background); }
.bg-card { background-color: var(--card); }
.bg-primary { background-color: var(--primary); }
.bg-secondary { background-color: var(--secondary); }
.bg-muted { background-color: var(--muted); }
.bg-accent { background-color: var(--accent); }
.bg-destructive { background-color: var(--destructive); }
.bg-transparent { background-color: transparent; }
.bg-popover { background-color: var(--popover); }
.bg-green-500 { background-color: #22c55e; }
.bg-yellow-500 { background-color: #eab308; }
.bg-black\/40 { background-color: rgba(0, 0, 0, 0.4); }
.text-foreground { color: var(--foreground); }
.text-primary { color: var(--primary); }
.text-primary-foreground { color: var(--primary-foreground); }
.text-secondary-foreground { color: var(--secondary-foreground); }
.text-muted-foreground { color: var(--muted-foreground); }
.text-accent-foreground { color: var(--accent-foreground); }
.text-destructive-foreground { color: var(--destructive-foreground); }
.text-green-500 { color: #22c55e; }
.text-green-600 { color: #16a34a; }
.text-green-400 { color: #4ade80; }
.text-red-500 { color: #ef4444; }
.text-red-600 { color: #dc2626; }
.text-red-400 { color: #f87171; }
.text-yellow-500 { color: #eab308; }
.text-yellow-600 { color: #ca8a04; }
.text-yellow-400 { color: #facc15; }
.text-gray-500 { color: #6b7280; }

/* Borders */
.border { border-width: 1px; border-style: solid; border-color: var(--border); }
.border-b { border-bottom-width: 1px; border-bottom-style: solid; border-color: var(--border); }
.border-l { border-left-width: 1px; border-left-style: solid; border-color: var(--border); }
.border-r { border-right-width: 1px; border-right-style: solid; border-color: var(--border); }
.border-1 { border-width: 1px; }
.border-2 { border-width: 2px; }
.border-transparent { border-color: transparent; }
.border-border { border-color: var(--border); }
.border-input { border-color: var(--input); }
.border-destructive { border-color: var(--destructive); }
.rounded { border-radius: var(--radius); }
.rounded-md { border-radius: calc(var(--radius) - 2px); }
.rounded-lg { border-radius: var(--radius); }
.rounded-xl { border-radius: calc(var(--radius) + 4px); }
.rounded-full { border-radius: 9999px; }
.rounded-sm { border-radius: calc(var(--radius) - 4px); }
.last\:border-b-0:last-child { border-bottom-width: 0; }

/* Opacity */
.opacity-0 { opacity: 0; }
.opacity-50 { opacity: 0.5; }
.opacity-80 { opacity: 0.8; }
.opacity-90 { opacity: 0.9; }

/* Cursor */
.cursor-pointer { cursor: pointer; }
.cursor-not-allowed { cursor: not-allowed; }
.pointer-events-none { pointer-events: none; }
.pointer-events-auto { pointer-events: auto; }

/* Transitions */
.transition-all {
    transition-property: all;
    transition-duration: 150ms;
    transition-timing-function: cubic-bezier(0.4, 0, 0.2, 1);
}
.transition-colors {
    transition-property: color, background-color, border-color;
    transition-duration: 150ms;
}
.transition-transform {
    transition-property: transform;
    transition-duration: 200ms;
}
.transition-\[grid-template-rows\] {
    transition-property: grid-template-rows;
}
.duration-200 { transition-duration: 200ms; }
.duration-300 { transition-duration: 300ms; }
.ease-in-out { transition-timing-function: cubic-bezier(0.4, 0, 0.2, 1); }
.ease-out { transition-timing-function: cubic-bezier(0, 0, 0.2, 1); }
.transition-ease-out { transition-timing-function: cubic-bezier(0, 0, 0.2, 1); }

/* Transform */
.transform { transform: translateX(var(--tw-translate-x, 0)) translateY(var(--tw-translate-y, 0)) rotate(var(--tw-rotate, 0)); }
.rotate-180 { transform: rotate(180deg); }
.translate-x-0 { transform: translateX(0); }
.translate-x-full { transform: translateX(100%); }
.-translate-x-full { transform: translateX(-100%); }

/* Focus */
.focus\:outline-none:focus { outline: none; }
.focus\:ring-2:focus { box-shadow: 0 0 0 2px var(--ring); }
.focus\:ring-offset-2:focus { box-shadow: 0 0 0 2px var(--background), 0 0 0 4px var(--ring); }
.focus\:ring-ring:focus { --tw-ring-color: var(--ring); }
.focus\:border-ring:focus { border-color: var(--ring); }

/* Hover states */
.hover\:underline:hover { text-decoration: underline; }
.hover\:bg-primary\/90:hover { background-color: rgba(250, 250, 250, 0.9); }
.hover\:bg-secondary\/80:hover { background-color: rgba(39, 39, 42, 0.8); }
.hover\:bg-muted:hover { background-color: var(--muted); }
.hover\:bg-accent\/10:hover { background-color: rgba(39, 39, 42, 0.1); }
.hover\:bg-destructive\/90:hover { background-color: rgba(239, 68, 68, 0.9); }
.hover\:shadow-lg:hover { box-shadow: 0 10px 15px -3px rgba(0, 0, 0, 0.1), 0 4px 6px -2px rgba(0, 0, 0, 0.05); }
.group:hover .group-hover\:opacity-100 { opacity: 1; }

/* Group states for accordion chevron rotation */
.group[aria-expanded="true"] .group-aria-expanded\:rotate-180 {
    transform: rotate(180deg);
}
[aria-expanded="true"] > .group-aria-expanded\:rotate-180 {
    transform: rotate(180deg);
}

/* Grid for accordion animation */
.grid-rows-\[0fr\] { grid-template-rows: 0fr; }
.grid-rows-\[1fr\] { grid-template-rows: 1fr; }
[data-open="true"] .group-data-\[open\=true\]\:grid-rows-\[1fr\] {
    grid-template-rows: 1fr;
}

/* Shadow */
.shadow-none { box-shadow: none; }
.shadow { box-shadow: 0 1px 3px 0 rgba(0, 0, 0, 0.1), 0 1px 2px 0 rgba(0, 0, 0, 0.06); }
.shadow-md { box-shadow: 0 4px 6px -1px rgba(0, 0, 0, 0.1), 0 2px 4px -1px rgba(0, 0, 0, 0.06); }
.ring-0 { box-shadow: none; }

/* Size utilities */
.size-4 { width: 1rem; height: 1rem; }
.size-5 { width: 1.25rem; height: 1.25rem; }
.h-5 { height: 1.25rem; }
.w-5 { width: 1.25rem; }
.h-6 { height: 1.5rem; }
.w-6 { width: 1.5rem; }
.h-8 { height: 2rem; }

/* Animation */
.animate-spin {
    animation: spin 1s linear infinite;
}
@keyframes spin {
    from { transform: rotate(0deg); }
    to { transform: rotate(360deg); }
}

.animate-slide-in-from-right {
    animation: slide-in-from-right 0.2s ease-out;
}
@keyframes slide-in-from-right {
    from { transform: translateX(100%); }
    to { transform: translateX(0); }
}

.animate-slide-out-to-right {
    animation: slide-out-to-right 0.2s ease-out forwards;
}
@keyframes slide-out-to-right {
    from { transform: translateX(0); }
    to { transform: translateX(100%); }
}

.inline-block { display: inline-block; }
.block { display: block; }
.hidden { display: none; }

/* Object fit */
.object-cover { object-fit: cover; }

/* ============================================
   Custom Component Styles
   ============================================ */

/* Card */
.card {
    background-color: var(--card);
    border: 1px solid var(--border);
    border-radius: var(--radius);
    padding: 1.5rem;
}

/* Tab */
.tab {
    padding: 0.5rem 1rem;
    font-size: 0.875rem;
    font-weight: 500;
    color: var(--muted-foreground);
    background: transparent;
    border: none;
    border-bottom: 2px solid transparent;
    cursor: pointer;
    transition: color 150ms, border-color 150ms;
    font-family: inherit;
}
.tab:hover {
    color: var(--foreground);
}
.tab-active {
    color: var(--foreground);
    border-bottom-color: var(--primary);
}

/* Form Group */
.form-group {
    display: flex;
    flex-direction: column;
}

/* Label */
.label {
    font-size: 0.875rem;
    font-weight: 500;
    margin-bottom: 0.5rem;
    color: var(--foreground);
}

/* Progress bar styling */
[role="progressbar"] {
    height: 0.75rem;
    background-color: var(--secondary);
    border-radius: 9999px;
    overflow: hidden;
    position: relative;
}

[role="progressbar"] > div {
    height: 100%;
    border-radius: 9999px;
    transition: width 300ms ease-in-out;
}

/* Switch base styling */
button[role="switch"] {
    background-color: var(--input);
    border: 2px solid transparent;
    border-radius: 9999px;
    cursor: pointer;
    position: relative;
    transition: background-color 300ms ease-in-out;
}

button[role="switch"][aria-checked="true"] {
    background-color: var(--primary);
}

button[role="switch"]:disabled {
    opacity: 0.5;
    cursor: not-allowed;
}

/* Switch thumb */
button[role="switch"] > span {
    display: inline-block;
    background-color: var(--background);
    border-radius: 9999px;
    box-shadow: 0 1px 3px rgba(0, 0, 0, 0.1);
    transition: transform 300ms ease-in-out;
    will-change: transform;
}

/* Checkbox styling */
[role="checkbox"] {
    display: inline-flex;
    align-items: center;
    justify-content: center;
    border-radius: 4px;
    border: 2px solid var(--input);
    background-color: var(--background);
    cursor: pointer;
    transition: all 150ms;
}

[role="checkbox"]:hover:not([aria-disabled="true"]) {
    background-color: rgba(39, 39, 42, 0.1);
}

[role="checkbox"][aria-checked="true"] {
    background-color: var(--primary);
    border-color: var(--primary);
}

[role="checkbox"][aria-disabled="true"] {
    opacity: 0.5;
    cursor: not-allowed;
}

/* Input field styling */
input[type="text"],
input[type="email"],
input[type="password"],
input[type="search"],
input[type="url"],
input[type="tel"],
input[type="number"] {
    width: 100%;
    padding: 0.5rem 0.75rem;
    font-size: 0.875rem;
    line-height: 1.5;
    color: var(--foreground);
    background-color: var(--background);
    border: 1px solid var(--input);
    border-radius: var(--radius);
    font-family: inherit;
    transition: border-color 150ms, box-shadow 150ms;
}

input:focus {
    outline: none;
    border-color: var(--ring);
    box-shadow: 0 0 0 2px var(--ring);
}

input::placeholder {
    color: var(--muted-foreground);
}

input:disabled {
    opacity: 0.5;
    cursor: not-allowed;
    background-color: var(--muted);
}

/* SR only for hidden inputs */
.sr-only {
    position: absolute;
    width: 1px;
    height: 1px;
    padding: 0;
    margin: -1px;
    overflow: hidden;
    clip: rect(0, 0, 0, 0);
    white-space: nowrap;
    border-width: 0;
}

/* Backdrop blur */
.backdrop-blur-sm { backdrop-filter: blur(4px); }
.backdrop-blur-xs { backdrop-filter: blur(2px); }

/* Max width */
.max-w-sm { max-width: 24rem; }

/* Aria states for switch */
[aria-checked="true"].bg-input { background-color: var(--primary); }
.aria-checked\:bg-primary[aria-checked="true"] { background-color: var(--primary); }

/* Debug colors */
.text-green-500 { color: #22c55e; }
.text-red-500 { color: #ef4444; }
"#;
