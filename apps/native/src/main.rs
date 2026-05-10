use dioxus::prelude::*;
use fts_story_shell::Lookbook;
use fts_ui::prelude::*;
use fts_ui::showcase::Showcase;

const TAILWIND_CSS: Asset = asset!("/assets/tailwind.css");

fn main() {
    // Touch the stories module so the linkme registrations survive LTO
    // dead-code elimination.
    fts_ui::stories::force_link();

    let args: Vec<String> = std::env::args().collect();
    let showcase = args.iter().any(|a| a == "--showcase");

    if showcase {
        dioxus::launch(ShowcaseApp);
    } else {
        dioxus::launch(LookbookApp);
    }
}

#[component]
fn ShowcaseApp() -> Element {
    rsx! {
        document::Stylesheet { href: TAILWIND_CSS }
        Showcase { renderer: "Native".to_string() }
    }
}

#[component]
fn LookbookApp() -> Element {
    // One global `ThemeProvider` wraps the whole Lookbook so every
    // story preview inherits the same active preset/mode. The
    // `ThemeSwitcher` is slotted into the shell's top-bar
    // `header_extras` so changing it re-themes every preview at
    // once.
    let theme_state = use_signal(|| ThemeState::new(default_theme_preset(), ThemeMode::Dark));

    rsx! {
        document::Stylesheet { href: TAILWIND_CSS }
        ThemeProvider { state: theme_state,
            Lookbook {
                header_extras: rsx! {
                    LookbookThemeControls { state: theme_state }
                },
            }
        }
    }
}

/// Compact theme controls for the Lookbook top bar — preset select +
/// light/dark toggle. Mirrors the bigger `ThemeSwitcher` panel but
/// trims the radius/spacing/typography knobs to keep the row light.
#[component]
fn LookbookThemeControls(state: Signal<ThemeState>) -> Element {
    let current = state();
    let preset_name = current.preset.clone();
    let mode = current.mode;
    let mut preset_value = use_signal(|| preset_name.clone());

    use_effect(move || {
        preset_value.set(preset_name.clone());
    });

    let mut state_signal = state;

    rsx! {
        span { class: "text-xs text-muted-foreground", "Preset" }
        div { class: "min-w-[12rem]",
            Select {
                value: preset_value,
                placeholder: "Theme…".to_string(),
                on_change: move |value: String| {
                    if let Some(p) = theme_preset(&value) {
                        state_signal.write().set_preset(p);
                    }
                },
                SelectContent {
                    for (i, p) in theme_presets().into_iter().enumerate() {
                        SelectItem { value: p.name, index: i, "{p.label}" }
                    }
                }
            }
        }
        ButtonGroup {
            Button {
                size: ButtonSize::Small,
                variant: if mode == ThemeMode::Light { ButtonVariant::Primary } else { ButtonVariant::Outline },
                on_click: move |_| state_signal.write().set_mode(ThemeMode::Light),
                "Light"
            }
            Button {
                size: ButtonSize::Small,
                variant: if mode == ThemeMode::Dark { ButtonVariant::Primary } else { ButtonVariant::Outline },
                on_click: move |_| state_signal.write().set_mode(ThemeMode::Dark),
                "Dark"
            }
        }
    }
}
