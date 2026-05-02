use dioxus::desktop::{tao::window::WindowBuilder, Config};
use dioxus::prelude::*;
use fts_story_shell::Lookbook;
use fts_ui::prelude::*;
use fts_ui::showcase::Showcase;

const TAILWIND_CSS: Asset = asset!("/assets/tailwind.css");

fn main() {
    fts_ui::stories::force_link();

    unsafe {
        std::env::set_var("GTK_THEME", "Adwaita:dark");
    }
    unsafe {
        std::env::set_var("WEBKIT_DISABLE_COMPOSITING_MODE", "1");
    }

    let args: Vec<String> = std::env::args().collect();
    let showcase = args.iter().any(|a| a == "--showcase");
    let snapshot_story: Option<String> = args
        .iter()
        .find_map(|a| a.strip_prefix("--snapshot=").map(|s| s.to_string()));

    if let Some(story) = snapshot_story {
        // Headless parity-snapshot mode: fixed window size, no chrome,
        // distinctive title for window-id lookup. The orchestrator
        // (`fts-story-parity`) waits for the window to appear in its
        // Xvfb display, screenshots it, then kills the process.
        let title = format!("fts-ui-snapshot:{story}");
        let cfg = Config::new()
            .with_window(
                WindowBuilder::new()
                    .with_title(title)
                    .with_decorations(false)
                    .with_inner_size(dioxus::desktop::tao::dpi::LogicalSize::new(1280.0, 800.0)),
            )
            .with_menu(None);
        SnapshotApp::set_story(story);
        LaunchBuilder::desktop().with_cfg(cfg).launch(SnapshotApp::run);
        return;
    }

    let cfg = Config::new()
        .with_window(
            WindowBuilder::new()
                .with_title(if showcase {
                    "fts-ui Showcase (wry)"
                } else {
                    "fts-ui Lookbook (wry)"
                })
                .with_inner_size(dioxus::desktop::tao::dpi::LogicalSize::new(1400.0, 900.0)),
        )
        .with_menu(None);

    if showcase {
        LaunchBuilder::desktop().with_cfg(cfg).launch(ShowcaseApp);
    } else {
        LaunchBuilder::desktop().with_cfg(cfg).launch(LookbookApp);
    }
}

mod snapshot {
    use super::*;
    use std::sync::OnceLock;

    static SNAPSHOT_STORY: OnceLock<String> = OnceLock::new();

    pub struct SnapshotApp;

    impl SnapshotApp {
        pub fn set_story(name: String) {
            let _ = SNAPSHOT_STORY.set(name);
        }

        pub fn run() -> Element {
            let story = SNAPSHOT_STORY.get().cloned().unwrap_or_default();
            let theme_state =
                use_signal(|| ThemeState::new(default_theme_preset(), ThemeMode::Dark));
            rsx! {
                document::Stylesheet { href: TAILWIND_CSS }
                ThemeProvider { state: theme_state,
                    Lookbook {
                        chrome: false,
                        initial_story: story,
                    }
                }
            }
        }
    }
}

use snapshot::SnapshotApp;

#[component]
fn ShowcaseApp() -> Element {
    rsx! {
        document::Stylesheet { href: TAILWIND_CSS }
        Showcase { renderer: "Desktop".to_string() }
    }
}

#[component]
fn LookbookApp() -> Element {
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
