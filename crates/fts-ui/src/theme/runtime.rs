use dioxus::prelude::*;

use crate::components::{Button, ButtonVariant, Label};

const SHADOW_KEYS: [&str; 6] = [
    "shadow-color",
    "shadow-opacity",
    "shadow-blur",
    "shadow-spread",
    "shadow-offset-x",
    "shadow-offset-y",
];

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ThemeMode {
    Light,
    Dark,
}

impl ThemeMode {
    pub fn as_str(self) -> &'static str {
        match self {
            Self::Light => "light",
            Self::Dark => "dark",
        }
    }

    pub fn is_dark(self) -> bool {
        matches!(self, Self::Dark)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct ThemeToken {
    pub key: String,
    pub value: String,
}

impl ThemeToken {
    pub fn new(key: impl Into<String>, value: impl Into<String>) -> Self {
        Self {
            key: key.into(),
            value: value.into(),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct ThemeStyle {
    pub tokens: Vec<ThemeToken>,
}

impl ThemeStyle {
    pub fn new(tokens: impl IntoIterator<Item = ThemeToken>) -> Self {
        Self {
            tokens: tokens.into_iter().collect(),
        }
    }

    pub fn get(&self, key: &str) -> Option<&str> {
        self.tokens
            .iter()
            .find(|token| token.key == key)
            .map(|token| token.value.as_str())
    }

    pub fn set(&mut self, key: impl Into<String>, value: impl Into<String>) {
        let key = key.into();
        let value = value.into();

        if let Some(token) = self.tokens.iter_mut().find(|token| token.key == key) {
            token.value = value;
        } else {
            self.tokens.push(ThemeToken::new(key, value));
        }
    }

    pub fn with(mut self, key: impl Into<String>, value: impl Into<String>) -> Self {
        self.set(key, value);
        self
    }

    pub fn css_variables(&self) -> String {
        let mut css = String::new();

        for token in &self.tokens {
            if !token.value.trim().is_empty() {
                css.push_str("--");
                css.push_str(&token.key);
                css.push_str(": ");
                css.push_str(&token.value);
                css.push(';');
            }
        }

        append_shadow_variables(&mut css, self);
        css
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct ThemeStyles {
    pub light: ThemeStyle,
    pub dark: ThemeStyle,
}

impl ThemeStyles {
    pub fn active(&self, mode: ThemeMode) -> &ThemeStyle {
        match mode {
            ThemeMode::Light => &self.light,
            ThemeMode::Dark => &self.dark,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct ThemePreset {
    pub name: String,
    pub label: String,
    pub styles: ThemeStyles,
}

#[derive(Clone, Debug, PartialEq)]
pub struct ThemeState {
    pub mode: ThemeMode,
    pub preset: String,
    pub styles: ThemeStyles,
}

impl ThemeState {
    pub fn new(preset: ThemePreset, mode: ThemeMode) -> Self {
        Self {
            mode,
            preset: preset.name,
            styles: preset.styles,
        }
    }

    pub fn active_style(&self) -> &ThemeStyle {
        self.styles.active(self.mode)
    }

    pub fn set_mode(&mut self, mode: ThemeMode) {
        self.mode = mode;
    }

    pub fn set_preset(&mut self, preset: ThemePreset) {
        self.preset = preset.name;
        self.styles = preset.styles;
    }

    pub fn set_token(&mut self, mode: ThemeMode, key: impl Into<String>, value: impl Into<String>) {
        match mode {
            ThemeMode::Light => self.styles.light.set(key, value),
            ThemeMode::Dark => self.styles.dark.set(key, value),
        }
    }
}

#[derive(Clone, Copy)]
pub struct ThemeContext {
    pub state: Signal<ThemeState>,
}

#[derive(Props, Clone, PartialEq)]
pub struct ThemeProviderProps {
    pub state: Signal<ThemeState>,
    #[props(default)]
    pub class: String,
    pub children: Element,
}

#[component]
pub fn ThemeProvider(props: ThemeProviderProps) -> Element {
    use_context_provider(|| ThemeContext { state: props.state });

    let state = (props.state)();
    let style = theme_style_attribute(&state.styles, state.mode);
    let mode = state.mode.as_str();

    rsx! {
        ThemeRuntimeStyle {}
        div {
            class: crate::cn::merge_slice(&["fts-theme-root min-h-full", props.class.as_str()]),
            "data-theme-mode": mode,
            style,
            {props.children}
        }
    }
}

#[derive(Props, Clone, PartialEq)]
pub struct ThemeScopeProps {
    pub styles: ThemeStyles,
    #[props(default)]
    pub mode: Option<ThemeMode>,
    #[props(default)]
    pub class: String,
    pub children: Element,
}

#[component]
pub fn ThemeScope(props: ThemeScopeProps) -> Element {
    let inherited = try_consume_context::<ThemeContext>();
    let inherited_mode = inherited
        .as_ref()
        .map(|context| (context.state)().mode)
        .unwrap_or(ThemeMode::Light);
    let mode = props.mode.unwrap_or(inherited_mode);
    let style = theme_style_attribute(&props.styles, mode);

    rsx! {
        div {
            class: crate::cn::merge_slice(&["fts-theme-scope", props.class.as_str()]),
            "data-theme-mode": mode.as_str(),
            style,
            {props.children}
        }
    }
}

#[component]
pub fn ThemeRuntimeStyle() -> Element {
    rsx! {
        document::Style {
            r#"
                .fts-theme-root,
                .fts-theme-scope {{
                    font-family: var(--font-sans);
                    letter-spacing: var(--letter-spacing, 0);
                }}

                .fts-theme-root .font-serif,
                .fts-theme-scope .font-serif {{
                    font-family: var(--font-serif);
                }}

                .fts-theme-root .font-mono,
                .fts-theme-scope .font-mono {{
                    font-family: var(--font-mono);
                }}

                .fts-theme-root .shadow-xs,
                .fts-theme-scope .shadow-xs {{
                    box-shadow: var(--shadow-xs) !important;
                }}

                .fts-theme-root .shadow-sm,
                .fts-theme-scope .shadow-sm {{
                    box-shadow: var(--shadow-sm) !important;
                }}

                .fts-theme-root .shadow-md,
                .fts-theme-scope .shadow-md {{
                    box-shadow: var(--shadow-md) !important;
                }}

                .fts-theme-root .shadow-lg,
                .fts-theme-scope .shadow-lg {{
                    box-shadow: var(--shadow-lg) !important;
                }}
            "#
        }
    }
}

#[derive(Props, Clone, PartialEq)]
pub struct ThemeSwitcherProps {
    pub state: Signal<ThemeState>,
    #[props(default)]
    pub class: String,
}

#[component]
pub fn ThemeSwitcher(mut props: ThemeSwitcherProps) -> Element {
    let current = (props.state)();
    let mode = current.mode;
    let preset = current.preset.clone();
    let radius = current
        .active_style()
        .get("radius")
        .unwrap_or("0.625rem")
        .to_string();
    let spacing = current
        .active_style()
        .get("spacing")
        .unwrap_or("0.25rem")
        .to_string();
    let font_sans = current
        .active_style()
        .get("font-sans")
        .unwrap_or("ui-sans-serif, system-ui, sans-serif")
        .to_string();
    let input_class = "h-9 w-full rounded-lg border border-input bg-input/30 px-3 py-1 text-sm shadow-xs transition-colors focus-visible:border-ring focus-visible:outline-none focus-visible:ring-[3px] focus-visible:ring-ring/50";

    rsx! {
        div {
            class: crate::cn::merge_slice(
                &["grid gap-3 rounded-lg border border-border bg-card p-4 text-card-foreground shadow-sm", props.class.as_str()],
            ),
            div { class: "grid gap-1",
                Label { "Preset" }
                select {
                    class: input_class,
                    value: "{preset}",
                    onchange: move |event: FormEvent| {
                        if let Some(preset) = theme_preset(&event.value()) {
                            props.state.write().set_preset(preset);
                        }
                    },
                    option { value: "default", "Default" }
                    option { value: "zinc", "Zinc" }
                    option { value: "supabase", "Supabase" }
                    option { value: "amethyst", "Amethyst" }
                }
            }
            div { class: "grid grid-cols-2 gap-2",
                Button {
                    variant: if mode == ThemeMode::Light { ButtonVariant::Primary } else { ButtonVariant::Outline },
                    on_click: move |_| props.state.write().set_mode(ThemeMode::Light),
                    "Light"
                }
                Button {
                    variant: if mode == ThemeMode::Dark { ButtonVariant::Primary } else { ButtonVariant::Outline },
                    on_click: move |_| props.state.write().set_mode(ThemeMode::Dark),
                    "Dark"
                }
            }
            div { class: "grid gap-1",
                Label { "Radius" }
                input {
                    class: input_class,
                    value: "{radius}",
                    oninput: move |event: FormEvent| props.state.write().set_token(mode, "radius", event.value()),
                }
            }
            div { class: "grid gap-1",
                Label { "Spacing" }
                input {
                    class: input_class,
                    value: "{spacing}",
                    oninput: move |event: FormEvent| props.state.write().set_token(mode, "spacing", event.value()),
                }
            }
            div { class: "grid gap-1",
                Label { "Typography" }
                input {
                    class: input_class,
                    value: "{font_sans}",
                    oninput: move |event: FormEvent| props.state.write().set_token(mode, "font-sans", event.value()),
                }
            }
        }
    }
}

pub fn use_theme() -> ThemeContext {
    use_context::<ThemeContext>()
}

pub fn default_theme_state() -> ThemeState {
    ThemeState::new(default_theme_preset(), ThemeMode::Light)
}

pub fn default_theme_preset() -> ThemePreset {
    ThemePreset {
        name: "default".to_string(),
        label: "Default".to_string(),
        styles: ThemeStyles {
            light: default_light_style(),
            dark: default_dark_style(),
        },
    }
}

pub fn theme_presets() -> Vec<ThemePreset> {
    vec![
        default_theme_preset(),
        zinc_theme_preset(),
        supabase_theme_preset(),
        amethyst_theme_preset(),
    ]
}

pub fn theme_preset(name: &str) -> Option<ThemePreset> {
    theme_presets()
        .into_iter()
        .find(|preset| preset.name == name)
}

fn theme_style_attribute(styles: &ThemeStyles, mode: ThemeMode) -> String {
    let mut style = styles.active(mode).css_variables();
    let color_scheme = if mode.is_dark() { "dark" } else { "light" };

    style.push_str("color-scheme: ");
    style.push_str(color_scheme);
    style.push(';');
    style.push_str("color: var(--foreground); background-color: var(--background);");
    style
}

fn append_shadow_variables(css: &mut String, style: &ThemeStyle) {
    if !SHADOW_KEYS.iter().all(|key| style.get(key).is_some()) {
        return;
    }

    let color = style.get("shadow-color").unwrap_or("oklch(0 0 0)");
    let opacity = style.get("shadow-opacity").unwrap_or("0.1");
    let blur = style.get("shadow-blur").unwrap_or("3px");
    let spread = style.get("shadow-spread").unwrap_or("0px");
    let x = style.get("shadow-offset-x").unwrap_or("0px");
    let y = style.get("shadow-offset-y").unwrap_or("1px");

    let color_half = format!("color-mix(in oklab, {color} calc({opacity} * 50%), transparent)");
    let color_full = format!("color-mix(in oklab, {color} calc({opacity} * 100%), transparent)");

    css.push_str("--shadow-xs: ");
    css.push_str(&format!("{x} {y} {blur} {spread} {color_half};"));
    css.push_str("--shadow-sm: ");
    css.push_str(&format!(
        "{x} {y} {blur} {spread} {color_full}, {x} 1px 2px -1px {color_full};"
    ));
    css.push_str("--shadow-md: ");
    css.push_str(&format!(
        "{x} {y} {blur} {spread} {color_full}, {x} 2px 4px -1px {color_full};"
    ));
    css.push_str("--shadow-lg: ");
    css.push_str(&format!(
        "{x} {y} {blur} {spread} {color_full}, {x} 4px 6px -1px {color_full};"
    ));
}

fn default_light_style() -> ThemeStyle {
    ThemeStyle::new([
        ThemeToken::new("background", "oklch(1 0 0)"),
        ThemeToken::new("foreground", "oklch(0.145 0 0)"),
        ThemeToken::new("card", "oklch(1 0 0)"),
        ThemeToken::new("card-foreground", "oklch(0.145 0 0)"),
        ThemeToken::new("popover", "oklch(1 0 0)"),
        ThemeToken::new("popover-foreground", "oklch(0.145 0 0)"),
        ThemeToken::new("primary", "oklch(0.205 0 0)"),
        ThemeToken::new("primary-foreground", "oklch(0.985 0 0)"),
        ThemeToken::new("secondary", "oklch(0.97 0 0)"),
        ThemeToken::new("secondary-foreground", "oklch(0.205 0 0)"),
        ThemeToken::new("muted", "oklch(0.97 0 0)"),
        ThemeToken::new("muted-foreground", "oklch(0.556 0 0)"),
        ThemeToken::new("accent", "oklch(0.97 0 0)"),
        ThemeToken::new("accent-foreground", "oklch(0.205 0 0)"),
        ThemeToken::new("destructive", "oklch(0.5757 0.2352 27.92)"),
        ThemeToken::new("destructive-foreground", "oklch(0.985 0 0)"),
        ThemeToken::new("border", "oklch(0.922 0 0)"),
        ThemeToken::new("input", "oklch(0.922 0 0)"),
        ThemeToken::new("ring", "oklch(0.708 0 0)"),
        ThemeToken::new("chart-1", "oklch(0.646 0.222 41.116)"),
        ThemeToken::new("chart-2", "oklch(0.6 0.118 184.704)"),
        ThemeToken::new("chart-3", "oklch(0.398 0.07 227.392)"),
        ThemeToken::new("chart-4", "oklch(0.828 0.189 84.429)"),
        ThemeToken::new("chart-5", "oklch(0.769 0.188 70.08)"),
        ThemeToken::new("sidebar", "oklch(0.985 0 0)"),
        ThemeToken::new("sidebar-foreground", "oklch(0.145 0 0)"),
        ThemeToken::new("sidebar-primary", "oklch(0.205 0 0)"),
        ThemeToken::new("sidebar-primary-foreground", "oklch(0.985 0 0)"),
        ThemeToken::new("sidebar-accent", "oklch(0.97 0 0)"),
        ThemeToken::new("sidebar-accent-foreground", "oklch(0.205 0 0)"),
        ThemeToken::new("sidebar-border", "oklch(0.922 0 0)"),
        ThemeToken::new("sidebar-ring", "oklch(0.708 0 0)"),
        ThemeToken::new("font-sans", "ui-sans-serif, system-ui, sans-serif"),
        ThemeToken::new("font-serif", "ui-serif, Georgia, serif"),
        ThemeToken::new(
            "font-mono",
            "ui-monospace, SFMono-Regular, Menlo, monospace",
        ),
        ThemeToken::new("radius", "0.625rem"),
        ThemeToken::new("spacing", "0.25rem"),
        ThemeToken::new("letter-spacing", "0"),
        ThemeToken::new("shadow-color", "oklch(0 0 0)"),
        ThemeToken::new("shadow-opacity", "0.1"),
        ThemeToken::new("shadow-blur", "3px"),
        ThemeToken::new("shadow-spread", "0px"),
        ThemeToken::new("shadow-offset-x", "0px"),
        ThemeToken::new("shadow-offset-y", "1px"),
    ])
}

fn default_dark_style() -> ThemeStyle {
    default_light_style()
        .with("background", "oklch(0.145 0 0)")
        .with("foreground", "oklch(0.985 0 0)")
        .with("card", "oklch(0.145 0 0)")
        .with("card-foreground", "oklch(0.985 0 0)")
        .with("popover", "oklch(0.145 0 0)")
        .with("popover-foreground", "oklch(0.985 0 0)")
        .with("primary", "oklch(0.985 0 0)")
        .with("primary-foreground", "oklch(0.205 0 0)")
        .with("secondary", "oklch(0.269 0 0)")
        .with("secondary-foreground", "oklch(0.985 0 0)")
        .with("muted", "oklch(0.269 0 0)")
        .with("muted-foreground", "oklch(0.708 0 0)")
        .with("accent", "oklch(0.269 0 0)")
        .with("accent-foreground", "oklch(0.985 0 0)")
        .with("destructive", "oklch(0.5058 0.2066 27.85)")
        .with("border", "oklch(0.269 0 0)")
        .with("input", "oklch(0.269 0 0)")
        .with("ring", "oklch(0.439 0 0)")
        .with("sidebar", "oklch(0.205 0 0)")
        .with("sidebar-foreground", "oklch(0.985 0 0)")
        .with("sidebar-accent", "oklch(0.269 0 0)")
        .with("sidebar-accent-foreground", "oklch(0.985 0 0)")
        .with("sidebar-border", "oklch(0.269 0 0)")
        .with("sidebar-ring", "oklch(0.439 0 0)")
}

fn zinc_theme_preset() -> ThemePreset {
    let light = default_light_style().with("primary", "oklch(0.274 0.006 286.033)");
    let dark = default_dark_style()
        .with("background", "oklch(0.141 0.005 285.823)")
        .with("card", "oklch(0.21 0.006 285.885)");

    ThemePreset {
        name: "zinc".to_string(),
        label: "Zinc".to_string(),
        styles: ThemeStyles { light, dark },
    }
}

fn supabase_theme_preset() -> ThemePreset {
    let light = default_light_style()
        .with("primary", "oklch(0.64 0.18 149)")
        .with("ring", "oklch(0.64 0.18 149)")
        .with("sidebar-primary", "oklch(0.64 0.18 149)")
        .with("radius", "0.5rem");
    let dark = default_dark_style()
        .with("primary", "oklch(0.72 0.18 149)")
        .with("ring", "oklch(0.72 0.18 149)")
        .with("sidebar-primary", "oklch(0.72 0.18 149)")
        .with("radius", "0.5rem");

    ThemePreset {
        name: "supabase".to_string(),
        label: "Supabase".to_string(),
        styles: ThemeStyles { light, dark },
    }
}

fn amethyst_theme_preset() -> ThemePreset {
    let light = default_light_style()
        .with("primary", "oklch(0.55 0.22 292)")
        .with("ring", "oklch(0.62 0.2 292)")
        .with("accent", "oklch(0.95 0.03 292)")
        .with("font-sans", "Inter, ui-sans-serif, system-ui, sans-serif")
        .with("radius", "0.875rem");
    let dark = default_dark_style()
        .with("primary", "oklch(0.72 0.19 292)")
        .with("ring", "oklch(0.72 0.19 292)")
        .with("accent", "oklch(0.27 0.06 292)")
        .with("font-sans", "Inter, ui-sans-serif, system-ui, sans-serif")
        .with("radius", "0.875rem");

    ThemePreset {
        name: "amethyst".to_string(),
        label: "Amethyst".to_string(),
        styles: ThemeStyles { light, dark },
    }
}
