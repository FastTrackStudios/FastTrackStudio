//! Dropdown menu — shadcn v4 maia style, standalone implementation.

use dioxus::prelude::*;

// ── Context ──────────────────────────────────────────────────────────────────

#[derive(Clone, Copy)]
struct DropdownContext {
    open: Signal<bool>,
}

// ── Dropdown ─────────────────────────────────────────────────────────────────

#[derive(Props, Clone, PartialEq)]
pub struct DropdownProps {
    #[props(default = false)]
    pub default_open: bool,
    #[props(default)]
    pub disabled: bool,
    #[props(default)]
    pub class: String,
    pub children: Element,
}

#[component]
pub fn Dropdown(props: DropdownProps) -> Element {
    let open = use_signal(|| props.default_open);
    use_context_provider(|| DropdownContext { open });

    let disabled_class = if props.disabled { "opacity-50 pointer-events-none" } else { "" };

    rsx! {
        div {
            class: format!("relative inline-block text-left {disabled_class} {}", props.class),
            {props.children}
        }
    }
}

// ── Trigger ──────────────────────────────────────────────────────────────────

#[derive(Props, Clone, PartialEq)]
pub struct DropdownTriggerProps {
    #[props(default)]
    pub class: String,
    pub children: Element,
}

#[component]
pub fn DropdownTrigger(props: DropdownTriggerProps) -> Element {
    let mut ctx: DropdownContext = use_context();

    rsx! {
        div {
            class: format!("cursor-pointer {}", props.class),
            onclick: move |_| {
                let current = *ctx.open.read();
                ctx.open.set(!current);
            },
            {props.children}
        }
    }
}

// ── Content ──────────────────────────────────────────────────────────────────

/// Dropdown size (kept for API compat).
#[derive(Clone, Copy, PartialEq, Eq, Debug, Default)]
pub enum DropdownSize {
    Small,
    #[default]
    Medium,
    Large,
}

#[derive(Props, Clone, PartialEq)]
pub struct DropdownContentProps {
    #[props(default = String::from("start"))]
    pub align: String,
    #[props(default = String::from("w-56"))]
    pub width: String,
    #[props(default)]
    pub class: String,
    pub children: Element,
}

/// shadcn v4 maia: cn-dropdown-menu-content
#[component]
pub fn DropdownContent(props: DropdownContentProps) -> Element {
    let mut ctx: DropdownContext = use_context();
    let is_open = *ctx.open.read();

    if !is_open {
        return rsx! {};
    }

    let align_class = match props.align.as_str() {
        "end" => "right-0 origin-top-right",
        "center" => "left-1/2 -translate-x-1/2 origin-top",
        _ => "left-0 origin-top-left",
    };

    rsx! {
        // Invisible backdrop to close on outside click
        div {
            class: "fixed inset-0 z-[99]",
            onclick: move |_| ctx.open.set(false),
        }
        // Menu panel
        div {
            class: format!(
                "absolute z-[100] mt-2 min-w-48 rounded-lg bg-popover text-popover-foreground border border-border shadow-md p-1 {} {align_class} {}",
                props.width, props.class
            ),
            onclick: move |e| e.stop_propagation(),
            {props.children}
        }
    }
}

// ── Item ─────────────────────────────────────────────────────────────────────

#[derive(Props, Clone, PartialEq)]
pub struct DropdownItemProps {
    #[props(default)]
    pub disabled: bool,
    #[props(default = false)]
    pub destructive: bool,
    #[props(default)]
    pub on_select: Option<Callback<()>>,
    #[props(default)]
    pub icon: Option<Element>,
    #[props(default)]
    pub class: String,
    pub children: Element,
}

/// shadcn v4 maia: cn-dropdown-menu-item
#[component]
pub fn DropdownItem(props: DropdownItemProps) -> Element {
    let mut ctx: DropdownContext = use_context();

    let variant_class = if props.destructive {
        "text-destructive hover:bg-destructive/10"
    } else {
        "hover:bg-accent hover:text-accent-foreground"
    };

    let disabled_class = if props.disabled {
        "pointer-events-none opacity-50"
    } else {
        ""
    };

    rsx! {
        div {
            class: format!(
                "relative flex cursor-pointer select-none items-center rounded-xl px-3 py-2 text-sm transition-colors gap-2.5 [&_svg:not([class*='size-'])]:size-4 {variant_class} {disabled_class} {}",
                props.class
            ),
            onclick: move |_| {
                if !props.disabled {
                    if let Some(cb) = &props.on_select {
                        cb.call(());
                    }
                    ctx.open.set(false);
                }
            },
            if let Some(icon) = &props.icon {
                span { class: "flex-shrink-0", {icon} }
            }
            {props.children}
        }
    }
}

// ── Label ────────────────────────────────────────────────────────────────────

#[derive(Props, Clone, PartialEq)]
pub struct DropdownLabelProps {
    #[props(default)]
    pub class: String,
    pub children: Element,
}

/// shadcn v4 maia: cn-dropdown-menu-label
#[component]
pub fn DropdownLabel(props: DropdownLabelProps) -> Element {
    rsx! {
        div {
            class: format!("text-muted-foreground px-3 py-2.5 text-xs {}", props.class),
            {props.children}
        }
    }
}

// ── Separator ────────────────────────────────────────────────────────────────

#[derive(Props, Clone, PartialEq)]
pub struct DropdownSeparatorProps {
    #[props(default)]
    pub class: String,
}

/// shadcn v4 maia: cn-dropdown-menu-separator
#[component]
pub fn DropdownSeparator(props: DropdownSeparatorProps) -> Element {
    rsx! {
        div {
            class: format!("bg-border/50 -mx-1 my-1 h-px {}", props.class),
            role: "separator",
        }
    }
}
