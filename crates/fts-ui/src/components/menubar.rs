//! Menubar — shadcn v4 maia style horizontal menu bar.

use dioxus::prelude::*;

// ── Context ──────────────────────────────────────────────────────────────────

#[derive(Clone, Copy)]
struct MenubarContext {
    active_menu: Signal<Option<String>>,
}

#[derive(Clone)]
struct MenubarMenuContext {
    value: String,
}

// ── Menubar ─────────────────────────────────────────────────────────────────

#[derive(Props, Clone, PartialEq)]
pub struct MenubarProps {
    #[props(default)]
    pub class: String,
    pub children: Element,
}

/// shadcn v4 maia: menubar root container
#[component]
pub fn Menubar(props: MenubarProps) -> Element {
    let active_menu = use_signal(|| None::<String>);
    use_context_provider(|| MenubarContext { active_menu });

    rsx! {
        div {
            class: crate::cn::merge(format!(
                "inline-flex items-center h-9 rounded-lg border p-1 gap-1 {}",
                props.class
            )),
            {props.children}
        }
    }
}

// ── MenubarMenu ─────────────────────────────────────────────────────────────

#[derive(Props, Clone, PartialEq)]
pub struct MenubarMenuProps {
    /// Unique identifier for this menu.
    pub value: String,
    #[props(default)]
    pub class: String,
    pub children: Element,
}

/// shadcn v4 maia: single menu within the bar (e.g. "File")
#[component]
pub fn MenubarMenu(props: MenubarMenuProps) -> Element {
    use_context_provider(|| MenubarMenuContext {
        value: props.value.clone(),
    });

    rsx! {
        div {
            class: crate::cn::merge_slice(&["relative", props.class.as_str()]),
            {props.children}
        }
    }
}

// ── MenubarTrigger ──────────────────────────────────────────────────────────

#[derive(Props, Clone, PartialEq)]
pub struct MenubarTriggerProps {
    #[props(default)]
    pub class: String,
    pub children: Element,
}

/// shadcn v4 maia: button that opens a menu
#[component]
pub fn MenubarTrigger(props: MenubarTriggerProps) -> Element {
    let mut ctx: MenubarContext = use_context();
    let menu_ctx: MenubarMenuContext = use_context();
    let menu_value = menu_ctx.value.clone();

    let is_active = ctx
        .active_menu
        .read()
        .as_ref()
        .map(|v| v == &menu_value)
        .unwrap_or(false);

    let active_class = if is_active { "bg-muted" } else { "" };

    let toggle_value = menu_value.clone();

    rsx! {
        button {
            r#type: "button",
            class: crate::cn::merge(format!(
                "inline-flex items-center justify-center rounded-xl px-2 py-0.5 text-sm font-medium hover:bg-muted cursor-pointer select-none transition-colors {active_class} {}",
                props.class
            )),
            onclick: move |_| {
                let current = ctx.active_menu.read().clone();
                if current.as_deref() == Some(&toggle_value) {
                    ctx.active_menu.set(None);
                } else {
                    ctx.active_menu.set(Some(toggle_value.clone()));
                }
            },
            {props.children}
        }
    }
}

// ── MenubarContent ──────────────────────────────────────────────────────────

#[derive(Props, Clone, PartialEq)]
pub struct MenubarContentProps {
    #[props(default)]
    pub class: String,
    pub children: Element,
}

/// shadcn v4 maia: dropdown panel for a menu
#[component]
pub fn MenubarContent(props: MenubarContentProps) -> Element {
    let mut ctx: MenubarContext = use_context();
    let menu_ctx: MenubarMenuContext = use_context();

    let is_active = ctx
        .active_menu
        .read()
        .as_ref()
        .map(|v| v == &menu_ctx.value)
        .unwrap_or(false);

    if !is_active {
        return rsx! {};
    }

    rsx! {
        // Invisible backdrop to close on outside click
        div {
            class: "fixed inset-0 z-[99]",
            onclick: move |_| ctx.active_menu.set(None),
        }
        // Menu panel
        div {
            class: crate::cn::merge(format!(
                "absolute left-0 top-full z-[100] mt-1 min-w-48 rounded-lg bg-popover text-popover-foreground border border-border shadow-md p-1 {}",
                props.class
            )),
            onclick: move |e| e.stop_propagation(),
            {props.children}
        }
    }
}

// ── MenubarItem ─────────────────────────────────────────────────────────────

#[derive(Props, Clone, PartialEq)]
pub struct MenubarItemProps {
    #[props(default)]
    pub on_select: Option<Callback<()>>,
    #[props(default)]
    pub disabled: bool,
    #[props(default)]
    pub class: String,
    pub children: Element,
}

/// shadcn v4 maia: menubar item
#[component]
pub fn MenubarItem(props: MenubarItemProps) -> Element {
    let mut ctx: MenubarContext = use_context();

    let disabled_class = if props.disabled {
        "pointer-events-none opacity-50"
    } else {
        ""
    };

    rsx! {
        div {
            class: crate::cn::merge(format!(
                "flex cursor-pointer select-none items-center rounded-xl px-3 py-2 text-sm hover:bg-accent hover:text-accent-foreground transition-colors gap-2.5 {disabled_class} {}",
                props.class
            )),
            onclick: move |_| {
                if !props.disabled {
                    if let Some(cb) = &props.on_select {
                        cb.call(());
                    }
                    ctx.active_menu.set(None);
                }
            },
            {props.children}
        }
    }
}

// ── MenubarSeparator ────────────────────────────────────────────────────────

#[derive(Props, Clone, PartialEq)]
pub struct MenubarSeparatorProps {
    #[props(default)]
    pub class: String,
}

/// shadcn v4 maia: menubar separator
#[component]
pub fn MenubarSeparator(props: MenubarSeparatorProps) -> Element {
    rsx! {
        div {
            class: crate::cn::merge_slice(&["bg-border/50 -mx-1 my-1 h-px", props.class.as_str()]),
            role: "separator",
        }
    }
}

// ── MenubarLabel ────────────────────────────────────────────────────────────

#[derive(Props, Clone, PartialEq)]
pub struct MenubarLabelProps {
    #[props(default)]
    pub class: String,
    pub children: Element,
}

/// shadcn v4 maia: menubar group label
#[component]
pub fn MenubarLabel(props: MenubarLabelProps) -> Element {
    rsx! {
        div {
            class: crate::cn::merge_slice(&["text-muted-foreground px-3 py-2.5 text-xs", props.class.as_str()]),
            {props.children}
        }
    }
}

// ── MenubarShortcut ─────────────────────────────────────────────────────────

#[derive(Props, Clone, PartialEq)]
pub struct MenubarShortcutProps {
    #[props(default)]
    pub class: String,
    pub children: Element,
}

/// shadcn v4 maia: keyboard shortcut hint
#[component]
pub fn MenubarShortcut(props: MenubarShortcutProps) -> Element {
    rsx! {
        span {
            class: crate::cn::merge_slice(&["ml-auto text-xs tracking-widest text-muted-foreground", props.class.as_str()]),
            {props.children}
        }
    }
}
