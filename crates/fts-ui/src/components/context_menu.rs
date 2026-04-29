//! ContextMenu — standalone, shadcn v4 maia style.

use dioxus::prelude::*;

// ---------------------------------------------------------------------------
// ContextMenu (root)
// ---------------------------------------------------------------------------

#[derive(Props, Clone, PartialEq)]
pub struct ContextMenuProps {
    #[props(default)]
    pub class: String,
    pub children: Element,
}

/// Root wrapper that tracks right-click state (open, x, y) via context.
#[component]
pub fn ContextMenu(props: ContextMenuProps) -> Element {
    let state = use_signal(|| ContextMenuState {
        open: false,
        x: 0.0,
        y: 0.0,
    });

    use_context_provider(|| state);

    rsx! {
        div {
            class: format!("relative {}", props.class),
            {props.children}
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
struct ContextMenuState {
    open: bool,
    x: f64,
    y: f64,
}

// ---------------------------------------------------------------------------
// ContextMenuTrigger
// ---------------------------------------------------------------------------

#[derive(Props, Clone, PartialEq)]
pub struct ContextMenuTriggerProps {
    pub children: Element,
}

/// Area that responds to right-click.
#[component]
pub fn ContextMenuTrigger(props: ContextMenuTriggerProps) -> Element {
    let mut state = use_context::<Signal<ContextMenuState>>();

    rsx! {
        div {
            oncontextmenu: move |evt: MouseEvent| {
                evt.prevent_default();
                let coords = evt.page_coordinates();
                state.write().open = true;
                state.write().x = coords.x;
                state.write().y = coords.y;
            },
            {props.children}
        }
    }
}

// ---------------------------------------------------------------------------
// ContextMenuContent
// ---------------------------------------------------------------------------

#[derive(Props, Clone, PartialEq)]
pub struct ContextMenuContentProps {
    #[props(default)]
    pub class: String,
    pub children: Element,
}

/// The popup menu that appears at the right-click position.
#[component]
pub fn ContextMenuContent(props: ContextMenuContentProps) -> Element {
    let mut state = use_context::<Signal<ContextMenuState>>();
    let s = *state.read();

    if !s.open {
        return rsx! {};
    }

    rsx! {
        // Invisible overlay to catch clicks outside the menu.
        div {
            class: "fixed inset-0 z-40",
            onclick: move |_| { state.write().open = false; },
        }

        // Menu
        div {
            class: format!(
                "fixed z-50 min-w-48 rounded-lg bg-popover text-popover-foreground border border-border shadow-md p-1 {}",
                props.class
            ),
            style: format!("left: {}px; top: {}px;", s.x, s.y),
            onclick: move |evt: MouseEvent| { evt.stop_propagation(); },
            {props.children}
        }
    }
}

// ---------------------------------------------------------------------------
// ContextMenuItem
// ---------------------------------------------------------------------------

#[derive(Props, Clone, PartialEq)]
pub struct ContextMenuItemProps {
    #[props(default)]
    pub on_select: Option<Callback<()>>,
    #[props(default = false)]
    pub destructive: bool,
    #[props(default = false)]
    pub disabled: bool,
    #[props(default)]
    pub class: String,
    pub children: Element,
}

#[component]
pub fn ContextMenuItem(props: ContextMenuItemProps) -> Element {
    let mut state = use_context::<Signal<ContextMenuState>>();
    let on_select = props.on_select;
    let disabled = props.disabled;

    let base = "flex cursor-pointer select-none items-center rounded-xl px-3 py-2 text-sm hover:bg-accent hover:text-accent-foreground transition-colors gap-2.5";
    let variant = if props.destructive {
        "text-destructive hover:bg-destructive/10"
    } else {
        ""
    };
    let disabled_class = if disabled {
        "opacity-50 pointer-events-none"
    } else {
        ""
    };

    rsx! {
        div {
            class: format!("{base} {variant} {disabled_class} {}", props.class),
            role: "menuitem",
            onclick: move |_| {
                if !disabled {
                    if let Some(cb) = &on_select {
                        cb.call(());
                    }
                    state.write().open = false;
                }
            },
            {props.children}
        }
    }
}

// ---------------------------------------------------------------------------
// ContextMenuSeparator
// ---------------------------------------------------------------------------

/// Visual separator between groups of menu items.
#[component]
pub fn ContextMenuSeparator() -> Element {
    rsx! {
        div { class: "bg-border/50 -mx-1 my-1 h-px" }
    }
}

// ---------------------------------------------------------------------------
// ContextMenuLabel
// ---------------------------------------------------------------------------

#[derive(Props, Clone, PartialEq)]
pub struct ContextMenuLabelProps {
    #[props(default)]
    pub class: String,
    pub children: Element,
}

/// Non-interactive label / group heading.
#[component]
pub fn ContextMenuLabel(props: ContextMenuLabelProps) -> Element {
    rsx! {
        div {
            class: format!("text-muted-foreground px-3 py-2.5 text-xs {}", props.class),
            {props.children}
        }
    }
}
