//! Tabs — shadcn v4 maia style accessible tabbed interface.

use dioxus::prelude::*;

#[derive(Props, Clone, PartialEq)]
pub struct TabsProps {
    #[props(default = String::new())]
    pub default_value: String,
    #[props(default)]
    pub on_change: Option<Callback<String>>,
    #[props(default)]
    pub class: String,
    pub children: Element,
}

/// shadcn v4 maia: cn-tabs
#[component]
pub fn Tabs(props: TabsProps) -> Element {
    let selected = use_signal(|| props.default_value.clone());
    use_context_provider(|| TabContext {
        selected,
        on_change: props.on_change,
    });

    rsx! {
        div {
            class: crate::cn::merge_slice(&["flex flex-col gap-2", props.class.as_str()]),
            "data-orientation": "horizontal",
            {props.children}
        }
    }
}

#[derive(Clone, Copy)]
struct TabContext {
    selected: Signal<String>,
    on_change: Option<Callback<String>>,
}

#[derive(Props, Clone, PartialEq)]
pub struct TabListProps {
    #[props(default)]
    pub class: String,
    pub children: Element,
}

/// shadcn v4 maia: cn-tabs-list
#[component]
pub fn TabList(props: TabListProps) -> Element {
    rsx! {
        div {
            role: "tablist",
            class: crate::cn::merge(format!(
                "inline-flex h-9 items-center justify-start gap-1 rounded-full bg-muted p-[3px] text-muted-foreground {}",
                props.class
            )),
            {props.children}
        }
    }
}

#[derive(Props, Clone, PartialEq)]
pub struct TabTriggerProps {
    pub value: String,
    #[props(default)]
    pub class: String,
    pub children: Element,
}

/// shadcn v4 maia: cn-tabs-trigger
#[component]
pub fn TabTrigger(props: TabTriggerProps) -> Element {
    let mut ctx: TabContext = use_context();
    let is_selected = *ctx.selected.read() == props.value;

    let active_class = if is_selected {
        "bg-background text-foreground shadow-sm"
    } else {
        "hover:bg-background/50 hover:text-foreground/80"
    };

    let value = props.value.clone();

    rsx! {
        button {
            role: "tab",
            r#type: "button",
            class: crate::cn::merge(format!(
                "inline-flex items-center justify-center gap-1.5 whitespace-nowrap rounded-xl border border-transparent px-2 py-1 text-sm font-medium transition-all focus-visible:outline-none focus-visible:ring-2 focus-visible:ring-ring disabled:pointer-events-none disabled:opacity-50 [&_svg:not([class*='size-'])]:size-4 {active_class} {}",
                props.class
            )),
            aria_selected: is_selected.to_string(),
            "data-state": if is_selected { "active" } else { "inactive" },
            tabindex: if is_selected { "0" } else { "-1" },
            onclick: move |_| {
                let val = value.clone();
                ctx.selected.set(val.clone());
                if let Some(cb) = &ctx.on_change {
                    cb.call(val);
                }
            },
            {props.children}
        }
    }
}

#[derive(Props, Clone, PartialEq)]
pub struct TabContentProps {
    pub value: String,
    #[props(default)]
    pub class: String,
    pub children: Element,
}

/// shadcn v4 maia: cn-tabs-content
#[component]
pub fn TabContent(props: TabContentProps) -> Element {
    let ctx: TabContext = use_context();
    let is_selected = *ctx.selected.read() == props.value;

    if !is_selected {
        return rsx! {};
    }

    rsx! {
        div {
            role: "tabpanel",
            class: crate::cn::merge_slice(&["text-sm", props.class.as_str()]),
            "data-state": "active",
            tabindex: "0",
            {props.children}
        }
    }
}
