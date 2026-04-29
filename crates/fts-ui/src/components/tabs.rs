//! Tabs — shadcn v4 maia style accessible tabbed interface.

use dioxus::prelude::*;
use dioxus_primitives::tabs::{
    TabContent as PrimitiveTabContent, TabList as PrimitiveTabList,
    TabTrigger as PrimitiveTabTrigger, Tabs as PrimitiveTabs,
};

#[derive(Props, Clone, PartialEq)]
pub struct TabsProps {
    #[props(default)]
    pub value: Option<String>,
    #[props(default = String::new())]
    pub default_value: String,
    #[props(default)]
    pub on_change: Option<Callback<String>>,
    #[props(default = false)]
    pub disabled: bool,
    #[props(default = true)]
    pub horizontal: bool,
    #[props(default = true)]
    pub roving_loop: bool,
    #[props(default)]
    pub class: String,
    pub children: Element,
}

/// shadcn v4 maia: cn-tabs
#[component]
pub fn Tabs(props: TabsProps) -> Element {
    rsx! {
        PrimitiveTabs {
            value: props.value,
            default_value: props.default_value,
            disabled: props.disabled,
            horizontal: props.horizontal,
            roving_loop: props.roving_loop,
            on_value_change: move |value: String| {
                if let Some(callback) = &props.on_change {
                    callback.call(value);
                }
            },
            class: crate::cn::merge_slice(&["flex flex-col gap-2", props.class.as_str()]),
            {props.children}
        }
    }
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
        PrimitiveTabList {
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
    pub index: usize,
    #[props(default = false)]
    pub disabled: bool,
    #[props(default)]
    pub id: Option<String>,
    #[props(default)]
    pub class: String,
    pub children: Element,
}

/// shadcn v4 maia: cn-tabs-trigger
#[component]
pub fn TabTrigger(props: TabTriggerProps) -> Element {
    rsx! {
        PrimitiveTabTrigger {
            value: props.value,
            index: props.index,
            disabled: props.disabled,
            id: props.id,
            class: crate::cn::merge(format!(
                "inline-flex items-center justify-center gap-1.5 whitespace-nowrap rounded-xl border border-transparent px-2 py-1 text-sm font-medium transition-all focus-visible:outline-none focus-visible:ring-2 focus-visible:ring-ring disabled:pointer-events-none disabled:opacity-50 [&_svg:not([class*='size-'])]:size-4 data-[state=active]:bg-background data-[state=active]:text-foreground data-[state=active]:shadow-sm data-[state=inactive]:hover:bg-background/50 data-[state=inactive]:hover:text-foreground/80 {}",
                props.class
            )),
            {props.children}
        }
    }
}

#[derive(Props, Clone, PartialEq)]
pub struct TabContentProps {
    pub value: String,
    pub index: usize,
    #[props(default)]
    pub id: Option<String>,
    #[props(default)]
    pub class: String,
    pub children: Element,
}

/// shadcn v4 maia: cn-tabs-content
#[component]
pub fn TabContent(props: TabContentProps) -> Element {
    rsx! {
        PrimitiveTabContent {
            value: props.value,
            index: props.index,
            id: props.id,
            class: crate::cn::merge_slice(&["text-sm", props.class.as_str()]),
            {props.children}
        }
    }
}
