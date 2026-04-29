//! Carousel — shadcn v4 maia style horizontal swipeable carousel.

use dioxus::prelude::*;

// ---------------------------------------------------------------------------
// Context
// ---------------------------------------------------------------------------

#[derive(Clone, Copy)]
struct CarouselContext {
    current: Signal<usize>,
    total: Signal<usize>,
}

// ---------------------------------------------------------------------------
// Carousel
// ---------------------------------------------------------------------------

#[derive(Props, Clone, PartialEq)]
pub struct CarouselProps {
    #[props(default)]
    pub class: String,
    pub children: Element,
}

/// Root carousel wrapper. Provides context for child navigation components.
#[component]
pub fn Carousel(props: CarouselProps) -> Element {
    let current = use_signal(|| 0usize);
    let total = use_signal(|| 0usize);
    use_context_provider(|| CarouselContext { current, total });

    rsx! {
        div {
            class: crate::cn::merge_slice(&["relative w-full overflow-hidden", props.class.as_str()]),
            {props.children}
        }
    }
}

// ---------------------------------------------------------------------------
// CarouselContent
// ---------------------------------------------------------------------------

#[derive(Props, Clone, PartialEq)]
pub struct CarouselContentProps {
    /// The number of slides. Required so the carousel knows the total count.
    pub count: usize,
    #[props(default)]
    pub class: String,
    pub children: Element,
}

/// The scrollable track that holds carousel items.
#[component]
pub fn CarouselContent(props: CarouselContentProps) -> Element {
    let mut ctx: CarouselContext = use_context();
    let count = props.count;
    use_effect(move || {
        ctx.total.set(count);
    });
    let current = *ctx.current.read();

    rsx! {
        div {
            class: crate::cn::merge_slice(&["flex transition-transform duration-300 ease-in-out", props.class.as_str()]),
            style: format!("transform: translateX(-{}%);", current * 100),
            {props.children}
        }
    }
}

// ---------------------------------------------------------------------------
// CarouselItem
// ---------------------------------------------------------------------------

#[derive(Props, Clone, PartialEq)]
pub struct CarouselItemProps {
    #[props(default)]
    pub class: String,
    pub children: Element,
}

/// A single slide within the carousel.
#[component]
pub fn CarouselItem(props: CarouselItemProps) -> Element {
    rsx! {
        div {
            class: crate::cn::merge_slice(&["min-w-full flex-shrink-0", props.class.as_str()]),
            {props.children}
        }
    }
}

// ---------------------------------------------------------------------------
// CarouselPrevious
// ---------------------------------------------------------------------------

#[derive(Props, Clone, PartialEq)]
pub struct CarouselPreviousProps {
    #[props(default)]
    pub class: String,
}

/// Previous-slide button with a left chevron.
#[component]
pub fn CarouselPrevious(props: CarouselPreviousProps) -> Element {
    let mut ctx: CarouselContext = use_context();
    let current = *ctx.current.read();
    let disabled = current == 0;

    rsx! {
        button {
            r#type: "button",
            class: crate::cn::merge(format!(
                "absolute left-2 top-1/2 -translate-y-1/2 z-10 inline-flex items-center justify-center size-8 rounded-full border border-input bg-background hover:bg-muted transition-colors disabled:opacity-50 {}",
                props.class
            )),
            disabled,
            onclick: move |_| {
                let cur = *ctx.current.read();
                if cur > 0 {
                    ctx.current.set(cur - 1);
                }
            },
            svg {
                xmlns: "http://www.w3.org/2000/svg",
                width: "16",
                height: "16",
                view_box: "0 0 24 24",
                fill: "none",
                stroke: "currentColor",
                stroke_width: "2",
                stroke_linecap: "round",
                stroke_linejoin: "round",
                class: "size-4",
                path { d: "m15 18-6-6 6-6" }
            }
        }
    }
}

// ---------------------------------------------------------------------------
// CarouselNext
// ---------------------------------------------------------------------------

#[derive(Props, Clone, PartialEq)]
pub struct CarouselNextProps {
    #[props(default)]
    pub class: String,
}

/// Next-slide button with a right chevron.
#[component]
pub fn CarouselNext(props: CarouselNextProps) -> Element {
    let mut ctx: CarouselContext = use_context();
    let current = *ctx.current.read();
    let total = *ctx.total.read();
    let disabled = total == 0 || current >= total - 1;

    rsx! {
        button {
            r#type: "button",
            class: crate::cn::merge(format!(
                "absolute right-2 top-1/2 -translate-y-1/2 z-10 inline-flex items-center justify-center size-8 rounded-full border border-input bg-background hover:bg-muted transition-colors disabled:opacity-50 {}",
                props.class
            )),
            disabled,
            onclick: move |_| {
                let cur = *ctx.current.read();
                let tot = *ctx.total.read();
                if tot > 0 && cur < tot - 1 {
                    ctx.current.set(cur + 1);
                }
            },
            svg {
                xmlns: "http://www.w3.org/2000/svg",
                width: "16",
                height: "16",
                view_box: "0 0 24 24",
                fill: "none",
                stroke: "currentColor",
                stroke_width: "2",
                stroke_linecap: "round",
                stroke_linejoin: "round",
                class: "size-4",
                path { d: "m9 18 6-6-6-6" }
            }
        }
    }
}
