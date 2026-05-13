//! Rendered inline-span Dioxus components. Only used when the per-span
//! `SpanDecoration::Rendered` is in effect.

use dioxus::prelude::*;
use uuid::Uuid;

#[component]
pub fn WikilinkSpan(
    target: String,
    heading: Option<String>,
    block_id: Option<String>,
    alias: Option<String>,
    raw: String,
    on_click: EventHandler<()>,
    on_hover: EventHandler<()>,
) -> Element {
    let label = alias
        .clone()
        .unwrap_or_else(|| match (&heading, &block_id) {
            (Some(h), _) => format!("{target} › {h}"),
            (_, Some(b)) => format!("{target} ^{b}"),
            _ => target.clone(),
        });
    let _ = raw;
    rsx! {
        span {
            class: "text-primary hover:underline cursor-pointer",
            "data-wikilink-target": "{target}",
            onmouseenter: move |_| on_hover.call(()),
            onclick: move |_| on_click.call(()),
            "{label}"
        }
    }
}

#[component]
pub fn TagSpan(path: Vec<String>, raw: String, on_click: EventHandler<()>) -> Element {
    let _ = raw;
    let label = path.join("/");
    rsx! {
        span {
            class: "text-muted-foreground bg-muted/40 rounded px-1 cursor-pointer hover:bg-muted/60",
            onclick: move |_| on_click.call(()),
            "#{label}"
        }
    }
}

#[component]
pub fn BlockRefSpan(
    target_id: Uuid,
    target_content: Option<String>,
    raw: String,
    on_click: EventHandler<()>,
    on_hover: EventHandler<()>,
) -> Element {
    let _ = raw;
    let label = target_content
        .clone()
        .unwrap_or_else(|| format!("(block {})", short_id(&target_id)));
    rsx! {
        span {
            class: "border-l-2 border-primary/40 bg-muted/30 rounded px-2 italic cursor-pointer",
            onmouseenter: move |_| on_hover.call(()),
            onclick: move |_| on_click.call(()),
            "{label}"
        }
    }
}

#[component]
pub fn EntityLinkSpan(
    kind: String,
    id: Uuid,
    alias: Option<String>,
    raw: String,
    on_click: EventHandler<()>,
) -> Element {
    let _ = raw;
    let label = alias.unwrap_or_else(|| format!("{kind}/{}", short_id(&id)));
    rsx! {
        span {
            class: "text-accent-foreground bg-accent/40 rounded px-1 cursor-pointer hover:bg-accent/60",
            onclick: move |_| on_click.call(()),
            "{label}"
        }
    }
}

fn short_id(id: &Uuid) -> String {
    let s = id.to_string();
    s.split('-').next().unwrap_or(&s).to_string()
}
