//! Asset embed — v1 scope: image only.
//!
//! Lookup is done via `asset_url_by_id` (parent component resolves
//! the asset → URL mapping; we don't reach into the asset feature
//! directly to keep `knowledge-ui` proto-light). Missing assets
//! render a grey placeholder. Non-image asset kinds render a
//! generic file-icon card with the filename — the kind/filename is
//! inferred from the URL extension since the asset feature lives
//! behind a separate proto we don't take as a dep here.

use dioxus::prelude::*;
use fts_ui::lucide_dioxus::File;
use knowledge_proto::canvas::CanvasNode;
use std::collections::HashMap;
use uuid::Uuid;

#[derive(Props, Clone, PartialEq)]
pub struct AssetProps {
    pub block_id: Uuid,
    pub node: CanvasNode,
    pub asset_id: Uuid,
    /// `Asset.id` → resolvable URL (data: / blob: / http:). `None`
    /// renders a grey placeholder.
    pub asset_url_by_id: HashMap<Uuid, String>,
    pub selected: bool,
}

#[component]
pub fn Asset(props: AssetProps) -> Element {
    let ring = if props.selected {
        "outline outline-2 outline-primary"
    } else {
        ""
    };
    let style = format!(
        "left: {x}px; top: {y}px; width: {w}px; height: {h}px; transform: rotate({rot}deg); transform-origin: center;",
        x = props.node.x,
        y = props.node.y,
        w = props.node.w,
        h = props.node.h,
        rot = props.node.rotation,
    );
    let url = props.asset_url_by_id.get(&props.asset_id).cloned();
    let is_image = url
        .as_deref()
        .map(|u| {
            let lower = u.to_ascii_lowercase();
            lower.ends_with(".png")
                || lower.ends_with(".jpg")
                || lower.ends_with(".jpeg")
                || lower.ends_with(".gif")
                || lower.ends_with(".webp")
                || lower.ends_with(".svg")
                || lower.starts_with("data:image/")
                || lower.starts_with("blob:")
        })
        .unwrap_or(false);

    match (url, is_image) {
        (Some(u), true) => rsx! {
            div { class: "absolute overflow-hidden rounded-md border border-border bg-card {ring}", style: "{style}",
                img {
                    src: "{u}",
                    class: "w-full h-full object-contain pointer-events-none",
                    alt: "",
                }
            }
        },
        (Some(u), false) => {
            let filename = u.rsplit('/').next().unwrap_or(&u).to_string();
            rsx! {
                div { class: "absolute flex items-center gap-2 p-2 rounded-md border border-border bg-card text-card-foreground shadow-sm {ring}", style: "{style}",
                    File { size: 16 }
                    span { class: "text-xs truncate", "{filename}" }
                }
            }
        }
        (None, _) => rsx! {
            div { class: "absolute rounded-md bg-muted border border-dashed border-border flex items-center justify-center {ring}", style: "{style}",
                span { class: "text-xs text-muted-foreground", "missing asset" }
            }
        },
    }
}
