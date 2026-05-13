//! Group container — a dashed bordered frame with a top-left label.
//! Children Blocks reference the group via `parent_block_id`; this
//! widget is purely a visual frame (the Whiteboard component handles
//! parent→child translation when a group is dragged).

use dioxus::prelude::*;
use knowledge_proto::canvas::CanvasNode;
use uuid::Uuid;

#[derive(Props, Clone, PartialEq)]
pub struct GroupProps {
    pub block_id: Uuid,
    pub node: CanvasNode,
    /// Group label — usually the parent Block's `content` first line.
    pub label: String,
    pub selected: bool,
}

#[component]
pub fn Group(props: GroupProps) -> Element {
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
    rsx! {
        div {
            class: "absolute rounded-md border border-dashed border-border bg-muted/20 {ring}",
            style: "{style}",
            div { class: "absolute -top-5 left-0 text-xs text-muted-foreground px-1.5 py-0.5 rounded bg-background border border-border",
                "{props.label}"
            }
        }
    }
}
