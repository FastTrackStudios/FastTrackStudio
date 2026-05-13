//! Drawn-primitive shapes — Rect, Ellipse, Diamond, StickyNote, Text.
//!
//! Each shape is a positioned div whose `width`/`height` come from
//! the `CanvasNode`'s `w`/`h` (world units; the outer transform
//! scales for zoom). Content for text-bearing shapes is the parent
//! Block's `content` field; double-clicking flips into an editable
//! `<textarea>` overlay — that's wired in `whiteboard.rs`.

use dioxus::prelude::*;
use knowledge_proto::canvas::{CanvasNode, CanvasShape};
use uuid::Uuid;

/// Which drawn-primitive kind this widget should render.
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum ShapeKind {
    Rect,
    Ellipse,
    Diamond,
    StickyNote,
    Text,
}

impl ShapeKind {
    pub fn from_canvas_shape(s: &CanvasShape) -> Option<ShapeKind> {
        Some(match s {
            CanvasShape::Rect => ShapeKind::Rect,
            CanvasShape::Ellipse => ShapeKind::Ellipse,
            CanvasShape::Diamond => ShapeKind::Diamond,
            CanvasShape::StickyNote => ShapeKind::StickyNote,
            CanvasShape::Text => ShapeKind::Text,
            _ => return None,
        })
    }
}

#[derive(Props, Clone, PartialEq)]
pub struct ShapeRectProps {
    pub block_id: Uuid,
    pub node: CanvasNode,
    pub content: String,
    pub kind: ShapeKind,
    pub selected: bool,
    pub editing: bool,
    pub on_text_commit: EventHandler<String>,
}

#[component]
pub fn ShapeRect(props: ShapeRectProps) -> Element {
    let ShapeRectProps {
        node,
        content,
        kind,
        selected,
        editing,
        on_text_commit,
        ..
    } = props;

    let base = "absolute flex items-center justify-center text-sm overflow-hidden";
    // Theme-token coloring. Sticky note uses the accent palette to
    // keep light/dark parity without hard-coding yellow.
    let palette = match kind {
        ShapeKind::StickyNote => {
            "bg-accent text-accent-foreground border border-accent-foreground/20 shadow-sm"
        }
        ShapeKind::Text => "bg-transparent text-foreground",
        _ => "bg-card text-card-foreground border border-border",
    };
    let shape_class = match kind {
        ShapeKind::Ellipse => "rounded-full",
        ShapeKind::StickyNote | ShapeKind::Rect => "rounded-md",
        ShapeKind::Diamond | ShapeKind::Text => "",
    };
    let selected_ring = if selected {
        "outline outline-2 outline-primary"
    } else {
        ""
    };

    // Diamond is a rotated square via clip-path so the AABB still
    // matches the world rect.
    let extra_style = match kind {
        ShapeKind::Diamond => "clip-path: polygon(50% 0%, 100% 50%, 50% 100%, 0% 50%);",
        _ => "",
    };

    let style = format!(
        "left: {x}px; top: {y}px; width: {w}px; height: {h}px; transform: rotate({rot}deg); transform-origin: center; {extra}",
        x = node.x,
        y = node.y,
        w = node.w,
        h = node.h,
        rot = node.rotation,
        extra = extra_style,
    );
    let class = format!("{base} {palette} {shape_class} {selected_ring}");

    rsx! {
        div { class: "{class}", style: "{style}",
            if editing {
                EditableTextarea { initial: content.clone(), on_commit: on_text_commit }
            } else {
                div { class: "p-2 whitespace-pre-wrap break-words text-center",
                    "{content}"
                }
            }
        }
    }
}

#[derive(Props, Clone, PartialEq)]
struct EditableTextareaProps {
    initial: String,
    on_commit: EventHandler<String>,
}

#[component]
fn EditableTextarea(props: EditableTextareaProps) -> Element {
    let mut draft = use_signal(|| props.initial.clone());
    rsx! {
        textarea {
            class: "w-full h-full p-2 bg-transparent text-foreground resize-none outline-none",
            value: "{draft.read()}",
            oninput: move |evt| draft.set(evt.value()),
            onblur: move |_| props.on_commit.call(draft.read().clone()),
        }
    }
}
