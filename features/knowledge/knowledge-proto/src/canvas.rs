//! Canvas/whiteboard placement schema. A whiteboard is a `Page` with
//! `ext = "canvas"`; its top-level Blocks carry [`CanvasNode`] in
//! `canvas_node_json`. The key property: a Block referenced from a
//! whiteboard IS the same block as the one in its source page —
//! editing in either place updates the other (Logseq's improvement
//! over Obsidian Canvas, which copies content into the JSON).
//!
//! Drawn primitives (rectangles, sticky notes, freehand strokes) are
//! also Blocks — they just carry the visual data here and have an
//! empty (or sticky-note text) `content` field. Reference cards point
//! at other Blocks by UUID via [`CanvasShape::BlockRef`].
//!
//! `.canvas` JSON files (Obsidian Canvas) parse/serialize through
//! [`parse_obsidian_canvas`] / [`serialize_obsidian_canvas`]. Round-trip
//! is best-effort — Obsidian's format predates ours and uses different
//! reference resolution (file paths vs block UUIDs).

use serde::{Deserialize, Serialize};
use uuid::Uuid;

/// Spatial + visual placement of a Block on a whiteboard.
#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct CanvasNode {
    pub x: f64,
    pub y: f64,
    pub w: f64,
    pub h: f64,
    /// Z-order. Higher renders on top.
    #[serde(default)]
    pub z: i32,
    /// Degrees, clockwise.
    #[serde(default)]
    pub rotation: f32,
    pub shape: CanvasShape,
    /// Theme token name (e.g. `"primary"`, `"muted"`, `"destructive"`),
    /// NOT a raw hex color — keeps the canvas theme-aware so theme
    /// switches recolor the whole board.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub color: Option<String>,
    /// Pinned in place; selection skips locked nodes by default.
    #[serde(default)]
    pub locked: bool,
    /// Free-form per-shape data (e.g. pen stroke points). Kept opaque
    /// so the schema can grow without proto changes.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub extras_json: Option<String>,
}

/// Shape variants. `BlockRef` and `PageRef` are *live references* —
/// rendering pulls content from the target block/page on every render.
/// Drawn primitives use the parent Block's `content` field for any
/// embedded text.
#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
#[serde(tag = "kind")]
pub enum CanvasShape {
    /// Live reference to another Block by UUID. Edits sync both ways.
    BlockRef {
        target_block_id: Uuid,
        #[serde(default)]
        view: BlockRefView,
    },
    /// Live reference to a whole Page.
    PageRef {
        target_page_id: Uuid,
    },
    Rect,
    Ellipse,
    Diamond,
    StickyNote,
    /// Freehand stroke. Point stream lives in `CanvasNode.extras_json`
    /// as `{"points": [[x,y], ...], "thickness": 2.0}`.
    Pen,
    /// Plain text node (single-line / wrapping).
    Text,
    /// Connector between two block-id-addressed endpoints.
    Connector {
        from: ConnectorEnd,
        to: ConnectorEnd,
        #[serde(default)]
        style: ConnectorStyle,
        #[serde(default, skip_serializing_if = "Option::is_none")]
        label: Option<String>,
    },
    /// Image / file / pdf embed.
    Asset {
        asset_id: Uuid,
    },
    /// Group container — children Blocks have `parent_block_id`
    /// pointing here. Drag-moving the group translates children.
    Group,
}

/// How much of a referenced block's content to render.
#[derive(Clone, Copy, Debug, PartialEq, Default, Serialize, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub enum BlockRefView {
    /// First line only with a "more" affordance.
    Compact,
    /// Full block content (single block, no children).
    #[default]
    Full,
    /// Full block + collapsed-by-default children outline.
    Outline,
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct ConnectorEnd {
    pub block_id: Uuid,
    #[serde(default)]
    pub anchor: Anchor,
}

#[derive(Clone, Copy, Debug, PartialEq, Default, Serialize, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub enum Anchor {
    #[default]
    Auto,
    Top,
    Right,
    Bottom,
    Left,
    Center,
}

#[derive(Clone, Debug, PartialEq, Default, Serialize, Deserialize)]
pub struct ConnectorStyle {
    #[serde(default)]
    pub line: ConnectorLine,
    #[serde(default)]
    pub start_arrow: ArrowHead,
    #[serde(default = "default_end_arrow")]
    pub end_arrow: ArrowHead,
    /// Theme token name. None inherits foreground.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub color: Option<String>,
}

fn default_end_arrow() -> ArrowHead {
    ArrowHead::Triangle
}

#[derive(Clone, Copy, Debug, PartialEq, Default, Serialize, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub enum ConnectorLine {
    #[default]
    Solid,
    Dashed,
    Dotted,
}

#[derive(Clone, Copy, Debug, PartialEq, Default, Serialize, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub enum ArrowHead {
    #[default]
    None,
    Triangle,
    Diamond,
    Circle,
    Bar,
}

// ── Serialization helpers ────────────────────────────────────────────

/// Encode a `CanvasNode` to the JSON blob stored on `Block.canvas_node_json`.
pub fn encode(node: &CanvasNode) -> String {
    serde_json::to_string(node).unwrap_or_else(|_| "null".into())
}

/// Decode the JSON blob back to `CanvasNode`. Returns `None` for empty
/// or unparseable input.
pub fn decode(json: &str) -> Option<CanvasNode> {
    if json.is_empty() {
        return None;
    }
    serde_json::from_str(json).ok()
}

// ── Obsidian Canvas (.canvas) interop ────────────────────────────────

/// Obsidian Canvas file shape (1.0 spec, see
/// <https://jsoncanvas.org/spec/1.0/>). One file = one whiteboard.
#[derive(Clone, Debug, Default, Serialize, Deserialize)]
pub struct ObsidianCanvas {
    #[serde(default)]
    pub nodes: Vec<ObsidianCanvasNode>,
    #[serde(default)]
    pub edges: Vec<ObsidianCanvasEdge>,
}

/// All variants share x/y/w/h + id; `type` discriminates.
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct ObsidianCanvasNode {
    pub id: String,
    pub x: f64,
    pub y: f64,
    pub width: f64,
    pub height: f64,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub color: Option<String>,

    #[serde(rename = "type")]
    pub kind: String, // "text" | "file" | "link" | "group"

    // type = "text"
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub text: Option<String>,

    // type = "file"
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub file: Option<String>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub subpath: Option<String>, // "#Heading" or "#^block-id"

    // type = "link"
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub url: Option<String>,

    // type = "group"
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub label: Option<String>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub background: Option<String>,
    #[serde(
        rename = "backgroundStyle",
        default,
        skip_serializing_if = "Option::is_none"
    )]
    pub background_style: Option<String>,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct ObsidianCanvasEdge {
    pub id: String,
    #[serde(rename = "fromNode")]
    pub from_node: String,
    #[serde(default, rename = "fromSide", skip_serializing_if = "Option::is_none")]
    pub from_side: Option<String>, // "top" | "right" | "bottom" | "left"
    #[serde(rename = "toNode")]
    pub to_node: String,
    #[serde(default, rename = "toSide", skip_serializing_if = "Option::is_none")]
    pub to_side: Option<String>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub color: Option<String>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub label: Option<String>,
}

/// Parse a `.canvas` file's JSON.
pub fn parse_obsidian_canvas(src: &str) -> Result<ObsidianCanvas, serde_json::Error> {
    serde_json::from_str(src)
}

/// Serialize back to `.canvas` format.
pub fn serialize_obsidian_canvas(c: &ObsidianCanvas) -> String {
    serde_json::to_string_pretty(c).unwrap_or_else(|_| "{}".into())
}

/// Convert an Obsidian Canvas edge side to our anchor enum.
pub fn anchor_from_side(side: Option<&str>) -> Anchor {
    match side {
        Some("top") => Anchor::Top,
        Some("right") => Anchor::Right,
        Some("bottom") => Anchor::Bottom,
        Some("left") => Anchor::Left,
        _ => Anchor::Auto,
    }
}

pub fn side_from_anchor(anchor: Anchor) -> Option<&'static str> {
    match anchor {
        Anchor::Top => Some("top"),
        Anchor::Right => Some("right"),
        Anchor::Bottom => Some("bottom"),
        Anchor::Left => Some("left"),
        Anchor::Center | Anchor::Auto => None,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn rect_round_trip() {
        let n = CanvasNode {
            x: 10.0,
            y: 20.0,
            w: 100.0,
            h: 50.0,
            z: 0,
            rotation: 0.0,
            shape: CanvasShape::Rect,
            color: Some("primary".into()),
            locked: false,
            extras_json: None,
        };
        let j = encode(&n);
        let n2 = decode(&j).unwrap();
        assert_eq!(n, n2);
    }

    #[test]
    fn block_ref_round_trip() {
        let target = Uuid::new_v4();
        let n = CanvasNode {
            x: 0.0,
            y: 0.0,
            w: 240.0,
            h: 120.0,
            z: 2,
            rotation: 0.0,
            shape: CanvasShape::BlockRef {
                target_block_id: target,
                view: BlockRefView::Full,
            },
            color: None,
            locked: false,
            extras_json: None,
        };
        let n2 = decode(&encode(&n)).unwrap();
        assert_eq!(n, n2);
        if let CanvasShape::BlockRef {
            target_block_id, ..
        } = n2.shape
        {
            assert_eq!(target_block_id, target);
        } else {
            panic!("wrong variant");
        }
    }

    #[test]
    fn connector_round_trip() {
        let a = Uuid::new_v4();
        let b = Uuid::new_v4();
        let n = CanvasNode {
            x: 0.0,
            y: 0.0,
            w: 0.0,
            h: 0.0,
            z: 1,
            rotation: 0.0,
            shape: CanvasShape::Connector {
                from: ConnectorEnd {
                    block_id: a,
                    anchor: Anchor::Right,
                },
                to: ConnectorEnd {
                    block_id: b,
                    anchor: Anchor::Left,
                },
                style: ConnectorStyle {
                    line: ConnectorLine::Dashed,
                    start_arrow: ArrowHead::None,
                    end_arrow: ArrowHead::Triangle,
                    color: None,
                },
                label: Some("depends on".into()),
            },
            color: None,
            locked: false,
            extras_json: None,
        };
        assert_eq!(decode(&encode(&n)).unwrap(), n);
    }

    #[test]
    fn obsidian_canvas_round_trip() {
        // r##"..."## (double hash) because the JSON contains `"#`
        // inside `"#^abc123"` which would otherwise terminate the raw string.
        let src = r##"{
            "nodes": [
                {"id":"n1","x":0,"y":0,"width":200,"height":120,"type":"text","text":"hi"},
                {"id":"n2","x":300,"y":0,"width":200,"height":120,"type":"file","file":"Notes/Foo.md","subpath":"#^abc123"}
            ],
            "edges": [
                {"id":"e1","fromNode":"n1","fromSide":"right","toNode":"n2","toSide":"left","label":"see also"}
            ]
        }"##;
        let c = parse_obsidian_canvas(src).unwrap();
        assert_eq!(c.nodes.len(), 2);
        assert_eq!(c.nodes[0].kind, "text");
        assert_eq!(c.nodes[0].text.as_deref(), Some("hi"));
        assert_eq!(c.nodes[1].file.as_deref(), Some("Notes/Foo.md"));
        assert_eq!(c.edges.len(), 1);
        assert_eq!(c.edges[0].label.as_deref(), Some("see also"));
        // round-trip
        let re = serialize_obsidian_canvas(&c);
        let c2 = parse_obsidian_canvas(&re).unwrap();
        assert_eq!(c2.nodes.len(), 2);
        assert_eq!(c2.edges.len(), 1);
    }
}
