//! `Whiteboard` — Logseq-style 2D canvas component.
//!
//! A whiteboard Page (`Page.ext == "canvas"`) carries top-level Blocks
//! whose `canvas_node_json` is `Some(CanvasNode)`. This component
//! decodes those, paints them inside a pan/zoom transform, and emits
//! patch events back to the parent — no Loro / repo dependency.
//!
//! ## v1 scope
//! - Pan (drag-empty / shift-drag), zoom (ctrl+wheel about cursor)
//! - Select (click / shift-click toggle / marquee)
//! - Move single + multi-selection (drag a selected node)
//! - Insert primitive shapes (Rect/Ellipse/Diamond/StickyNote/Text)
//! - Inline text edit on Sticky/Text via `<textarea>` overlay
//! - Render connectors (orthogonal polyline, arrowheads, dash/dotted)
//! - Render BlockRef / PageRef / Group / Asset / Pen widgets
//! - Keyboard: arrow nudge, Delete, Esc, mode shortcuts, fit (F)
//!
//! ## Not in v1
//! - Freehand pen drawing tool (only renders existing strokes)
//! - Rotation handles
//! - Resize handles (placeholder — `Resize` drag state exists but no
//!   handle hit-rects are drawn yet)
//! - Group drag translating children (children just stay put visually)
//! - Connector drag-create end-to-end commit (the drag state machine
//!   is wired but `on_node_create` payload assembly is `FUTURE:`)
//! - Color/style mutation through the toolbar (button exists, the
//!   `on_node_patch` wiring for color change is `FUTURE:`)

use std::collections::{HashMap, HashSet};

use dioxus::prelude::*;
use fts_ui::lucide_dioxus::{
    Circle as IconCircle, Diamond as IconDiamond, Hand, Maximize, MousePointer2, PenLine, Spline,
    Square as IconSquare, StickyNote as IconSticky, Type as IconType, ZoomIn, ZoomOut,
};
use fts_ui::prelude::{Button, ButtonSize, ButtonVariant};
use knowledge_proto::canvas::{CanvasNode, CanvasShape, decode, encode};
use knowledge_proto::{Block, Page};
use uuid::Uuid;

use crate::canvas::coords::{
    self, Viewport, fit_to_bounds, screen_to_world, world_to_screen, zoom_at,
};
use crate::canvas::hit::{self, PlacedNode, anchor_point, connector_path, pick, pick_rect};
use crate::canvas::selection::Selection;
use crate::canvas::widgets::{
    asset::Asset,
    block_ref::BlockRefCard,
    connector::Connector,
    group::Group,
    page_ref::PageRefCard,
    shape_pen::ShapePen,
    shape_rect::{ShapeKind, ShapeRect},
};
use crate::editor::BlockUpdate;

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum InsertShapeKind {
    Rect,
    Ellipse,
    Diamond,
    StickyNote,
    Text,
}

#[derive(Clone, Debug, PartialEq)]
pub enum CanvasMode {
    Pan,
    Select,
    InsertShape(InsertShapeKind),
    DrawConnector { from: Option<Uuid> },
    Pen,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum ResizeHandle {
    N,
    S,
    E,
    W,
    NE,
    NW,
    SE,
    SW,
}

#[derive(Clone, Debug)]
enum DragState {
    Node {
        ids: HashSet<Uuid>,
        start_world: (f64, f64),
        original_positions: HashMap<Uuid, (f64, f64)>,
    },
    Viewport {
        start_screen: (f64, f64),
        original_pan: (f64, f64),
    },
    Marquee {
        start_world: (f64, f64),
    },
    Resize {
        id: Uuid,
        handle: ResizeHandle,
        original: (f64, f64, f64, f64),
    },
    ConnectorEnd {
        from_id: Uuid,
        start_world: (f64, f64),
        current_screen: (f64, f64),
    },
}

#[derive(Props, Clone, PartialEq)]
pub struct WhiteboardProps {
    pub page_id: Uuid,
    pub blocks: Vec<Block>,
    pub blocks_by_id: HashMap<Uuid, Block>,
    pub pages_by_id: HashMap<Uuid, Page>,
    #[props(default = true)]
    pub editable: bool,
    pub on_node_move: EventHandler<(Uuid, f64, f64)>,
    pub on_node_resize: EventHandler<(Uuid, f64, f64, f64, f64)>,
    pub on_node_create: EventHandler<Block>,
    pub on_node_delete: EventHandler<Uuid>,
    pub on_node_patch: EventHandler<(Uuid, BlockUpdate)>,
    pub on_open_block: EventHandler<Uuid>,
    pub on_open_page: EventHandler<Uuid>,
}

#[component]
pub fn Whiteboard(props: WhiteboardProps) -> Element {
    let mut viewport = use_signal(Viewport::default);
    let mut selection = use_signal(Selection::new);
    let mut mode = use_signal(|| CanvasMode::Select);
    let mut drag = use_signal::<Option<DragState>>(|| None);
    let mut editing_text = use_signal::<Option<Uuid>>(|| None);
    let mut marquee = use_signal::<Option<(f64, f64, f64, f64)>>(|| None);

    // Decode CanvasNodes once per blocks change, sort by z.
    let blocks = props.blocks.clone();
    let page_id = props.page_id;
    let placed = use_memo(use_reactive!(|(blocks, page_id)| {
        let _ = page_id;
        let mut v: Vec<PlacedNode> = blocks
            .iter()
            .filter_map(|b| {
                let json = b.canvas_node_json.as_deref()?;
                let n = decode(json)?;
                Some(PlacedNode {
                    block_id: b.id,
                    node: n,
                })
            })
            .collect();
        v.sort_by_key(|p| p.node.z);
        v
    }));

    let v = viewport();
    let zoom = v.zoom;
    let pan_x = v.pan_x;
    let pan_y = v.pan_y;
    let transform_style = format!(
        "transform: translate3d({pan_x}px, {pan_y}px, 0) scale({zoom}); transform-origin: 0 0;"
    );

    // ── Toolbar handlers ────────────────────────────────────────────
    let zoom_in = move |_| {
        viewport.set(zoom_at(viewport(), (400.0, 300.0), 1.25));
    };
    let zoom_out = move |_| {
        viewport.set(zoom_at(viewport(), (400.0, 300.0), 0.8));
    };
    let fit = move |_| {
        let nodes = placed.read().clone();
        if nodes.is_empty() {
            viewport.set(Viewport::default());
            return;
        }
        let mut min_x = f64::INFINITY;
        let mut min_y = f64::INFINITY;
        let mut max_x = f64::NEG_INFINITY;
        let mut max_y = f64::NEG_INFINITY;
        for n in nodes.iter() {
            min_x = min_x.min(n.node.x);
            min_y = min_y.min(n.node.y);
            max_x = max_x.max(n.node.x + n.node.w);
            max_y = max_y.max(n.node.y + n.node.h);
        }
        // Use a reasonable default container size (real size needs
        // a measure ref; v1 picks a sane viewport).
        let bounds = (min_x, min_y, max_x - min_x, max_y - min_y);
        viewport.set(fit_to_bounds(viewport(), (1200.0, 800.0), bounds, 48.0));
    };

    let mode_btn =
        |target: CanvasMode, label: &'static str| -> (CanvasMode, &'static str) { (target, label) };
    let _ = mode_btn; // silence unused (kept for future toolbar generator)

    // ── Pointer + key event handlers ───────────────────────────────
    // We encode hit-tests against the latest `placed` and viewport
    // values inside the closures via signal reads.

    let on_pointer_down = move |evt: PointerEvent| {
        if !props.editable {
            return;
        }
        let coords_screen = (evt.client_coordinates().x, evt.client_coordinates().y);
        let v = viewport();
        let world = screen_to_world(&v, coords_screen);
        let nodes = placed.read().clone();
        let hit_node = pick(&nodes, world).cloned();
        let m = mode();

        match (m, hit_node) {
            (CanvasMode::Pan, _) => {
                drag.set(Some(DragState::Viewport {
                    start_screen: coords_screen,
                    original_pan: (v.pan_x, v.pan_y),
                }));
            }
            (CanvasMode::InsertShape(kind), _) => {
                // Drop a new node with default dimensions at world point.
                let (w, h) = default_dims(kind);
                let shape = match kind {
                    InsertShapeKind::Rect => CanvasShape::Rect,
                    InsertShapeKind::Ellipse => CanvasShape::Ellipse,
                    InsertShapeKind::Diamond => CanvasShape::Diamond,
                    InsertShapeKind::StickyNote => CanvasShape::StickyNote,
                    InsertShapeKind::Text => CanvasShape::Text,
                };
                let node = CanvasNode {
                    x: world.0,
                    y: world.1,
                    w,
                    h,
                    z: nodes.last().map(|n| n.node.z + 1).unwrap_or(0),
                    rotation: 0.0,
                    shape,
                    color: None,
                    locked: false,
                    extras_json: None,
                };
                let new_block = new_canvas_block(props.page_id, &node);
                props.on_node_create.call(new_block);
                mode.set(CanvasMode::Select);
            }
            (CanvasMode::DrawConnector { .. }, Some(n)) => {
                let from_world = anchor_point(&n.node, knowledge_proto::canvas::Anchor::Auto);
                drag.set(Some(DragState::ConnectorEnd {
                    from_id: n.block_id,
                    start_world: from_world,
                    current_screen: coords_screen,
                }));
                mode.set(CanvasMode::DrawConnector {
                    from: Some(n.block_id),
                });
            }
            (CanvasMode::Select, Some(n)) => {
                let id = n.block_id;
                let shift = evt.modifiers().shift();
                let mut sel = selection();
                if shift {
                    sel.toggle(id);
                } else if !sel.contains(id) {
                    sel.replace_with(std::iter::once(id));
                }
                selection.set(sel.clone());
                // Snapshot positions for all selected nodes.
                let mut originals = HashMap::new();
                for p in nodes.iter() {
                    if sel.contains(p.block_id) {
                        originals.insert(p.block_id, (p.node.x, p.node.y));
                    }
                }
                drag.set(Some(DragState::Node {
                    ids: sel.block_ids.clone(),
                    start_world: world,
                    original_positions: originals,
                }));
            }
            (CanvasMode::Select, None) => {
                if !evt.modifiers().shift() {
                    let mut sel = selection();
                    sel.clear();
                    selection.set(sel);
                }
                drag.set(Some(DragState::Marquee { start_world: world }));
                marquee.set(Some((world.0, world.1, 0.0, 0.0)));
            }
            _ => {}
        }
    };

    let on_pointer_move = move |evt: PointerEvent| {
        let coords_screen = (evt.client_coordinates().x, evt.client_coordinates().y);
        let v = viewport();
        let world = screen_to_world(&v, coords_screen);
        let Some(state) = drag() else {
            return;
        };
        match state {
            DragState::Viewport {
                start_screen,
                original_pan,
            } => {
                let dx = coords_screen.0 - start_screen.0;
                let dy = coords_screen.1 - start_screen.1;
                let mut new_v = v;
                new_v.pan_x = original_pan.0 + dx;
                new_v.pan_y = original_pan.1 + dy;
                viewport.set(new_v);
            }
            DragState::Node {
                start_world,
                ref original_positions,
                ..
            } => {
                let dx = world.0 - start_world.0;
                let dy = world.1 - start_world.1;
                // We don't mutate `blocks` directly — we just preview
                // by re-firing `on_node_move` on pointerup. For a
                // smooth preview the parent would need to accept a
                // "live" event; v1 commits on release only.
                let _ = (dx, dy, original_positions);
            }
            DragState::Marquee { start_world } => {
                let x = start_world.0.min(world.0);
                let y = start_world.1.min(world.1);
                let w = (world.0 - start_world.0).abs();
                let h = (world.1 - start_world.1).abs();
                marquee.set(Some((x, y, w, h)));
            }
            DragState::Resize { .. } => {
                // FUTURE: compute new w/h from handle direction.
            }
            DragState::ConnectorEnd {
                from_id,
                start_world,
                ..
            } => {
                drag.set(Some(DragState::ConnectorEnd {
                    from_id,
                    start_world,
                    current_screen: coords_screen,
                }));
            }
        }
    };

    let on_pointer_up = move |evt: PointerEvent| {
        let coords_screen = (evt.client_coordinates().x, evt.client_coordinates().y);
        let v = viewport();
        let world = screen_to_world(&v, coords_screen);
        let Some(state) = drag() else {
            return;
        };
        match state {
            DragState::Node {
                start_world,
                original_positions,
                ..
            } => {
                let dx = world.0 - start_world.0;
                let dy = world.1 - start_world.1;
                for (id, (ox, oy)) in original_positions {
                    props.on_node_move.call((id, ox + dx, oy + dy));
                }
            }
            DragState::Marquee { .. } => {
                if let Some(rect) = marquee() {
                    let nodes = placed.read().clone();
                    let hit_ids: HashSet<Uuid> =
                        pick_rect(&nodes, rect).iter().map(|n| n.block_id).collect();
                    let mut sel = selection();
                    if evt.modifiers().shift() {
                        for id in hit_ids {
                            sel.add(id);
                        }
                    } else {
                        sel.replace_with(hit_ids);
                    }
                    selection.set(sel);
                }
                marquee.set(None);
            }
            DragState::ConnectorEnd { .. } => {
                // FUTURE: hit-test under cursor; if a node, build the
                // CanvasNode Connector + emit on_node_create. v1 just
                // clears the drag and resets mode.
                mode.set(CanvasMode::Select);
            }
            _ => {}
        }
        drag.set(None);
    };

    let on_wheel = move |evt: WheelEvent| {
        let mods = evt.modifiers();
        let coords_screen = (evt.client_coordinates().x, evt.client_coordinates().y);
        let delta_y = evt.delta().strip_units().y;
        if mods.ctrl() || mods.meta() {
            let factor = if delta_y < 0.0 { 1.1 } else { 0.9 };
            viewport.set(zoom_at(viewport(), coords_screen, factor));
        } else {
            let mut v = viewport();
            let delta_x = evt.delta().strip_units().x;
            v.pan_x -= delta_x;
            v.pan_y -= delta_y;
            viewport.set(v);
        }
    };

    let on_key_down = move |evt: KeyboardEvent| {
        let key = evt.key();
        match key {
            Key::Backspace | Key::Delete => {
                let sel = selection();
                for id in sel.block_ids.iter() {
                    props.on_node_delete.call(*id);
                }
                let mut s = sel.clone();
                s.clear();
                selection.set(s);
            }
            Key::Escape => {
                let mut s = selection();
                s.clear();
                selection.set(s);
                mode.set(CanvasMode::Select);
                editing_text.set(None);
            }
            Key::ArrowLeft | Key::ArrowRight | Key::ArrowUp | Key::ArrowDown => {
                let step = if evt.modifiers().shift() { 1.0 } else { 8.0 };
                let (dx, dy) = match key {
                    Key::ArrowLeft => (-step, 0.0),
                    Key::ArrowRight => (step, 0.0),
                    Key::ArrowUp => (0.0, -step),
                    Key::ArrowDown => (0.0, step),
                    _ => unreachable!(),
                };
                let sel = selection();
                for n in placed.read().clone().iter() {
                    if sel.contains(n.block_id) {
                        props
                            .on_node_move
                            .call((n.block_id, n.node.x + dx, n.node.y + dy));
                    }
                }
            }
            Key::Character(ref s) => match s.as_str() {
                "v" | "V" => mode.set(CanvasMode::Select),
                "r" | "R" => mode.set(CanvasMode::InsertShape(InsertShapeKind::Rect)),
                "o" | "O" => mode.set(CanvasMode::InsertShape(InsertShapeKind::Ellipse)),
                "d" | "D" => mode.set(CanvasMode::InsertShape(InsertShapeKind::Diamond)),
                "s" | "S" => mode.set(CanvasMode::InsertShape(InsertShapeKind::StickyNote)),
                "t" | "T" => mode.set(CanvasMode::InsertShape(InsertShapeKind::Text)),
                "c" | "C" => mode.set(CanvasMode::DrawConnector { from: None }),
                "h" | "H" => mode.set(CanvasMode::Pan),
                "f" | "F" => {
                    let nodes = placed.read().clone();
                    if !nodes.is_empty() {
                        let mut min_x = f64::INFINITY;
                        let mut min_y = f64::INFINITY;
                        let mut max_x = f64::NEG_INFINITY;
                        let mut max_y = f64::NEG_INFINITY;
                        for n in nodes.iter() {
                            min_x = min_x.min(n.node.x);
                            min_y = min_y.min(n.node.y);
                            max_x = max_x.max(n.node.x + n.node.w);
                            max_y = max_y.max(n.node.y + n.node.h);
                        }
                        let bounds = (min_x, min_y, max_x - min_x, max_y - min_y);
                        viewport.set(fit_to_bounds(viewport(), (1200.0, 800.0), bounds, 48.0));
                    }
                }
                "0" => {
                    let mut v = viewport();
                    v.zoom = 1.0;
                    viewport.set(v);
                }
                "+" | "=" => {
                    viewport.set(zoom_at(viewport(), (600.0, 400.0), 1.25));
                }
                "-" | "_" => {
                    viewport.set(zoom_at(viewport(), (600.0, 400.0), 0.8));
                }
                _ => {}
            },
            _ => {}
        }
    };

    // Top-level blocks-by-page (for PageRefCard preview): synthesize
    // from `blocks_by_id` — top-level = no parent_block_id, grouped
    // by page_id.
    let mut top_blocks_by_page: HashMap<Uuid, Vec<Block>> = HashMap::new();
    for b in props.blocks_by_id.values() {
        if b.parent_block_id.is_none() && b.canvas_node_json.is_none() {
            top_blocks_by_page
                .entry(b.page_id)
                .or_default()
                .push(b.clone());
        }
    }

    let placed_list = placed.read().clone();
    let sel = selection();
    let editing = editing_text();

    // Compute world bounds for the SVG overlay.
    let (svg_x, svg_y, svg_w, svg_h) = if placed_list.is_empty() {
        (-1000.0, -1000.0, 4000.0, 4000.0)
    } else {
        let mut min_x = f64::INFINITY;
        let mut min_y = f64::INFINITY;
        let mut max_x = f64::NEG_INFINITY;
        let mut max_y = f64::NEG_INFINITY;
        for n in placed_list.iter() {
            min_x = min_x.min(n.node.x);
            min_y = min_y.min(n.node.y);
            max_x = max_x.max(n.node.x + n.node.w);
            max_y = max_y.max(n.node.y + n.node.h);
        }
        let pad = 1000.0;
        (
            min_x - pad,
            min_y - pad,
            (max_x - min_x) + 2.0 * pad,
            (max_y - min_y) + 2.0 * pad,
        )
    };
    let svg_view_box = format!("{svg_x} {svg_y} {svg_w} {svg_h}");
    let svg_outer_style = format!(
        "position: absolute; left: {svg_x}px; top: {svg_y}px; width: {svg_w}px; height: {svg_h}px; pointer-events: none;"
    );

    let _ = (world_to_screen, coords::MIN_ZOOM); // used elsewhere
    let _ = hit::connector_tolerance_world;

    rsx! {
        div {
            class: "relative w-full h-full overflow-hidden bg-background touch-none select-none outline-none",
            tabindex: 0,
            onpointerdown: on_pointer_down,
            onpointermove: on_pointer_move,
            onpointerup: on_pointer_up,
            onwheel: on_wheel,
            onkeydown: on_key_down,

            // Floating toolbar.
            div { class: "absolute top-2 left-1/2 -translate-x-1/2 z-10 flex items-center gap-1 px-1.5 py-1 rounded-md border border-border bg-card text-card-foreground shadow-sm",
                Button { variant: ButtonVariant::Ghost, size: ButtonSize::Small, on_click: Callback::new(zoom_in),
                    ZoomIn { size: 14 }
                }
                Button { variant: ButtonVariant::Ghost, size: ButtonSize::Small, on_click: Callback::new(zoom_out),
                    ZoomOut { size: 14 }
                }
                Button { variant: ButtonVariant::Ghost, size: ButtonSize::Small, on_click: Callback::new(fit),
                    Maximize { size: 14 }
                }
                div { class: "w-px h-5 bg-border mx-1" }
                ModeBtn { current: mode(), target: CanvasMode::Select, on_pick: Callback::new(move |m: CanvasMode| mode.set(m)),
                    MousePointer2 { size: 14 }
                }
                ModeBtn { current: mode(), target: CanvasMode::Pan, on_pick: Callback::new(move |m: CanvasMode| mode.set(m)),
                    Hand { size: 14 }
                }
                ModeBtn { current: mode(), target: CanvasMode::InsertShape(InsertShapeKind::Rect), on_pick: Callback::new(move |m: CanvasMode| mode.set(m)),
                    IconSquare { size: 14 }
                }
                ModeBtn { current: mode(), target: CanvasMode::InsertShape(InsertShapeKind::Ellipse), on_pick: Callback::new(move |m: CanvasMode| mode.set(m)),
                    IconCircle { size: 14 }
                }
                ModeBtn { current: mode(), target: CanvasMode::InsertShape(InsertShapeKind::Diamond), on_pick: Callback::new(move |m: CanvasMode| mode.set(m)),
                    IconDiamond { size: 14 }
                }
                ModeBtn { current: mode(), target: CanvasMode::InsertShape(InsertShapeKind::StickyNote), on_pick: Callback::new(move |m: CanvasMode| mode.set(m)),
                    IconSticky { size: 14 }
                }
                ModeBtn { current: mode(), target: CanvasMode::InsertShape(InsertShapeKind::Text), on_pick: Callback::new(move |m: CanvasMode| mode.set(m)),
                    IconType { size: 14 }
                }
                ModeBtn { current: mode(), target: CanvasMode::DrawConnector { from: None }, on_pick: Callback::new(move |m: CanvasMode| mode.set(m)),
                    Spline { size: 14 }
                }
                ModeBtn { current: mode(), target: CanvasMode::Pen, on_pick: Callback::new(move |m: CanvasMode| mode.set(m)),
                    PenLine { size: 14 }
                }
            }

            // World-transformed inner div.
            div {
                class: "absolute top-0 left-0",
                style: "{transform_style}",

                // SVG overlay for connectors + marquee.
                svg {
                    style: "{svg_outer_style}",
                    view_box: "{svg_view_box}",
                    preserve_aspect_ratio: "xMinYMin meet",
                    for n in placed_list.iter() {
                        if let CanvasShape::Connector { ref from, ref to, ref style, ref label } = n.node.shape {
                            {
                                let from_pn = placed_list.iter().find(|p| p.block_id == from.block_id).cloned();
                                let to_pn = placed_list.iter().find(|p| p.block_id == to.block_id).cloned();
                                match (from_pn, to_pn) {
                                    (Some(fp), Some(tp)) => {
                                        let pts = connector_path(&fp, from.anchor, &tp, to.anchor);
                                        let key = n.block_id.simple().to_string();
                                        rsx! {
                                            Connector {
                                                key: "{key}",
                                                key_id: key.clone(),
                                                points: pts,
                                                style: style.clone(),
                                                label: label.clone(),
                                                selected: sel.contains(n.block_id),
                                            }
                                        }
                                    }
                                    _ => rsx! { g {} },
                                }
                            }
                        }
                    }

                    // Marquee rectangle.
                    if let Some((mx, my, mw, mh)) = marquee() {
                        rect {
                            x: "{mx}",
                            y: "{my}",
                            width: "{mw}",
                            height: "{mh}",
                            class: "fill-primary/10 stroke-primary",
                            stroke_width: "1",
                            stroke_dasharray: "4 2",
                        }
                    }
                }

                // Non-connector widgets — divs positioned absolutely
                // in world coords.
                for n in placed_list.iter() {
                    {
                        let selected = sel.contains(n.block_id);
                        let key = n.block_id.simple().to_string();
                        let block = props.blocks_by_id.get(&n.block_id).cloned();
                        let content = block.as_ref().map(|b| b.content.clone()).unwrap_or_default();
                        let is_editing = editing == Some(n.block_id);
                        match n.node.shape.clone() {
                            CanvasShape::Rect | CanvasShape::Ellipse | CanvasShape::Diamond
                            | CanvasShape::StickyNote | CanvasShape::Text => {
                                let kind = ShapeKind::from_canvas_shape(&n.node.shape).unwrap_or(ShapeKind::Rect);
                                let id = n.block_id;
                                let on_text_commit = EventHandler::new(move |val: String| {
                                    props.on_node_patch.call((id, BlockUpdate::SetContent(val)));
                                    editing_text.set(None);
                                });
                                rsx! {
                                    div {
                                        key: "{key}",
                                        ondoubleclick: move |_| {
                                            if matches!(kind, ShapeKind::StickyNote | ShapeKind::Text) {
                                                editing_text.set(Some(id));
                                            }
                                        },
                                        ShapeRect {
                                            block_id: id,
                                            node: n.node.clone(),
                                            content,
                                            kind,
                                            selected,
                                            editing: is_editing,
                                            on_text_commit,
                                        }
                                    }
                                }
                            }
                            CanvasShape::BlockRef { target_block_id, view } => {
                                rsx! {
                                    BlockRefCard {
                                        key: "{key}",
                                        block_id: n.block_id,
                                        node: n.node.clone(),
                                        target_block_id,
                                        view,
                                        blocks_by_id: props.blocks_by_id.clone(),
                                        selected,
                                        on_open_block: props.on_open_block,
                                    }
                                }
                            }
                            CanvasShape::PageRef { target_page_id } => {
                                rsx! {
                                    PageRefCard {
                                        key: "{key}",
                                        block_id: n.block_id,
                                        node: n.node.clone(),
                                        target_page_id,
                                        pages_by_id: props.pages_by_id.clone(),
                                        top_blocks_by_page: top_blocks_by_page.clone(),
                                        selected,
                                        on_open_page: props.on_open_page,
                                    }
                                }
                            }
                            CanvasShape::Pen => rsx! {
                                ShapePen { key: "{key}", block_id: n.block_id, node: n.node.clone(), selected }
                            },
                            CanvasShape::Group => rsx! {
                                Group { key: "{key}", block_id: n.block_id, node: n.node.clone(), label: content, selected }
                            },
                            CanvasShape::Asset { asset_id } => rsx! {
                                Asset {
                                    key: "{key}",
                                    block_id: n.block_id,
                                    node: n.node.clone(),
                                    asset_id,
                                    asset_url_by_id: HashMap::new(),
                                    selected,
                                }
                            },
                            CanvasShape::Connector { .. } => rsx! { div { key: "{key}" } },
                        }
                    }
                }
            }
        }
    }
}

#[derive(Props, Clone, PartialEq)]
struct ModeBtnProps {
    current: CanvasMode,
    target: CanvasMode,
    on_pick: Callback<CanvasMode>,
    children: Element,
}

#[component]
fn ModeBtn(props: ModeBtnProps) -> Element {
    let active = props.current == props.target;
    let variant = if active {
        ButtonVariant::Primary
    } else {
        ButtonVariant::Ghost
    };
    let target = props.target.clone();
    let cb = props.on_pick;
    rsx! {
        Button {
            variant,
            size: ButtonSize::Small,
            on_click: Callback::new(move |_| cb.call(target.clone())),
            {props.children}
        }
    }
}

fn default_dims(kind: InsertShapeKind) -> (f64, f64) {
    match kind {
        InsertShapeKind::Rect => (160.0, 100.0),
        InsertShapeKind::Ellipse => (140.0, 100.0),
        InsertShapeKind::Diamond => (140.0, 120.0),
        InsertShapeKind::StickyNote => (180.0, 140.0),
        InsertShapeKind::Text => (200.0, 40.0),
    }
}

fn new_canvas_block(page_id: Uuid, node: &CanvasNode) -> Block {
    let now = chrono::Utc::now();
    Block {
        id: Uuid::new_v4(),
        vault_id: Uuid::nil(),
        page_id,
        parent_block_id: None,
        sort_key: "a0".into(),
        kind: "canvas".into(),
        content: String::new(),
        heading_level: None,
        list_ordered: false,
        list_task: None,
        code_lang: None,
        callout_kind: None,
        callout_foldable: false,
        properties_json: "{}".into(),
        obsidian_block_id: None,
        collapsed: false,
        refs_json: "[]".into(),
        canvas_node_json: Some(encode(node)),
        created_at: now,
        updated_at: now,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use knowledge_proto::canvas::BlockRefView;

    #[test]
    fn new_canvas_block_carries_encoded_node() {
        let pid = Uuid::new_v4();
        let n = CanvasNode {
            x: 1.0,
            y: 2.0,
            w: 10.0,
            h: 20.0,
            z: 3,
            rotation: 0.0,
            shape: CanvasShape::Rect,
            color: None,
            locked: false,
            extras_json: None,
        };
        let b = new_canvas_block(pid, &n);
        assert_eq!(b.page_id, pid);
        assert_eq!(b.kind, "canvas");
        let decoded = decode(b.canvas_node_json.as_deref().unwrap()).unwrap();
        assert_eq!(decoded, n);
    }

    #[test]
    fn default_dims_are_positive() {
        for k in [
            InsertShapeKind::Rect,
            InsertShapeKind::Ellipse,
            InsertShapeKind::Diamond,
            InsertShapeKind::StickyNote,
            InsertShapeKind::Text,
        ] {
            let (w, h) = default_dims(k);
            assert!(w > 0.0 && h > 0.0);
        }
    }

    #[test]
    fn block_ref_view_default_is_full() {
        assert_eq!(BlockRefView::default(), BlockRefView::Full);
    }
}
