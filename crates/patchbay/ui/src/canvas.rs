//! The graph canvas: positioned node cards + an SVG cable layer.

use dioxus::prelude::*;
use patchbay_proto::{MediaKind, PortDirection};

use crate::layout::{self, CARD_W, COL_GAP, Filters, MARGIN, ROW_H, column_titles};
use crate::state::{
    self, ARMED_OUTPUTS, EXPANDED_GROUPS, GRAPH, HIDE_MONITORS, HIDE_UNCONNECTED, MEDIA_TAB, PAN,
    SEARCH, SELECTED_NODE, ZOOM,
};

fn kind_color(kind: MediaKind) -> &'static str {
    match kind {
        MediaKind::Audio => "#7cc4ff",
        MediaKind::Video => "#ffd479",
        MediaKind::Midi => "#b487ff",
        MediaKind::Other => "#9aa4b2",
    }
}

#[component]
pub fn GraphCanvas() -> Element {
    // Fetch application icons for nodes we haven't looked up yet
    // (re-runs whenever the graph changes; already-known names are
    // skipped inside).
    let icon_handle = state::use_patchbay();
    use_effect(move || {
        let _ = GRAPH.read().nodes.len();
        state::request_missing_icons(icon_handle.clone());
    });

    let handle = state::use_patchbay();
    let graph = GRAPH.read();
    let aliases = state::ALIASES.read();
    let search = SEARCH.read();
    let expanded = EXPANDED_GROUPS.read();
    let filters = Filters {
        search: &search,
        tab: *MEDIA_TAB.read(),
        hide_unconnected: *HIDE_UNCONNECTED.read(),
        aliases: &aliases,
        hide_monitors: *HIDE_MONITORS.read(),
    };
    let lay = layout::compute_layout(&graph, &filters, &expanded);

    // Ports rendered as condensed stereo pairs draw thin cables.
    let pair_ports: std::collections::HashSet<u32> = lay
        .cards
        .iter()
        .flat_map(|c| c.rows.iter())
        .filter(|r| r.pair)
        .flat_map(|r| r.ports.iter().copied())
        .collect();

    // Cables: only when both anchors are visible. Collapsed-group
    // fan-ins collapse to one drawn path per (from-anchor, to-anchor)
    // pair — the path remembers every link id it stands for, so
    // clicking it disconnects them all.
    struct Cable {
        ids: Vec<u32>,
        d: String,
        color: String,
        active: bool,
        thin: bool,
    }
    let mut by_path: std::collections::HashMap<(u64, u64), usize> =
        std::collections::HashMap::new();
    let mut cables: Vec<Cable> = Vec::new();
    for l in &graph.links {
        let (Some(&(x1, y1)), Some(&(x2, y2))) =
            (lay.anchors.get(&l.output_port), lay.anchors.get(&l.input_port))
        else {
            continue;
        };
        let key = (
            (x1 as u64) << 32 | (y1 as u64),
            (x2 as u64) << 32 | (y2 as u64),
        );
        if let Some(&i) = by_path.get(&key) {
            cables[i].ids.push(l.id);
            cables[i].active |= l.active;
            continue;
        }
        let (kind, port_name) = graph
            .ports
            .iter()
            .find(|p| p.id == l.output_port)
            .map(|p| (p.media_kind, p.name.as_str()))
            .unwrap_or((MediaKind::Other, ""));
        let node_name = graph
            .nodes
            .iter()
            .find(|n| n.id == l.output_node)
            .map(|n| n.name.as_str())
            .unwrap_or("");
        let color = state::port_color(node_name, port_name, kind_color(kind));
        let dx = ((x2 - x1) * 0.5).max(40.0);
        by_path.insert(key, cables.len());
        cables.push(Cable {
            ids: vec![l.id],
            d: format!("M {x1} {y1} C {} {y1}, {} {y2}, {x2} {y2}", x1 + dx, x2 - dx),
            color,
            active: l.active,
            thin: pair_ports.contains(&l.output_port) || pair_ports.contains(&l.input_port),
        });
    }

    let world_w = lay.width;
    let world_h = lay.height.max(400.0);

    let zoom = *ZOOM.read();
    let (pan_x, pan_y) = *PAN.read();
    // Middle-drag pan state: last pointer position in client coords.
    let mut drag_last = use_signal(|| None::<(f64, f64)>);

    rsx! {
        div { class: "canvas-scroll",
            // Wheel = zoom toward the cursor; middle-drag = pan.
            onwheel: move |e: Event<WheelData>| {
                e.prevent_default();
                let dy = match e.delta() {
                    dioxus::html::geometry::WheelDelta::Pixels(v) => v.y,
                    dioxus::html::geometry::WheelDelta::Lines(v) => v.y * 40.0,
                    dioxus::html::geometry::WheelDelta::Pages(v) => v.y * 400.0,
                };
                let old = *ZOOM.peek();
                let new = (old * (1.0 - dy * 0.0015)).clamp(0.15, 3.0);
                // Keep the point under the cursor fixed while zooming.
                let cur = e.element_coordinates();
                let (px, py) = *PAN.peek();
                let scale = new / old;
                *PAN.write() = (
                    cur.x - (cur.x - px) * scale,
                    cur.y - (cur.y - py) * scale,
                );
                *ZOOM.write() = new;
            },
            onmousedown: move |e: Event<MouseData>| {
                if e.trigger_button() == Some(dioxus::html::input_data::MouseButton::Auxiliary) {
                    e.prevent_default();
                    let c = e.client_coordinates();
                    drag_last.set(Some((c.x, c.y)));
                }
            },
            onmousemove: move |e: Event<MouseData>| {
                let last = *drag_last.peek();
                if let Some((lx, ly)) = last {
                    let c = e.client_coordinates();
                    let (px, py) = *PAN.peek();
                    *PAN.write() = (px + (c.x - lx), py + (c.y - ly));
                    drag_last.set(Some((c.x, c.y)));
                }
            },
            onmouseup: move |_| drag_last.set(None),
            onmouseleave: move |_| drag_last.set(None),
            div { class: "canvas-tools",
                button { class: "chip", title: "zoom out",
                    onclick: move |_| { let z = *ZOOM.peek(); *ZOOM.write() = (z / 1.25).max(0.15); },
                    "−"
                }
                span { class: "zoom-pct", "{(zoom * 100.0) as i32}%" }
                button { class: "chip", title: "zoom in",
                    onclick: move |_| { let z = *ZOOM.peek(); *ZOOM.write() = (z * 1.25).min(3.0); },
                    "+"
                }
                button { class: "chip", title: "reset view",
                    onclick: move |_| { *ZOOM.write() = 1.0; *PAN.write() = (0.0, 0.0); },
                    "reset"
                }
            }
            div {
                class: "canvas-world",
                style: "width:{world_w}px;height:{world_h}px;\
                        transform: translate({pan_x}px, {pan_y}px) scale({zoom});\
                        transform-origin: 0 0;",
                for (i, title) in column_titles(*MEDIA_TAB.read()).iter().enumerate() {
                    div {
                        key: "{title}",
                        class: "col-header",
                        style: "left:{MARGIN + i as f64 * (CARD_W + COL_GAP)}px;width:{CARD_W}px;",
                        "{title}"
                    }
                }
                svg {
                    class: "cable-layer",
                    width: "{world_w}",
                    height: "{world_h}",
                    view_box: "0 0 {world_w} {world_h}",
                    for cable in cables {
                        {
                            let handle = handle.clone();
                            let ids = cable.ids.clone();
                            let n = ids.len();
                            rsx! {
                                path {
                                    key: "{cable.ids[0]}",
                                    class: "cable",
                                    d: "{cable.d}",
                                    fill: "none",
                                    stroke: "{cable.color}",
                                    stroke_width: if cable.thin { "1.2" } else { "2" },
                                    opacity: if cable.active { "0.95" } else { "0.45" },
                                    pointer_events: "stroke",
                                    onclick: move |e: Event<MouseData>| {
                                        e.stop_propagation();
                                        let handle = handle.clone();
                                        let ids = ids.clone();
                                        spawn(async move {
                                            for id in ids {
                                                if let Err(e) = handle.0.destroy_link(id).await {
                                                    tracing::warn!("cable disconnect failed: {e:?}");
                                                }
                                            }
                                        });
                                    },
                                    title { "{n} link(s) — click to disconnect" }
                                }
                            }
                        }
                    }
                }
                for card in &lay.cards {
                    NodeCard {
                        key: "{card.key}",
                        node_id: card.node.id,
                        node_name: card.node.name.clone(),
                        node_label: state::node_label(&card.node.name, &card.node.label),
                        media_class: card.node.media_class.clone(),
                        accent: state::node_color(
                            &card.node.name,
                            kind_color(card.node.media_kind),
                        ),
                        icon: state::ICONS
                            .read()
                            .get(&card.node.icon_name)
                            .cloned()
                            .unwrap_or_default(),
                        x: card.x,
                        y: card.y,
                        h: card.h,
                        rows: card
                            .rows
                            .iter()
                            .map(|r| RowProps {
                                aliased: aliases
                                    .contains_key(&format!("{}:{}", card.node.name, r.label)),
                                label: if r.pair {
                                    r.label.clone()
                                } else {
                                    state::port_label(&card.node.name, &r.label)
                                },
                                raw_name: r.label.clone(),
                                monitor: r.monitor,
                                direction: r.direction,
                                dot: if r.pair {
                                    state::node_color(&card.node.name, kind_color(r.kind))
                                } else {
                                    state::port_color(
                                        &card.node.name,
                                        &r.label,
                                        kind_color(r.kind),
                                    )
                                },
                                pair: r.pair,
                                ports: r.ports.clone(),
                                group_key: r.group_key.clone(),
                                expanded: r
                                    .group_key
                                    .as_ref()
                                    .map(|k| expanded.get(k).copied().unwrap_or(false))
                                    .unwrap_or(false),
                            })
                            .collect::<Vec<_>>(),
                    }
                }
            }
        }
    }
}

#[derive(Clone, PartialEq)]
struct RowProps {
    label: String,
    raw_name: String,
    aliased: bool,
    monitor: bool,
    direction: PortDirection,
    /// Resolved dot/cable color for this row.
    dot: String,
    /// Condensed stereo pair (ports = [L, R]).
    pair: bool,
    ports: Vec<u32>,
    group_key: Option<String>,
    expanded: bool,
}

#[component]
fn NodeCard(
    node_id: u32,
    node_name: String,
    node_label: String,
    media_class: String,
    accent: String,
    icon: String,
    x: f64,
    y: f64,
    h: f64,
    rows: Vec<RowProps>,
) -> Element {
    let armed = ARMED_OUTPUTS.read().clone();
    let inspected = *SELECTED_NODE.read() == Some(node_id);
    let handle = state::use_patchbay();

    rsx! {
        div {
            class: if inspected { "node-card inspected" } else { "node-card" },
            style: "left:{x}px;top:{y}px;width:{CARD_W}px;height:{h}px;",
            div {
                class: "node-header",
                style: "border-left: 3px solid {accent};",
                onclick: move |_| {
                    let cur = *SELECTED_NODE.peek();
                    *SELECTED_NODE.write() = if cur == Some(node_id) { None } else { Some(node_id) };
                },
                if !icon.is_empty() {
                    img { class: "node-icon", src: "{icon}" }
                }
                div { class: "node-titles",
                    span { class: "node-title", "{node_label}" }
                    span { class: "node-class", "{media_class}" }
                }
            }
            for (i, row) in rows.iter().enumerate() {
                {
                    let row = row.clone();
                    let handle = handle.clone();
                    let is_group = row.group_key.is_some();
                    let is_armed = row.direction == PortDirection::Output
                        && !row.ports.is_empty()
                        && !is_group
                        && row.ports == armed;
                    let connectable = row.direction == PortDirection::Input
                        && !armed.is_empty()
                        && !is_group
                        && !row.ports.is_empty();
                    let side = if row.direction == PortDirection::Input { "row-in" } else { "row-out" };
                    let classes = format!(
                        "port-row {side}{}{}{}{}{}{}",
                        if is_group { " group" } else { "" },
                        if is_armed { " armed" } else { "" },
                        if connectable { " connectable" } else { "" },
                        if row.aliased { " aliased" } else { "" },
                        if row.monitor { " mon" } else { "" },
                        if row.pair { " pair" } else { "" },
                    );
                    let tooltip = if is_group {
                        format!(
                            "{} ports — click to {}",
                            row.ports.len().max(1),
                            if row.expanded { "collapse" } else { "expand" }
                        )
                    } else if row.pair {
                        format!("stereo pair — click to {} both channels",
                            if row.direction == PortDirection::Output { "arm" } else { "connect" })
                    } else if row.monitor {
                        format!("{} — monitor tap (a copy of what this sink plays)", row.raw_name)
                    } else if row.aliased {
                        row.raw_name.clone()
                    } else {
                        String::new()
                    };
                    let dot = row.dot.clone();
                    rsx! {
                        div {
                            key: "{i}",
                            class: "{classes}",
                            style: "height:{ROW_H}px;",
                            title: "{tooltip}",
                            onclick: move |_| {
                                if let Some(key) = &row.group_key {
                                    let now = !row.expanded;
                                    EXPANDED_GROUPS.write().insert(key.clone(), now);
                                    return;
                                }
                                if row.ports.is_empty() {
                                    return;
                                }
                                match row.direction {
                                    PortDirection::Output => {
                                        let cur = ARMED_OUTPUTS.peek().clone();
                                        *ARMED_OUTPUTS.write() = if cur == row.ports {
                                            Vec::new()
                                        } else {
                                            row.ports.clone()
                                        };
                                    }
                                    PortDirection::Input => {
                                        state::connect_armed(handle.clone(), &row.ports);
                                    }
                                }
                            },
                            if row.direction == PortDirection::Input {
                                span {
                                    class: if row.pair { "port-dot pair-dot" } else { "port-dot" },
                                    style: "background:{dot};",
                                }
                            }
                            span { class: "port-name",
                                if is_group {
                                    if row.expanded { "▾ {row.label}" } else { "▸ {row.label}" }
                                } else {
                                    "{row.label}"
                                }
                            }
                            if row.direction == PortDirection::Output {
                                span {
                                    class: if row.pair { "port-dot pair-dot" } else { "port-dot" },
                                    style: "background:{dot};",
                                }
                            }
                        }
                    }
                }
            }
        }
    }
}
