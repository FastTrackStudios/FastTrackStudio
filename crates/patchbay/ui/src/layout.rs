//! Pure layout math: filtered graph → positioned cards + port anchors.
//!
//! Nodes land in three columns (outputs-only | duplex | inputs-only),
//! stacked vertically. Consecutive same-prefix numbered ports
//! (`playback_1..128`) collapse into one group row unless expanded —
//! that's how a 128-channel Inferno node stays one small card instead
//! of a 3000-pixel wall.

use std::collections::HashMap;

use patchbay_proto::{GraphSnapshot, MediaKind, PortDirection, PwNode, PwPort};

pub const CARD_W: f64 = 280.0;
pub const ROW_H: f64 = 22.0;
pub const HEADER_H: f64 = 34.0;
pub const CARD_GAP: f64 = 18.0;
pub const COL_GAP: f64 = 200.0;
pub const MARGIN: f64 = 24.0;
/// Vertical space reserved for the column titles.
pub const COL_HEADER_H: f64 = 34.0;
/// Runs shorter than this never group.
pub const GROUP_MIN: usize = 5;

/// Column semantics: 0 = Inputs (capture devices/sources), 1 =
/// Applications (streams + app clients), 2 = Outputs (sinks). Driven
/// by `media.class`, not port shape — an `Audio/Sink` belongs on the
/// right even though its monitor ports also produce audio.
pub fn column_of(media_class: &str) -> usize {
    if media_class.contains("/Sink") {
        2
    } else if media_class.contains("/Source") && !media_class.starts_with("Stream") {
        0
    } else {
        1
    }
}

/// Column titles per media tab.
pub fn column_titles(tab: MediaKind) -> [&'static str; 3] {
    match tab {
        MediaKind::Midi => ["MIDI Inputs", "Applications", "MIDI Outputs"],
        MediaKind::Video => ["Cameras / Sources", "Applications", "Outputs"],
        _ => ["Inputs", "Applications", "Outputs"],
    }
}

/// Monitor ports (a sink's loop-out taps) get tagged + dimmed so
/// they're never mistaken for the sink's real playback inputs.
pub fn is_monitor(port_name: &str) -> bool {
    port_name.starts_with("monitor_") || port_name == "monitor"
}

/// One rendered row inside a card: a single port, or a collapsed group.
pub struct PortRow {
    pub label: String,
    pub direction: PortDirection,
    pub kind: MediaKind,
    /// All port ids this row anchors (1 for a plain port row).
    pub ports: Vec<u32>,
    /// Set when this row is a collapsible group (expansion key).
    pub group_key: Option<String>,
    /// A sink's monitor tap (rendered dimmed + tagged).
    pub monitor: bool,
    /// y offset of the row center, relative to the card top.
    pub y: f64,
}

pub struct CardLayout {
    pub node: PwNode,
    /// Unique render key — a MIDI device split by direction yields two
    /// cards from one node (`"<id>-out"` / `"<id>-in"`).
    pub key: String,
    pub x: f64,
    pub y: f64,
    pub h: f64,
    pub rows: Vec<PortRow>,
}

pub struct GraphLayout {
    pub cards: Vec<CardLayout>,
    /// port id → (x, y) cable anchor in world coordinates.
    pub anchors: HashMap<u32, (f64, f64)>,
    pub width: f64,
    pub height: f64,
}

/// Split `playback_97` → (`playback_`, 97). Ports without a numeric
/// suffix never group.
fn split_numeric(name: &str) -> Option<(&str, u64)> {
    let digits = name.chars().rev().take_while(|c| c.is_ascii_digit()).count();
    if digits == 0 || digits == name.len() {
        return None;
    }
    let (prefix, num) = name.split_at(name.len() - digits);
    num.parse().ok().map(|n| (prefix, n))
}

/// Group one direction's ports into rows. `expanded` keys are
/// `"node.name/in|out/prefix<first>"`.
///
/// Alias interaction: a handful of named channels ("Guitar" on a
/// 128-port Inferno node) split OUT of their group so they're always
/// visible — the whole point of naming a channel is seeing it. But a
/// bank where ≥ GROUP_MIN channels are named (a full chanmap import)
/// stays grouped, or the card would explode back to 128 rows; the
/// aliases show when the group is expanded.
fn build_rows(
    node: &PwNode,
    ports: &[&PwPort],
    direction: PortDirection,
    expanded: &HashMap<String, bool>,
    aliases: &HashMap<String, String>,
) -> Vec<PortRow> {
    let aliased = |p: &PwPort| aliases.contains_key(&format!("{}:{}", node.name, p.name));
    let dir_str = match direction {
        PortDirection::Input => "in",
        PortDirection::Output => "out",
    };
    let mut rows = Vec::new();

    let single = |rows: &mut Vec<PortRow>, p: &PwPort| {
        rows.push(PortRow {
            label: p.name.clone(),
            direction,
            kind: p.media_kind,
            ports: vec![p.id],
            group_key: None,
            monitor: is_monitor(&p.name),
            y: 0.0,
        });
    };
    let group = |rows: &mut Vec<PortRow>, run: &[&PwPort]| {
        let (prefix, first) = split_numeric(&run[0].name).unwrap_or(("", 0));
        let last = split_numeric(&run[run.len() - 1].name)
            .map(|(_, n)| n)
            .unwrap_or(0);
        // `first` in the key keeps two segments of the same prefix
        // (split by a named channel) independently expandable.
        let key = format!("{}/{}/{}{}", node.name, dir_str, prefix, first);
        let is_expanded = expanded.get(&key).copied().unwrap_or(false);
        let monitor = is_monitor(&run[0].name);
        if is_expanded {
            rows.push(PortRow {
                label: format!("{}{}–{}", prefix, first, last),
                direction,
                kind: run[0].media_kind,
                ports: Vec::new(),
                group_key: Some(key),
                monitor,
                y: 0.0,
            });
            for p in run {
                single(rows, p);
            }
        } else {
            rows.push(PortRow {
                label: format!("{}{}–{}", prefix, first, last),
                direction,
                kind: run[0].media_kind,
                ports: run.iter().map(|p| p.id).collect(),
                group_key: Some(key),
                monitor,
                y: 0.0,
            });
        }
    };
    // A slice shorter than GROUP_MIN renders as singles.
    let segment = |rows: &mut Vec<PortRow>, seg: &[&PwPort]| {
        if seg.len() >= GROUP_MIN {
            group(rows, seg);
        } else {
            for p in seg {
                single(rows, p);
            }
        }
    };

    let mut i = 0;
    while i < ports.len() {
        // Extend a run of consecutive same-prefix numbered ports
        // (alias-blind — alias handling comes after).
        let run_end = match split_numeric(&ports[i].name) {
            None => i + 1,
            Some((prefix, mut num)) => {
                let mut j = i + 1;
                while j < ports.len() {
                    match split_numeric(&ports[j].name) {
                        Some((p, n)) if p == prefix && n == num + 1 => {
                            num = n;
                            j += 1;
                        }
                        _ => break,
                    }
                }
                j
            }
        };
        let run = &ports[i..run_end];
        i = run_end;

        if run.len() < GROUP_MIN {
            for p in run {
                single(&mut rows, p);
            }
            continue;
        }
        let aliased_count = run.iter().filter(|p| aliased(p)).count();
        if aliased_count == 0 || aliased_count >= GROUP_MIN {
            group(&mut rows, run);
            continue;
        }
        // A few named channels: split them out, group the gaps.
        let mut seg_start = 0;
        for k in 0..run.len() {
            if aliased(run[k]) {
                segment(&mut rows, &run[seg_start..k]);
                single(&mut rows, run[k]);
                seg_start = k + 1;
            }
        }
        segment(&mut rows, &run[seg_start..]);
    }
    rows
}

/// Does a port belong on the given media tab? `Other` (control/dsp
/// oddballs) rides with Audio so nothing is unreachable.
pub fn kind_on_tab(kind: MediaKind, tab: MediaKind) -> bool {
    match tab {
        MediaKind::Audio => matches!(kind, MediaKind::Audio | MediaKind::Other),
        other => kind == other,
    }
}

/// Which nodes/ports survive the current filters.
pub struct Filters<'a> {
    pub search: &'a str,
    /// Active media tab (Audio | Midi | Video).
    pub tab: MediaKind,
    pub hide_unconnected: bool,
    /// The full alias map (`node.name` and `node.name:port.name` keys):
    /// search matches what the user sees, and aliased ports stay out
    /// of collapsed groups.
    pub aliases: &'a HashMap<String, String>,
    /// Drop monitor ports entirely (cables through them disappear).
    pub hide_monitors: bool,
}

pub fn compute_layout(
    graph: &GraphSnapshot,
    filters: &Filters,
    expanded: &HashMap<String, bool>,
) -> GraphLayout {
    let search = filters.search.to_lowercase();
    let mut columns: [Vec<CardLayout>; 3] = [Vec::new(), Vec::new(), Vec::new()];

    for node in &graph.nodes {
        if !search.is_empty() {
            let alias = filters.aliases.get(&node.name).map(String::as_str).unwrap_or("");
            let hay = format!("{} {} {}", node.name, node.label, alias).to_lowercase();
            if !hay.contains(&search) {
                continue;
            }
        }
        let keep = |p: &&PwPort| {
            kind_on_tab(p.media_kind, filters.tab)
                && !(filters.hide_monitors && is_monitor(&p.name))
        };
        let mut ins: Vec<&PwPort> = graph
            .ports
            .iter()
            .filter(|p| p.node_id == node.id && p.direction == PortDirection::Input)
            .filter(keep)
            .collect();
        let mut outs: Vec<&PwPort> = graph
            .ports
            .iter()
            .filter(|p| p.node_id == node.id && p.direction == PortDirection::Output)
            .filter(keep)
            .collect();
        if ins.is_empty() && outs.is_empty() {
            continue; // metadata/factory nodes — nothing to patch
        }
        if filters.hide_unconnected {
            let touched = graph
                .links
                .iter()
                .any(|l| l.output_node == node.id || l.input_node == node.id);
            if !touched {
                continue;
            }
        }
        // Numeric-aware sort so playback_10 follows playback_9.
        let numeric_key = |p: &&PwPort| match split_numeric(&p.name) {
            Some((prefix, n)) => (prefix.to_string(), n),
            None => (p.name.clone(), 0),
        };
        ins.sort_by_key(numeric_key);
        outs.sort_by_key(numeric_key);

        let make_card = |rows: Vec<PortRow>, col: usize, key: String| {
            let mut rows = rows;
            for (idx, row) in rows.iter_mut().enumerate() {
                row.y = HEADER_H + (idx as f64 + 0.5) * ROW_H;
            }
            let h = HEADER_H + rows.len() as f64 * ROW_H + 8.0;
            CardLayout {
                node: node.clone(),
                key,
                x: MARGIN + col as f64 * (CARD_W + COL_GAP),
                y: 0.0,
                h,
                rows,
            }
        };

        // On the MIDI tab, pure-MIDI nodes (hardware bridges, midir
        // ports) are DEVICES: split them by direction — their outputs
        // (keyboards, control surfaces) go left, their inputs (synths,
        // light guides) right. Applications — anything that also
        // carries audio, or a stream — stay whole in the middle,
        // since their MIDI mostly routes within themselves.
        let is_app = graph
            .ports
            .iter()
            .any(|p| p.node_id == node.id && p.media_kind == MediaKind::Audio)
            || node.media_class.starts_with("Stream");
        if filters.tab == MediaKind::Midi && !is_app {
            if !outs.is_empty() {
                let rows =
                    build_rows(node, &outs, PortDirection::Output, expanded, filters.aliases);
                columns[0].push(make_card(rows, 0, format!("{}-out", node.id)));
            }
            if !ins.is_empty() {
                let rows =
                    build_rows(node, &ins, PortDirection::Input, expanded, filters.aliases);
                columns[2].push(make_card(rows, 2, format!("{}-in", node.id)));
            }
        } else {
            let mut rows =
                build_rows(node, &ins, PortDirection::Input, expanded, filters.aliases);
            rows.extend(build_rows(
                node,
                &outs,
                PortDirection::Output,
                expanded,
                filters.aliases,
            ));
            let col = column_of(&node.media_class);
            columns[col].push(make_card(rows, col, node.id.to_string()));
        }
    }

    // Stack each column; stable order by label keeps the layout calm
    // as ids churn.
    let mut cards = Vec::new();
    let mut height: f64 = 0.0;
    for col in &mut columns {
        col.sort_by(|a, b| a.node.label.to_lowercase().cmp(&b.node.label.to_lowercase()));
        let mut y = MARGIN + COL_HEADER_H;
        for mut card in col.drain(..) {
            card.y = y;
            y += card.h + CARD_GAP;
            cards.push(card);
        }
        height = height.max(y);
    }

    // Anchors: every port maps to its row's edge point (collapsed group
    // members all share the group row's anchor).
    let mut anchors = HashMap::new();
    for card in &cards {
        for row in &card.rows {
            let (x, y) = match row.direction {
                PortDirection::Input => (card.x, card.y + row.y),
                PortDirection::Output => (card.x + CARD_W, card.y + row.y),
            };
            for pid in &row.ports {
                anchors.insert(*pid, (x, y));
            }
        }
    }

    GraphLayout {
        cards,
        anchors,
        width: MARGIN * 2.0 + 3.0 * CARD_W + 2.0 * COL_GAP,
        height: height + MARGIN,
    }
}

#[cfg(test)]
mod reaper_layout {
    use super::*;

    /// REAPER joins the graph as a JACK client: `node.name = "REAPER"`,
    /// EMPTY `media.class` (kind `Other`), 128 audio `in*` + 128 audio
    /// `out*` ports. It must still lay out as a routable duplex card in
    /// the middle (Applications) column on the Audio tab — the "REAPER
    /// isn't a thing I can route to/from" regression.
    fn reaper_snapshot() -> GraphSnapshot {
        let node = PwNode {
            id: 1,
            name: "REAPER".into(),
            label: "REAPER".into(),
            media_class: String::new(), // JACK clients carry no media.class
            media_kind: MediaKind::Other,
            app_name: String::new(),
            latency: String::new(),
        };
        let mut ports = Vec::new();
        let mut pid = 100;
        for (dir, prefix) in [
            (PortDirection::Input, "in"),
            (PortDirection::Output, "out"),
        ] {
            for ch in 1..=128 {
                ports.push(PwPort {
                    id: pid,
                    node_id: 1,
                    name: format!("{prefix}{ch}"),
                    direction: dir,
                    media_kind: MediaKind::Audio,
                });
                pid += 1;
            }
        }
        GraphSnapshot { nodes: vec![node], ports, links: Vec::new() }
    }

    #[test]
    fn reaper_is_a_routable_card() {
        let graph = reaper_snapshot();
        let aliases = HashMap::new();
        let filters = Filters {
            search: "",
            tab: MediaKind::Audio,
            hide_unconnected: false,
            aliases: &aliases,
            hide_monitors: false,
        };
        let lay = compute_layout(&graph, &filters, &HashMap::new());

        let card = lay
            .cards
            .iter()
            .find(|c| c.node.name == "REAPER")
            .expect("REAPER must appear as a card on the Audio tab");

        // Empty media.class ⇒ Applications (middle) column.
        let middle_x = MARGIN + 1.0 * (CARD_W + COL_GAP);
        assert!(
            (card.x - middle_x).abs() < 1.0,
            "REAPER should land in the Applications column, got x={}",
            card.x
        );

        // Both directions are present, so it's routable to AND from.
        assert!(
            card.rows.iter().any(|r| r.direction == PortDirection::Input),
            "REAPER must expose input rows (route audio INTO it)"
        );
        assert!(
            card.rows.iter().any(|r| r.direction == PortDirection::Output),
            "REAPER must expose output rows (route audio OUT of it)"
        );

        // Every one of the 256 audio ports is anchored (reachable by a cable).
        assert_eq!(
            graph.ports.iter().filter(|p| lay.anchors.contains_key(&p.id)).count(),
            256,
            "all REAPER audio ports must be cable-anchored"
        );
    }
}

#[cfg(test)]
mod live_probe {
    use super::*;
    use patchbay_proto::PatchbayServiceClient;

    /// Diagnose UI-vs-engine drift against a RUNNING patchbay app:
    /// `cargo test -p patchbay-ui live_layout -- --ignored --nocapture`
    #[tokio::test]
    #[ignore = "needs a running patchbay app on :4046"]
    async fn live_layout() {
        let link = vox_websocket::WsLink::connect("ws://127.0.0.1:4046/vox")
            .await
            .expect("ws connect (is patchbay running?)");
        let client: PatchbayServiceClient =
            vox_core::initiator_on(link).establish().await.expect("establish");
        let graph = client.graph().await.expect("graph");
        println!(
            "graph: {} nodes / {} ports / {} links",
            graph.nodes.len(),
            graph.ports.len(),
            graph.links.len()
        );
        let aliases = HashMap::new();
        let filters = Filters {
            search: "",
            tab: MediaKind::Audio,
            hide_unconnected: false,
            aliases: &aliases,
            hide_monitors: false,
        };
        let lay = compute_layout(&graph, &filters, &HashMap::new());
        for col in 0..3 {
            let x = MARGIN + col as f64 * (CARD_W + COL_GAP);
            println!("── column {col} ({})", COLUMN_TITLES_DBG[col]);
            for c in lay.cards.iter().filter(|c| (c.x - x).abs() < 1.0) {
                println!("   y={:>6.0} h={:>5.0} {} [{}]", c.y, c.h, c.node.label, c.node.name);
            }
        }
        assert!(
            lay.cards.iter().any(|c| c.node.name == "REAPER"),
            "REAPER missing from layout"
        );
    }

    const COLUMN_TITLES_DBG: [&str; 3] = ["Inputs", "Applications", "Outputs"];
}
