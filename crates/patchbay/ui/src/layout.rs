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
/// Runs shorter than this never group.
pub const GROUP_MIN: usize = 5;

/// One rendered row inside a card: a single port, or a collapsed group.
pub struct PortRow {
    pub label: String,
    pub direction: PortDirection,
    pub kind: MediaKind,
    /// All port ids this row anchors (1 for a plain port row).
    pub ports: Vec<u32>,
    /// Set when this row is a collapsible group (expansion key).
    pub group_key: Option<String>,
    /// y offset of the row center, relative to the card top.
    pub y: f64,
}

pub struct CardLayout {
    pub node: PwNode,
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
/// `"node.name/in|out/prefix"`.
fn build_rows(
    node: &PwNode,
    ports: &[&PwPort],
    direction: PortDirection,
    expanded: &HashMap<String, bool>,
) -> Vec<PortRow> {
    let dir_str = match direction {
        PortDirection::Input => "in",
        PortDirection::Output => "out",
    };
    let mut rows = Vec::new();
    let mut i = 0;
    while i < ports.len() {
        // Extend a run of consecutive same-prefix numbered ports.
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
        if run.len() >= GROUP_MIN {
            let prefix = split_numeric(&run[0].name).map(|(p, _)| p).unwrap_or("");
            let key = format!("{}/{}/{}", node.name, dir_str, prefix);
            let is_expanded = expanded.get(&key).copied().unwrap_or(false);
            if is_expanded {
                // Group header row (collapse affordance), then every port.
                rows.push(PortRow {
                    label: format!("{}… ({})", prefix.trim_end_matches('_'), run.len()),
                    direction,
                    kind: run[0].media_kind,
                    ports: Vec::new(),
                    group_key: Some(key),
                    y: 0.0,
                });
                for p in run {
                    rows.push(PortRow {
                        label: p.name.clone(),
                        direction,
                        kind: p.media_kind,
                        ports: vec![p.id],
                        group_key: None,
                        y: 0.0,
                    });
                }
            } else {
                let first = split_numeric(&run[0].name).map(|(_, n)| n).unwrap_or(0);
                let last = split_numeric(&run[run.len() - 1].name)
                    .map(|(_, n)| n)
                    .unwrap_or(0);
                rows.push(PortRow {
                    label: format!("{}{}–{}", prefix, first, last),
                    direction,
                    kind: run[0].media_kind,
                    ports: run.iter().map(|p| p.id).collect(),
                    group_key: Some(key),
                    y: 0.0,
                });
            }
        } else {
            for p in run {
                rows.push(PortRow {
                    label: p.name.clone(),
                    direction,
                    kind: p.media_kind,
                    ports: vec![p.id],
                    group_key: None,
                    y: 0.0,
                });
            }
        }
        i = run_end;
    }
    rows
}

/// Which nodes/ports survive the current filters.
pub struct Filters<'a> {
    pub search: &'a str,
    pub kinds: &'a [MediaKind],
    pub hide_unconnected: bool,
    /// `node.name → alias` so search matches what the user sees.
    pub aliases: &'a HashMap<String, String>,
}

pub fn compute_layout(
    graph: &GraphSnapshot,
    filters: &Filters,
    expanded: &HashMap<String, bool>,
) -> GraphLayout {
    let search = filters.search.to_lowercase();
    let mut columns: [Vec<CardLayout>; 3] = [Vec::new(), Vec::new(), Vec::new()];

    for node in &graph.nodes {
        if !filters.kinds.contains(&node.media_kind) {
            continue;
        }
        if !search.is_empty() {
            let alias = filters.aliases.get(&node.name).map(String::as_str).unwrap_or("");
            let hay = format!("{} {} {}", node.name, node.label, alias).to_lowercase();
            if !hay.contains(&search) {
                continue;
            }
        }
        let mut ins: Vec<&PwPort> = graph
            .ports
            .iter()
            .filter(|p| p.node_id == node.id && p.direction == PortDirection::Input)
            .collect();
        let mut outs: Vec<&PwPort> = graph
            .ports
            .iter()
            .filter(|p| p.node_id == node.id && p.direction == PortDirection::Output)
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

        let mut rows = build_rows(node, &ins, PortDirection::Input, expanded);
        rows.extend(build_rows(node, &outs, PortDirection::Output, expanded));
        for (idx, row) in rows.iter_mut().enumerate() {
            row.y = HEADER_H + (idx as f64 + 0.5) * ROW_H;
        }
        let h = HEADER_H + rows.len() as f64 * ROW_H + 8.0;

        let col = match (outs.is_empty(), ins.is_empty()) {
            (false, true) => 0,  // pure source
            (false, false) => 1, // duplex
            _ => 2,              // pure sink
        };
        columns[col].push(CardLayout {
            node: node.clone(),
            x: MARGIN + col as f64 * (CARD_W + COL_GAP),
            y: 0.0,
            h,
            rows,
        });
    }

    // Stack each column; stable order by label keeps the layout calm
    // as ids churn.
    let mut cards = Vec::new();
    let mut height: f64 = 0.0;
    for col in &mut columns {
        col.sort_by(|a, b| a.node.label.to_lowercase().cmp(&b.node.label.to_lowercase()));
        let mut y = MARGIN;
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
