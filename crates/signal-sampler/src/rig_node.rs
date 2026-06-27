//! Composition tree for the Nord-style **Keys rig** — the structure that the
//! flat guitar-rig chain can't express. See `docs/nord-stage-4-signal-routing.md`.
//!
//! Two orthogonal axes (per the design doc):
//!
//! 1. **Containment (the folder tree)** — a [`RigNode`] is either a leaf
//!    [`RigBlock`] or a [`Container`]: an infinitely-nestable "block folder".
//!    A container carries a [`Role`] label (Preset / Engine / Layer / Module —
//!    intent only) and a [`Combine`] rule (Serial = chain children; Parallel =
//!    sum children). `Layer`/`Engine`/`Preset` are just Modules with a role tag;
//!    grouping is always "nest another Module", never "a Layer inside a Layer".
//!
//! 2. **Routing (the signal graph)** — independent of containment. Each container
//!    also holds **modulators** (control-rate [`RigBlock`]s — `Envelope`/`Lfo`/
//!    `Arpeggiator` — that drive params, not audio) and **sends** (cross-tree
//!    audio routes, e.g. a layer's "To Rotary"). The tree groups/owns; these
//!    route. Cross-layer routing lives here, not in the tree shape.
//!
//! This module is **structure only** — every block is a placeholder
//! ([`RigBlock`] with no realization → `has_backend() == false`). DSP gets
//! implemented block-type by block-type later; the routing is locked first.

use signal_proto::block::BlockType;

use crate::rig::RigBlock;

/// A node in the composition tree: a leaf processor or a container.
#[derive(Debug, Clone)]
pub enum RigNode {
    Block(RigBlock),
    Container(Container),
}

/// Semantic role of a container — a label describing intent. The audio behaviour
/// is set by [`Combine`], not by this; roles drive display + where shared-vs-
/// per-child processing is understood to sit.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Role {
    /// The whole program (top of the tree).
    Preset,
    /// An instrument part (Organ / Keys / Synth).
    Engine,
    /// A processing lane; its parallel siblings sum.
    Layer,
    /// A serial folder / signal-chain segment (infinitely nestable).
    Module,
}

impl Role {
    pub const fn tag(self) -> &'static str {
        match self {
            Role::Preset => "Preset",
            Role::Engine => "Engine",
            Role::Layer => "Layer",
            Role::Module => "Module",
        }
    }
}

/// How a container combines its children into its output.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Combine {
    /// Children chained in order: `child[0] → child[1] → … → out`.
    Serial,
    /// Children fed the same input; their outputs summed (parallel lanes).
    Parallel,
}

impl Combine {
    pub const fn tag(self) -> &'static str {
        match self {
            Combine::Serial => "serial",
            Combine::Parallel => "parallel",
        }
    }
}

/// A cross-tree audio send (the routing axis) — this container's output also
/// flows to the node named `target` (e.g. a layer routing "To Rotary").
#[derive(Debug, Clone)]
pub struct Send {
    /// Name of the destination node (resolved against the tree).
    pub target: String,
    /// Human label for the route, e.g. "To Rotary".
    pub label: String,
}

/// A container node: a named folder of children with a combine rule, plus the
/// routing-axis attachments (modulators + sends).
#[derive(Debug, Clone)]
pub struct Container {
    pub role: Role,
    pub name: String,
    pub combine: Combine,
    /// Audio children, in order.
    pub children: Vec<RigNode>,
    /// Control-rate modulators attached here (drive params via routes, not audio).
    pub modulators: Vec<RigBlock>,
    /// Cross-tree audio sends from this node's output.
    pub sends: Vec<Send>,
    /// Container-level settings that aren't blocks — e.g. a Layer's `voice_mode`,
    /// `unison`, `octave`, `level`; an Engine's menu options. `(name, value)`.
    pub params: Vec<(String, String)>,
    /// Whether this whole subtree is bypassed.
    pub bypassed: bool,
}

impl From<RigBlock> for RigNode {
    fn from(b: RigBlock) -> Self {
        RigNode::Block(b)
    }
}

impl From<Container> for RigNode {
    fn from(c: Container) -> Self {
        RigNode::Container(c)
    }
}

impl RigNode {
    pub fn name(&self) -> &str {
        match self {
            RigNode::Block(b) => &b.name,
            RigNode::Container(c) => &c.name,
        }
    }

    pub fn as_container(&self) -> Option<&Container> {
        match self {
            RigNode::Container(c) => Some(c),
            _ => None,
        }
    }
}

impl Container {
    fn new(role: Role, name: impl Into<String>, combine: Combine) -> Self {
        Self {
            role,
            name: name.into(),
            combine,
            children: Vec::new(),
            modulators: Vec::new(),
            sends: Vec::new(),
            params: Vec::new(),
            bypassed: false,
        }
    }

    // ── Fluent builders ──────────────────────────────────────────────────

    /// A serial Module (a folder / signal-chain segment).
    pub fn module(name: impl Into<String>) -> Self {
        Self::new(Role::Module, name, Combine::Serial)
    }
    /// A parallel folder (children sum) — e.g. an engine's set of voice Layers.
    pub fn parallel(name: impl Into<String>) -> Self {
        Self::new(Role::Module, name, Combine::Parallel)
    }
    /// A processing Layer (serial inside; sums with its parallel siblings).
    pub fn layer(name: impl Into<String>) -> Self {
        Self::new(Role::Layer, name, Combine::Serial)
    }
    /// An Engine (instrument part).
    pub fn engine(name: impl Into<String>) -> Self {
        Self::new(Role::Engine, name, Combine::Serial)
    }
    /// A Preset (whole program).
    pub fn preset(name: impl Into<String>) -> Self {
        Self::new(Role::Preset, name, Combine::Serial)
    }

    /// Append a child node (block or container).
    #[must_use]
    pub fn add(mut self, child: impl Into<RigNode>) -> Self {
        self.children.push(child.into());
        self
    }

    /// Append several children.
    #[must_use]
    pub fn extend(mut self, children: impl IntoIterator<Item = RigNode>) -> Self {
        self.children.extend(children);
        self
    }

    /// Convenience: append a placeholder leaf block of `block_type`, named.
    #[must_use]
    pub fn block(mut self, block_type: BlockType, name: impl Into<String>) -> Self {
        self.children
            .push(RigNode::Block(RigBlock::of_type(block_type).named(name)));
        self
    }

    /// Attach a control-rate modulator (placeholder block) to this container.
    #[must_use]
    pub fn modulator(mut self, block_type: BlockType, name: impl Into<String>) -> Self {
        self.modulators
            .push(RigBlock::of_type(block_type).named(name));
        self
    }

    /// Add a cross-tree send from this node's output to `target`.
    #[must_use]
    pub fn send(mut self, target: impl Into<String>, label: impl Into<String>) -> Self {
        self.sends.push(Send {
            target: target.into(),
            label: label.into(),
        });
        self
    }

    /// Set a container-level setting (e.g. `voice_mode`, `unison`, `octave`).
    #[must_use]
    pub fn param(mut self, name: impl Into<String>, value: impl Into<String>) -> Self {
        self.params.push((name.into(), value.into()));
        self
    }

    // ── Queries ──────────────────────────────────────────────────────────

    /// Every leaf block in this subtree (recursive, audio tree only — excludes
    /// modulators).
    pub fn blocks(&self) -> Vec<&RigBlock> {
        let mut out = Vec::new();
        self.collect_blocks(&mut out);
        out
    }

    fn collect_blocks<'a>(&'a self, out: &mut Vec<&'a RigBlock>) {
        for child in &self.children {
            match child {
                RigNode::Block(b) => out.push(b),
                RigNode::Container(c) => c.collect_blocks(out),
            }
        }
    }

    /// All modulator blocks in this subtree (recursive).
    pub fn modulators_recursive(&self) -> Vec<&RigBlock> {
        let mut out: Vec<&RigBlock> = self.modulators.iter().collect();
        for child in &self.children {
            if let RigNode::Container(c) = child {
                out.extend(c.modulators_recursive());
            }
        }
        out
    }

    /// Find the first descendant container named `name` (depth-first, incl self).
    pub fn find(&self, name: &str) -> Option<&Container> {
        if self.name == name {
            return Some(self);
        }
        for child in &self.children {
            if let RigNode::Container(c) = child {
                if let Some(found) = c.find(name) {
                    return Some(found);
                }
            }
        }
        None
    }

    /// Containers of a given role anywhere in the subtree (incl self).
    pub fn of_role(&self, role: Role) -> Vec<&Container> {
        let mut out = Vec::new();
        self.collect_role(role, &mut out);
        out
    }

    fn collect_role<'a>(&'a self, role: Role, out: &mut Vec<&'a Container>) {
        if self.role == role {
            out.push(self);
        }
        for child in &self.children {
            if let RigNode::Container(c) = child {
                c.collect_role(role, out);
            }
        }
    }

    /// All cross-tree sends in this subtree, as `(from_name, Send)`.
    pub fn sends_recursive(&self) -> Vec<(&str, &Send)> {
        let mut out: Vec<(&str, &Send)> = self
            .sends
            .iter()
            .map(|s| (self.name.as_str(), s))
            .collect();
        for child in &self.children {
            if let RigNode::Container(c) = child {
                out.extend(c.sends_recursive());
            }
        }
        out
    }

    /// Render the subtree as an indented routing diagram (for inspection/tests).
    pub fn dump(&self) -> String {
        let mut s = String::new();
        self.dump_into(&mut s, "", true, true);
        s
    }

    fn dump_into(&self, out: &mut String, prefix: &str, last: bool, root: bool) {
        let (branch, child_prefix) = if root {
            ("", String::new())
        } else if last {
            ("└─ ", format!("{prefix}   "))
        } else {
            ("├─ ", format!("{prefix}│  "))
        };
        out.push_str(prefix);
        out.push_str(branch);
        out.push_str(&format!(
            "{} \"{}\" [{}]",
            self.role.tag(),
            self.name,
            self.combine.tag()
        ));
        if !self.params.is_empty() {
            let ps: Vec<String> = self
                .params
                .iter()
                .map(|(k, v)| format!("{k}={v}"))
                .collect();
            out.push_str(&format!("  {{{}}}", ps.join(", ")));
        }
        for m in &self.modulators {
            out.push_str(&format!("  ~{}:{}", m.block_type_tag(), m.display_name()));
        }
        for snd in &self.sends {
            out.push_str(&format!("  ⟿ {}→{}", snd.label, snd.target));
        }
        out.push('\n');

        let n = self.children.len();
        for (i, child) in self.children.iter().enumerate() {
            let is_last = i + 1 == n;
            match child {
                RigNode::Container(c) => c.dump_into(out, &child_prefix, is_last, false),
                RigNode::Block(b) => {
                    let bb = if is_last { "└─ " } else { "├─ " };
                    out.push_str(&child_prefix);
                    out.push_str(bb);
                    out.push_str(&format!(
                        "Block {} \"{}\"{}\n",
                        b.block_type_tag(),
                        b.display_name(),
                        if b.has_backend() { "" } else { " (placeholder)" }
                    ));
                }
            }
        }
    }
}

impl RigBlock {
    /// Lowercase tag of the block's type (e.g. "amp", "delay") for display/dump.
    pub fn block_type_tag(&self) -> &'static str {
        self.block_type.as_str()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn builds_a_nested_tree_and_finds_nodes() {
        let preset = Container::preset("P")
            .add(
                Container::parallel("Voices")
                    .add(Container::layer("A").block(BlockType::Oscillator, "Osc"))
                    .add(Container::layer("B").block(BlockType::Oscillator, "Osc")),
            )
            .add(Container::module("Global").block(BlockType::Rotary, "Rotary"));

        // Two layers, three blocks (2 osc + rotary).
        assert_eq!(preset.of_role(Role::Layer).len(), 2);
        assert_eq!(preset.blocks().len(), 3);
        assert!(preset.find("Voices").is_some());
        assert!(preset.find("Rotary").is_none()); // Rotary is a Block, not a container
        // Every block is a placeholder (no DSP yet).
        assert!(preset.blocks().iter().all(|b| !b.has_backend()));
    }

    #[test]
    fn modules_nest_infinitely() {
        let m = Container::module("Delay")
            .block(BlockType::Delay, "Delay")
            .add(Container::module("Feedback FX").block(BlockType::Chorus, "fb"))
            .block(BlockType::Filter, "fb-filter");
        // Delay + fb + fb-filter = 3 leaf blocks across the nested modules.
        assert_eq!(m.blocks().len(), 3);
    }

    #[test]
    fn sends_and_modulators_are_collected() {
        let layer = Container::layer("Synth A")
            .modulator(BlockType::Envelope, "Amp Env")
            .modulator(BlockType::Lfo, "LFO")
            .add(Container::module("Amp/EQ").send("Rotary", "To Rotary"));
        assert_eq!(layer.modulators_recursive().len(), 2);
        let sends = layer.sends_recursive();
        assert_eq!(sends.len(), 1);
        assert_eq!(sends[0].1.label, "To Rotary");
    }
}
