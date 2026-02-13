//! FxTree discovery — map a DAW FX chain tree to signal modules.
//!
//! The [`discover_rig`] function walks an [`FxTree`] and produces a
//! [`DiscoveredRig`] by matching top-level containers against known
//! [`ModuleType`] names (via [`ModuleType::from_container_name`]).
//!
//! Top-level plugins that aren't inside a recognized container end up
//! in the `unassigned` bucket.

use daw_proto::{Fx, FxNode, FxNodeId, FxNodeKind, FxRoutingMode, FxTree};
use signal_proto::module::ModuleType;

// ─────────────────────────────────────────────────────────────────────────────
// Discovery types
// ─────────────────────────────────────────────────────────────────────────────

/// A discovered block — either a plugin or a nested container with children.
#[derive(Debug, Clone)]
pub struct DiscoveredBlock {
    /// Stable node ID in the FxTree.
    pub node_id: FxNodeId,
    /// The FX info (for plugins) or a synthetic Fx with the container name.
    pub fx: Fx,
    /// Whether this node is enabled (not bypassed).
    pub enabled: bool,
    /// For nested containers, their child blocks.
    pub children: Vec<DiscoveredBlock>,
}

/// A discovered module — a top-level container that maps to a signal ModuleType.
#[derive(Debug, Clone)]
pub struct DiscoveredModule {
    /// The signal module type this container maps to.
    pub module_type: ModuleType,
    /// The FxNodeId of the container in the FxTree.
    pub container_id: FxNodeId,
    /// Display name of the container (e.g. "DRIVE", "AMP").
    pub container_name: String,
    /// Routing mode (serial or parallel).
    pub routing: FxRoutingMode,
    /// Whether the container is enabled.
    pub enabled: bool,
    /// The blocks (plugins/nested containers) inside this module.
    pub blocks: Vec<DiscoveredBlock>,
}

/// The result of discovering a rig from an FxTree.
#[derive(Debug, Clone)]
pub struct DiscoveredRig {
    /// Track GUID this rig was discovered from.
    pub track_guid: String,
    /// Modules discovered from recognized containers, in chain order.
    pub modules: Vec<DiscoveredModule>,
    /// Top-level FX that aren't inside a recognized container.
    pub unassigned: Vec<DiscoveredBlock>,
}

// ─────────────────────────────────────────────────────────────────────────────
// Discovery
// ─────────────────────────────────────────────────────────────────────────────

/// Walk an FxTree and map its structure to signal modules.
///
/// Top-level containers whose names match a [`ModuleType`] become
/// [`DiscoveredModule`]s. Everything else goes into `unassigned`.
pub fn discover_rig(tree: &FxTree, track_guid: &str) -> DiscoveredRig {
    let mut modules = Vec::new();
    let mut unassigned = Vec::new();

    for node in tree.iter() {
        match &node.kind {
            FxNodeKind::Container {
                name,
                children,
                routing,
                ..
            } => {
                if let Some(module_type) = ModuleType::from_container_name(name) {
                    modules.push(DiscoveredModule {
                        module_type,
                        container_id: node.id.clone(),
                        container_name: name.clone(),
                        routing: *routing,
                        enabled: node.enabled,
                        blocks: children.iter().map(node_to_block).collect(),
                    });
                } else {
                    // Unrecognized container — treat as unassigned block
                    unassigned.push(node_to_block(node));
                }
            }
            FxNodeKind::Plugin(_) => {
                unassigned.push(node_to_block(node));
            }
        }
    }

    DiscoveredRig {
        track_guid: track_guid.to_string(),
        modules,
        unassigned,
    }
}

/// Convert an FxNode to a DiscoveredBlock (recursive for containers).
fn node_to_block(node: &FxNode) -> DiscoveredBlock {
    match &node.kind {
        FxNodeKind::Plugin(fx) => DiscoveredBlock {
            node_id: node.id.clone(),
            fx: fx.clone(),
            enabled: node.enabled,
            children: Vec::new(),
        },
        FxNodeKind::Container { name, children, .. } => DiscoveredBlock {
            node_id: node.id.clone(),
            fx: Fx::new(node.id.as_str().to_string(), 0, name.clone()),
            enabled: node.enabled,
            children: children.iter().map(node_to_block).collect(),
        },
    }
}

// ─────────────────────────────────────────────────────────────────────────────
// Tests
// ─────────────────────────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;
    use daw_proto::FxContainerChannelConfig;

    fn test_fx(name: &str, guid: &str) -> Fx {
        Fx::new(guid.to_string(), 0, name.to_string())
    }

    /// Build a realistic guitar rig tree:
    /// ```text
    /// [0] ReaTune (plugin — unassigned)
    /// [1] DRIVE (container, serial)
    ///     [1.0] Protein Green (plugin)
    ///     [1.1] ReaEQ (plugin)
    /// [2] AMP (container, serial)
    ///     [2.0] NAM (container, serial — nested for A/B)
    ///         [2.0.0] NeuralAmpModeler (plugin)
    ///         [2.0.1] Volume/Pan Smoother (plugin)
    ///     [2.1] CabSim (plugin)
    /// [3] TIME (container, serial)
    ///     [3.0] ReaDelay (plugin)
    ///     [3.1] ReaVerbate (plugin)
    /// [4] MASTER (container, serial)
    ///     [4.0] ReaComp (plugin)
    /// ```
    fn build_guitar_rig_tree() -> FxTree {
        let reatune = FxNode::plugin(
            FxNodeId::from_guid("guid-reatune"),
            test_fx("ReaTune", "guid-reatune"),
            true,
            None,
        );

        let mut drive = FxNode::container(
            FxNodeId::container("1"),
            "DRIVE",
            FxRoutingMode::Serial,
            FxContainerChannelConfig::stereo(),
            true,
            None,
        );
        drive.children_mut().unwrap().push(FxNode::plugin(
            FxNodeId::from_guid("guid-protein"),
            test_fx("Protein Green", "guid-protein"),
            true,
            Some(FxNodeId::container("1")),
        ));
        drive.children_mut().unwrap().push(FxNode::plugin(
            FxNodeId::from_guid("guid-reaeq"),
            test_fx("ReaEQ", "guid-reaeq"),
            true,
            Some(FxNodeId::container("1")),
        ));

        let mut nam = FxNode::container(
            FxNodeId::container("2:0"),
            "NAM",
            FxRoutingMode::Serial,
            FxContainerChannelConfig::stereo(),
            true,
            Some(FxNodeId::container("2")),
        );
        nam.children_mut().unwrap().push(FxNode::plugin(
            FxNodeId::from_guid("guid-nam"),
            test_fx("NeuralAmpModeler", "guid-nam"),
            true,
            Some(FxNodeId::container("2:0")),
        ));
        nam.children_mut().unwrap().push(FxNode::plugin(
            FxNodeId::from_guid("guid-smoother"),
            test_fx("Volume/Pan Smoother", "guid-smoother"),
            true,
            Some(FxNodeId::container("2:0")),
        ));

        let mut amp = FxNode::container(
            FxNodeId::container("2"),
            "AMP",
            FxRoutingMode::Serial,
            FxContainerChannelConfig::stereo(),
            true,
            None,
        );
        amp.children_mut().unwrap().push(nam);
        amp.children_mut().unwrap().push(FxNode::plugin(
            FxNodeId::from_guid("guid-cabsim"),
            test_fx("CabSim", "guid-cabsim"),
            true,
            Some(FxNodeId::container("2")),
        ));

        let mut time = FxNode::container(
            FxNodeId::container("3"),
            "TIME",
            FxRoutingMode::Serial,
            FxContainerChannelConfig::stereo(),
            true,
            None,
        );
        time.children_mut().unwrap().push(FxNode::plugin(
            FxNodeId::from_guid("guid-delay"),
            test_fx("ReaDelay", "guid-delay"),
            true,
            Some(FxNodeId::container("3")),
        ));
        time.children_mut().unwrap().push(FxNode::plugin(
            FxNodeId::from_guid("guid-reverb"),
            test_fx("ReaVerbate", "guid-reverb"),
            false,
            Some(FxNodeId::container("3")),
        ));

        let mut master = FxNode::container(
            FxNodeId::container("4"),
            "MASTER",
            FxRoutingMode::Serial,
            FxContainerChannelConfig::stereo(),
            true,
            None,
        );
        master.children_mut().unwrap().push(FxNode::plugin(
            FxNodeId::from_guid("guid-reacomp"),
            test_fx("ReaComp", "guid-reacomp"),
            true,
            Some(FxNodeId::container("4")),
        ));

        FxTree::from_nodes(vec![reatune, drive, amp, time, master])
    }

    #[test]
    fn discovers_modules_in_chain_order() {
        let tree = build_guitar_rig_tree();
        let rig = discover_rig(&tree, "track-guid-123");

        assert_eq!(rig.track_guid, "track-guid-123");
        assert_eq!(rig.modules.len(), 4);

        assert_eq!(rig.modules[0].module_type, ModuleType::Drive);
        assert_eq!(rig.modules[1].module_type, ModuleType::Amp);
        assert_eq!(rig.modules[2].module_type, ModuleType::Time);
        assert_eq!(rig.modules[3].module_type, ModuleType::Master);
    }

    #[test]
    fn discovers_blocks_within_modules() {
        let tree = build_guitar_rig_tree();
        let rig = discover_rig(&tree, "guid");

        // DRIVE has 2 plugin blocks
        let drive = &rig.modules[0];
        assert_eq!(drive.blocks.len(), 2);
        assert_eq!(drive.blocks[0].fx.name, "Protein Green");
        assert_eq!(drive.blocks[1].fx.name, "ReaEQ");

        // AMP has NAM container + CabSim plugin
        let amp = &rig.modules[1];
        assert_eq!(amp.blocks.len(), 2);
        assert_eq!(amp.blocks[0].fx.name, "NAM"); // nested container
        assert_eq!(amp.blocks[0].children.len(), 2);
        assert_eq!(amp.blocks[0].children[0].fx.name, "NeuralAmpModeler");
        assert_eq!(amp.blocks[0].children[1].fx.name, "Volume/Pan Smoother");
        assert_eq!(amp.blocks[1].fx.name, "CabSim");
    }

    #[test]
    fn unassigned_fx_collected() {
        let tree = build_guitar_rig_tree();
        let rig = discover_rig(&tree, "guid");

        assert_eq!(rig.unassigned.len(), 1);
        assert_eq!(rig.unassigned[0].fx.name, "ReaTune");
        assert_eq!(
            rig.unassigned[0].node_id,
            FxNodeId::from_guid("guid-reatune")
        );
    }

    #[test]
    fn preserves_enabled_state() {
        let tree = build_guitar_rig_tree();
        let rig = discover_rig(&tree, "guid");

        // TIME module's ReaVerbate is disabled
        let time = &rig.modules[2];
        assert!(time.blocks[0].enabled); // ReaDelay enabled
        assert!(!time.blocks[1].enabled); // ReaVerbate disabled
    }

    #[test]
    fn preserves_container_metadata() {
        let tree = build_guitar_rig_tree();
        let rig = discover_rig(&tree, "guid");

        let drive = &rig.modules[0];
        assert_eq!(drive.container_name, "DRIVE");
        assert_eq!(drive.container_id, FxNodeId::container("1"));
        assert_eq!(drive.routing, FxRoutingMode::Serial);
        assert!(drive.enabled);
    }

    #[test]
    fn empty_tree_produces_empty_rig() {
        let tree = FxTree::new();
        let rig = discover_rig(&tree, "guid");

        assert!(rig.modules.is_empty());
        assert!(rig.unassigned.is_empty());
    }

    #[test]
    fn unrecognized_container_goes_to_unassigned() {
        let mut tree = FxTree::new();
        let custom = FxNode::container(
            FxNodeId::container("0"),
            "MY CUSTOM THING",
            FxRoutingMode::Serial,
            FxContainerChannelConfig::stereo(),
            true,
            None,
        );
        tree.push(custom);

        let rig = discover_rig(&tree, "guid");
        assert!(rig.modules.is_empty());
        assert_eq!(rig.unassigned.len(), 1);
        assert_eq!(rig.unassigned[0].fx.name, "MY CUSTOM THING");
    }
}
