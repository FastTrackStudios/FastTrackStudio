//! FxRigBinding — live binding between a DAW FX chain and signal modules.
//!
//! Fetches an [`FxTree`] from a track's FX chain, runs discovery to map
//! containers to signal [`ModuleType`]s, and provides materialization
//! into signal-proto [`Module`] structs for UI consumption.

use daw_control::FxChain;
use daw_proto::FxTree;
use eyre::Result;
use signal::block::{Block, PluginFormat, PluginId};
use signal::discovery::{discover_rig, DiscoveredBlock, DiscoveredModule, DiscoveredRig};
use signal::module::{Module, ModuleBlock, ModuleType};
use signal::normalized::Order;

/// Live binding between a DAW track's FX chain and signal modules.
///
/// Holds a snapshot of the discovered rig structure. Call [`refresh`] to
/// re-fetch the FxTree from the DAW and re-run discovery.
#[derive(Debug, Clone)]
pub struct FxRigBinding {
    /// The discovered rig structure.
    pub rig: DiscoveredRig,
    /// Track GUID this binding is for.
    pub track_guid: String,
    /// Cached FxTree from the last fetch.
    tree: FxTree,
}

impl FxRigBinding {
    /// Create a binding by fetching the FxTree from a track's FX chain.
    pub async fn from_chain(chain: &FxChain, track_guid: &str) -> Result<Self> {
        let tree = chain.tree().await?;
        let rig = discover_rig(&tree, track_guid);
        Ok(Self {
            rig,
            track_guid: track_guid.to_string(),
            tree,
        })
    }

    /// Re-fetch the FxTree and re-run discovery.
    pub async fn refresh(&mut self, chain: &FxChain) -> Result<()> {
        self.tree = chain.tree().await?;
        self.rig = discover_rig(&self.tree, &self.track_guid);
        Ok(())
    }

    /// The discovered modules (containers that mapped to a ModuleType).
    pub fn modules(&self) -> &[DiscoveredModule] {
        &self.rig.modules
    }

    /// Find a discovered module by its ModuleType.
    pub fn module_by_type(&self, mt: ModuleType) -> Option<&DiscoveredModule> {
        self.rig.modules.iter().find(|m| m.module_type == mt)
    }

    /// Top-level FX not inside a recognized container.
    pub fn unassigned(&self) -> &[DiscoveredBlock] {
        &self.rig.unassigned
    }

    /// The cached FxTree from the last fetch/refresh.
    pub fn tree(&self) -> &FxTree {
        &self.tree
    }

    /// Materialize discovered modules into signal-proto Module structs.
    ///
    /// Each `DiscoveredModule` becomes a `Module` with its blocks populated
    /// from the discovered FX. This is the bridge from DAW state to signal
    /// domain state for UI consumption.
    pub fn to_signal_modules(&self) -> Vec<Module> {
        self.rig
            .modules
            .iter()
            .map(|dm| {
                let mut module = Module::new(&dm.container_name, dm.module_type);
                module.enabled = dm.enabled;
                module.fx_node_id = Some(dm.container_id.clone());

                for (i, db) in dm.blocks.iter().enumerate() {
                    let plugin_id =
                        PluginId::new(PluginFormat::Vst3, &db.fx.guid, &db.fx.plugin_name);
                    let block = Block::new(&db.fx.name, plugin_id);
                    let mut mb = ModuleBlock::new(block, Order::new(i as u8));
                    mb.fx_node_id = Some(db.node_id.clone());
                    module.add_block(mb);
                }

                module
            })
            .collect()
    }
}
