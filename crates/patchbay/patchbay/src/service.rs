//! `PatchbayService` implementation over the engine + preset store.

use std::collections::HashSet;
use std::sync::Arc;
use std::sync::mpsc;

use parking_lot::RwLock;
use patchbay_proto::services::patchbay_service::{
    PatchbayServiceStreamSource, patchbay_service_stream_service_descriptor, stream_serve,
};
use patchbay_proto::{
    AliasEntry, ApplyReport, ClockInfo, DanteDevice, DanteStatus, GraphEvent, GraphSnapshot,
    PatchbayError, PatchbayService, PresetLink, RoutingPreset, ServiceAction, ServiceStatus,
    patchbay_service_service_descriptor, serve_patchbay_service,
};

use crate::engine::{self, Command, EngineHandle};
use crate::presets::PresetStore;
use crate::store::GraphStore;

/// The headless patchbay backend: PipeWire engine thread + graph mirror
/// + presets/aliases + the RPC surface. Cheap to clone; all state is
/// shared behind the `Arc`.
#[derive(Clone)]
pub struct PatchbayBackend {
    inner: Arc<Inner>,
}

struct Inner {
    store: Arc<RwLock<GraphStore>>,
    engine: EngineHandle,
    events_hub: architect::PubSub<GraphEvent>,
    presets: PresetStore,
    dante: crate::dante_net::DanteEndpoints,
}

impl Default for PatchbayBackend {
    fn default() -> Self {
        Self::new()
    }
}

impl PatchbayBackend {
    /// Spawn the PipeWire thread and the event pump. The replay window
    /// covers the snapshot→subscribe gap: clients fetch `graph()` first,
    /// then apply (idempotent) events from the recent past.
    pub fn new() -> Self {
        let store = Arc::new(RwLock::new(GraphStore::default()));
        let (events_tx, events_rx) = mpsc::channel::<GraphEvent>();
        let engine = engine::spawn(store.clone(), events_tx);
        let events_hub = architect::PubSub::sliding(256);
        let pump = events_hub.clone();
        std::thread::Builder::new()
            .name("patchbay-events".into())
            .spawn(move || {
                while let Ok(ev) = events_rx.recv() {
                    pump.publish(ev);
                }
                tracing::warn!("patchbay event pump ended (engine gone)");
            })
            .expect("spawn patchbay-events thread");
        Self {
            inner: Arc::new(Inner {
                store,
                engine,
                events_hub,
                presets: PresetStore::open(),
                dante: crate::dante_net::DanteEndpoints::default(),
            }),
        }
    }

    /// A fresh `LayerRouter` serving this backend — the RPC layer plus
    /// its `#[subscribe]` stream sibling over the same impl.
    pub fn router(&self) -> architect::LayerRouter {
        architect::LayerRouter::new()
            .with(
                patchbay_service_service_descriptor(),
                serve_patchbay_service(self.clone()),
            )
            .with(
                patchbay_service_stream_service_descriptor(),
                stream_serve(self.clone()),
            )
    }

    /// Resolve + send one create-link command. Returns whether a
    /// command was actually sent (false = link already exists).
    fn create_link_inner(&self, output_port: u32, input_port: u32) -> Result<bool, PatchbayError> {
        let (output_node, input_node) = {
            let store = self.inner.store.read();
            if store.link_between(output_port, input_port).is_some() {
                return Ok(false);
            }
            let out_node = store
                .node_of_port(output_port)
                .ok_or_else(|| PatchbayError::not_found("output port", output_port))?
                .id;
            let in_node = store
                .node_of_port(input_port)
                .ok_or_else(|| PatchbayError::not_found("input port", input_port))?
                .id;
            (out_node, in_node)
        };
        self.inner
            .engine
            .send(Command::CreateLink {
                output_node,
                output_port,
                input_node,
                input_port,
            })
            .map_err(PatchbayError::EngineUnavailable)?;
        Ok(true)
    }

    /// Current links as name-keyed [`PresetLink`]s (links with
    /// unresolvable endpoints are skipped).
    fn links_by_names(&self) -> Vec<PresetLink> {
        let store = self.inner.store.read();
        let mut out: Vec<PresetLink> = store
            .links
            .values()
            .filter_map(|l| {
                let on = store.nodes.get(&l.output_node)?;
                let op = store.ports.get(&l.output_port)?;
                let inn = store.nodes.get(&l.input_node)?;
                let ip = store.ports.get(&l.input_port)?;
                Some(PresetLink {
                    output_node: on.name.clone(),
                    output_port: op.name.clone(),
                    input_node: inn.name.clone(),
                    input_port: ip.name.clone(),
                })
            })
            .collect();
        out.sort_by(|a, b| {
            (&a.output_node, &a.output_port, &a.input_node, &a.input_port).cmp(&(
                &b.output_node,
                &b.output_port,
                &b.input_node,
                &b.input_port,
            ))
        });
        out.dedup();
        out
    }
}

impl PatchbayServiceStreamSource for PatchbayBackend {
    fn graph_events_hub(&self) -> &architect::PubSub<GraphEvent> {
        &self.inner.events_hub
    }
}

impl PatchbayService for PatchbayBackend {
    async fn graph(&self) -> Result<GraphSnapshot, PatchbayError> {
        Ok(self.inner.store.read().snapshot())
    }

    async fn create_link(&self, output_port: u32, input_port: u32) -> Result<(), PatchbayError> {
        self.create_link_inner(output_port, input_port).map(|_| ())
    }

    async fn destroy_link(&self, link_id: u32) -> Result<(), PatchbayError> {
        if !self.inner.store.read().links.contains_key(&link_id) {
            return Err(PatchbayError::not_found("link", link_id));
        }
        self.inner
            .engine
            .send(Command::DestroyLink { id: link_id })
            .map_err(PatchbayError::EngineUnavailable)
    }

    async fn list_presets(&self) -> Result<Vec<RoutingPreset>, PatchbayError> {
        Ok(self.inner.presets.presets())
    }

    async fn save_preset(
        &self,
        name: String,
        description: String,
    ) -> Result<RoutingPreset, PatchbayError> {
        if name.trim().is_empty() {
            return Err(PatchbayError::Internal("preset name is empty".into()));
        }
        let links = self.links_by_names();
        Ok(self.inner.presets.upsert_preset(name, description, links))
    }

    async fn apply_preset(
        &self,
        name: String,
        exclusive: bool,
    ) -> Result<ApplyReport, PatchbayError> {
        let preset = self
            .inner
            .presets
            .preset(&name)
            .ok_or_else(|| PatchbayError::not_found("preset", &name))?;
        let mut report = ApplyReport::default();

        // Create every remembered link whose endpoints are live.
        for link in &preset.links {
            let resolved = {
                let store = self.inner.store.read();
                let out = store.port_by_names(&link.output_node, &link.output_port);
                let inp = store.port_by_names(&link.input_node, &link.input_port);
                out.zip(inp)
            };
            match resolved {
                None => report.missing.push(link.clone()),
                Some((out, inp)) => match self.create_link_inner(out, inp)? {
                    true => report.created += 1,
                    false => report.existing += 1,
                },
            }
        }

        // Exclusive: tear down live links the preset doesn't contain.
        if exclusive {
            let wanted: HashSet<&PresetLink> = preset.links.iter().collect();
            let extras: Vec<u32> = {
                let store = self.inner.store.read();
                store
                    .links
                    .values()
                    .filter_map(|l| {
                        let named = PresetLink {
                            output_node: store.nodes.get(&l.output_node)?.name.clone(),
                            output_port: store.ports.get(&l.output_port)?.name.clone(),
                            input_node: store.nodes.get(&l.input_node)?.name.clone(),
                            input_port: store.ports.get(&l.input_port)?.name.clone(),
                        };
                        (!wanted.contains(&named)).then_some(l.id)
                    })
                    .collect()
            };
            for id in extras {
                self.inner
                    .engine
                    .send(Command::DestroyLink { id })
                    .map_err(PatchbayError::EngineUnavailable)?;
                report.destroyed += 1;
            }
        }
        Ok(report)
    }

    async fn delete_preset(&self, name: String) -> Result<(), PatchbayError> {
        self.inner
            .presets
            .delete_preset(&name)
            .then_some(())
            .ok_or_else(|| PatchbayError::not_found("preset", &name))
    }

    async fn aliases(&self) -> Result<Vec<AliasEntry>, PatchbayError> {
        Ok(self.inner.presets.aliases())
    }

    async fn set_alias(&self, target: String, alias: String) -> Result<(), PatchbayError> {
        self.inner.presets.set_alias(target, alias);
        Ok(())
    }

    async fn import_chanmap(&self, node: String, path: String) -> Result<u32, PatchbayError> {
        let names = crate::chanmap::read_names(&path).map_err(PatchbayError::Internal)?;
        // Every port of `node` whose numeric suffix is a named channel
        // gets the alias (playback + monitor + capture all match, so
        // the name shows on whichever side you're patching).
        let ports: Vec<String> = {
            let store = self.inner.store.read();
            let Some(n) = store.nodes.values().find(|n| n.name == node) else {
                return Err(PatchbayError::not_found("node", &node));
            };
            store
                .ports
                .values()
                .filter(|p| p.node_id == n.id)
                .map(|p| p.name.clone())
                .collect()
        };
        let mut written = 0;
        for port in ports {
            let Some(channel) = crate::chanmap::channel_of_port(&port) else {
                continue;
            };
            if let Some(name) = names.get(&channel) {
                self.inner
                    .presets
                    .set_alias(format!("{node}:{port}"), name.clone());
                written += 1;
            }
        }
        Ok(written)
    }

    async fn export_chanmap(&self, node: String, path: String) -> Result<u32, PatchbayError> {
        // channel → alias, from this node's aliased ports. Multiple
        // ports can share a channel (playback_5 + monitor_5) — first
        // alias wins, they're the same name after an import anyway.
        let prefix = format!("{node}:");
        let mut names = std::collections::BTreeMap::new();
        for entry in self.inner.presets.aliases() {
            let Some(port) = entry.target.strip_prefix(&prefix) else {
                continue;
            };
            let Some(channel) = crate::chanmap::channel_of_port(port) else {
                continue;
            };
            names.entry(channel).or_insert(entry.alias);
        }
        if names.is_empty() {
            return Err(PatchbayError::not_found("port aliases on node", &node));
        }
        crate::chanmap::write_names(&path, &names).map_err(PatchbayError::Internal)?;
        Ok(names.len() as u32)
    }

    async fn clock(&self) -> Result<ClockInfo, PatchbayError> {
        // Shells out — keep it off the async executor.
        Ok(tokio::task::spawn_blocking(crate::clock::clock_info)
            .await
            .map_err(|e| PatchbayError::Internal(e.to_string()))?)
    }

    async fn force_quantum(&self, frames: u32) -> Result<(), PatchbayError> {
        tokio::task::spawn_blocking(move || crate::clock::force_quantum(frames))
            .await
            .map_err(|e| PatchbayError::Internal(e.to_string()))
    }

    async fn dante_status(&self) -> Result<DanteStatus, PatchbayError> {
        Ok(tokio::task::spawn_blocking(crate::dante::status)
            .await
            .map_err(|e| PatchbayError::Internal(e.to_string()))?)
    }

    async fn set_dante(&self, on: bool) -> Result<(), PatchbayError> {
        tokio::task::spawn_blocking(move || crate::dante::set(on))
            .await
            .map_err(|e| PatchbayError::Internal(e.to_string()))?
            .map_err(PatchbayError::Internal)
    }

    async fn services(&self) -> Result<Vec<ServiceStatus>, PatchbayError> {
        Ok(tokio::task::spawn_blocking(crate::units::status_all)
            .await
            .map_err(|e| PatchbayError::Internal(e.to_string()))?)
    }

    async fn service_action(
        &self,
        unit: String,
        action: ServiceAction,
    ) -> Result<(), PatchbayError> {
        tokio::task::spawn_blocking(move || crate::units::action(&unit, action))
            .await
            .map_err(|e| PatchbayError::Internal(e.to_string()))?
    }

    async fn dante_network(&self) -> Result<Vec<DanteDevice>, PatchbayError> {
        self.inner.dante.network().await
    }

    async fn dante_subscribe(
        &self,
        rx_device: String,
        rx_channel: u32,
        tx_device: String,
        tx_channel: String,
    ) -> Result<(), PatchbayError> {
        self.inner
            .dante
            .subscribe(&rx_device, rx_channel, &tx_device, &tx_channel)
            .await
    }

    async fn dante_unsubscribe(
        &self,
        rx_device: String,
        rx_channel: u32,
    ) -> Result<(), PatchbayError> {
        self.inner.dante.unsubscribe(&rx_device, rx_channel).await
    }
}
