//! `PatchbayService` implementation over the engine + preset store.

use std::collections::HashSet;
use std::sync::Arc;
use std::sync::mpsc;

use parking_lot::RwLock;
use patchbay_proto::services::patchbay_service::{
    PatchbayServiceStreamSource, patchbay_service_stream_service_descriptor, stream_serve,
};
use patchbay_proto::{
    AliasEntry, ApplyReport, ClockDefaults, ClockInfo, ColorEntry, DanteDevice, DanteStatus,
    GraphEvent, GraphSnapshot, IconEntry, LatencyRule, PatchbayError, PatchbayService, PresetLink,
    RoutingPreset, ServiceAction, ServiceStatus, VirtualSink,
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
    presets: Arc<PresetStore>,
    dante: crate::dante_net::DanteEndpoints,
    icons: crate::icons::IconCache,
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
        let presets = Arc::new(PresetStore::open());
        let (events_tx, events_rx) = mpsc::channel::<GraphEvent>();
        let enrich_tx = events_tx.clone();
        let engine = engine::spawn(store.clone(), events_tx);
        // Debounce gate for the pw-dump enrichment pass.
        let enrich_pending = Arc::new(std::sync::atomic::AtomicBool::new(false));
        // Big window: a single app connecting is a burst of hundreds of
        // port events, a PipeWire reconnect is ~2000 — a small ring
        // drops the middle of the burst and clients silently lose
        // nodes (REAPER "not showing up" was exactly this).
        let events_hub = architect::PubSub::sliding(16_384);
        let pump = events_hub.clone();
        {
            let store = store.clone();
            let presets = presets.clone();
            let engine = engine.clone();
            std::thread::Builder::new()
                .name("patchbay-events".into())
                .spawn(move || {
                    while let Ok(ev) = events_rx.recv() {
                        // Any node addition schedules a debounced
                        // pw-dump enrichment (full props the registry
                        // subset lacks — node.group, application.*).
                        if matches!(ev, GraphEvent::NodeAdded(_))
                            && !enrich_pending.swap(true, std::sync::atomic::Ordering::SeqCst)
                        {
                            let store = store.clone();
                            let tx = enrich_tx.clone();
                            let pending = enrich_pending.clone();
                            std::thread::spawn(move || {
                                std::thread::sleep(std::time::Duration::from_millis(1500));
                                pending.store(false, std::sync::atomic::Ordering::SeqCst);
                                crate::enrich::enrich_nodes(&store, &tx);
                            });
                        }
                        match &ev {
                            // REAPER (re)appeared: pick up its channel
                            // names from the host chanmap once the
                            // ports have registered.
                            GraphEvent::NodeAdded(n) if n.name == "REAPER" => {
                                let store = store.clone();
                                let presets = presets.clone();
                                std::thread::spawn(move || {
                                    std::thread::sleep(std::time::Duration::from_secs(2));
                                    auto_import_chanmap(&store, &presets, "REAPER");
                                });
                            }
                            // Engine reconnected (PipeWire restart):
                            // re-create the persisted virtual sinks
                            // once the fresh mirror has settled.
                            GraphEvent::Reset => {
                                let store = store.clone();
                                let presets = presets.clone();
                                let engine = engine.clone();
                                std::thread::spawn(move || {
                                    std::thread::sleep(std::time::Duration::from_secs(3));
                                    ensure_virtual_sinks(&store, &presets, &engine);
                                });
                            }
                            _ => {}
                        }
                        pump.publish(ev);
                    }
                    tracing::warn!("patchbay event pump ended (engine gone)");
                })
                .expect("spawn patchbay-events thread");
        }
        // First connect emits no Reset — seed the virtual sinks once
        // the initial mirror is up.
        {
            let store = store.clone();
            let presets = presets.clone();
            let engine = engine.clone();
            std::thread::spawn(move || {
                std::thread::sleep(std::time::Duration::from_secs(3));
                ensure_virtual_sinks(&store, &presets, &engine);
            });
        }
        Self {
            inner: Arc::new(Inner {
                store,
                engine,
                events_hub,
                presets,
                dante: crate::dante_net::DanteEndpoints::default(),
                icons: crate::icons::IconCache::default(),
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

impl PatchbayBackend {
    /// Rewrite the WirePlumber drop-in from `rules` (blocking I/O +
    /// a pw-metadata read for the graph rate → off the executor).
    async fn write_latency_dropin(&self, rules: Vec<LatencyRule>) -> Result<(), PatchbayError> {
        tokio::task::spawn_blocking(move || {
            let rate = crate::clock::clock_info().rate;
            crate::latency::write_dropin(&rules, rate)
        })
        .await
        .map_err(|e| PatchbayError::Internal(e.to_string()))?
        .map_err(PatchbayError::Internal)
    }
}

use patchbay_proto::sink_node_name;

/// Create any persisted virtual sink that isn't in the live graph.
fn ensure_virtual_sinks(
    store: &Arc<RwLock<GraphStore>>,
    presets: &Arc<PresetStore>,
    engine: &EngineHandle,
) {
    for sink in presets.virtual_sinks() {
        let node_name = sink_node_name(&sink.name);
        let live = store.read().nodes.values().any(|n| n.name == node_name);
        if live {
            continue;
        }
        tracing::info!(name = %sink.name, "creating virtual sink");
        if let Err(e) = engine.send(Command::CreateVirtualSink {
            node_name,
            description: sink.name.clone(),
            channels: sink.channels.max(1),
        }) {
            tracing::warn!("virtual sink create failed: {e}");
        }
    }
}

/// Non-destructive chanmap import: name `node`'s channels from the
/// host's default ReaperChanMap, skipping channels the user already
/// aliased in the patchbay. Missing chanmap file = silently nothing.
fn auto_import_chanmap(
    store: &Arc<RwLock<GraphStore>>,
    presets: &Arc<PresetStore>,
    node: &str,
) {
    let Ok(names) = crate::chanmap::read_names("") else {
        return;
    };
    let ports: Vec<String> = {
        let s = store.read();
        let Some(n) = s.nodes.values().find(|n| n.name == node) else {
            return;
        };
        s.ports
            .values()
            .filter(|p| p.node_id == n.id)
            .map(|p| p.name.clone())
            .collect()
    };
    let mut written = 0u32;
    for port in ports {
        let Some(channel) = crate::chanmap::channel_of_port(&port) else {
            continue;
        };
        let target = format!("{node}:{port}");
        if presets.has_alias(&target) {
            continue;
        }
        if let Some(name) = names.get(&channel) {
            presets.set_alias(target, name.clone());
            written += 1;
        }
    }
    if written > 0 {
        tracing::info!(node, written, "auto-imported chanmap names");
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

    async fn connect_one_to_one(
        &self,
        output_node: String,
        input_node: String,
    ) -> Result<u32, PatchbayError> {
        // Pair by numeric suffix (out7 → playback_7). Non-numeric
        // ports don't participate.
        let pairs: Vec<(u32, u32)> = {
            let store = self.inner.store.read();
            let node_id = |name: &str| {
                store
                    .nodes
                    .values()
                    .find(|n| n.name == name)
                    .map(|n| n.id)
                    .ok_or_else(|| PatchbayError::not_found("node", name))
            };
            let out_id = node_id(&output_node)?;
            let in_id = node_id(&input_node)?;
            let by_channel = |node: u32, dir: patchbay_proto::PortDirection| {
                let mut m = std::collections::BTreeMap::new();
                for p in store.ports.values() {
                    if p.node_id == node && p.direction == dir {
                        if let Some(ch) = crate::chanmap::channel_of_port(&p.name) {
                            m.entry(ch).or_insert(p.id);
                        }
                    }
                }
                m
            };
            let outs = by_channel(out_id, patchbay_proto::PortDirection::Output);
            let ins = by_channel(in_id, patchbay_proto::PortDirection::Input);
            outs.into_iter()
                .filter_map(|(ch, out)| ins.get(&ch).map(|inp| (out, *inp)))
                .collect()
        };
        if pairs.is_empty() {
            return Err(PatchbayError::Internal(format!(
                "no numeric-suffix port pairs between {output_node} and {input_node}"
            )));
        }
        let mut created = 0;
        for (out, inp) in pairs {
            if self.create_link_inner(out, inp)? {
                created += 1;
            }
        }
        Ok(created)
    }

    async fn disconnect_nodes(
        &self,
        output_node: String,
        input_node: String,
    ) -> Result<u32, PatchbayError> {
        let link_ids: Vec<u32> = {
            let store = self.inner.store.read();
            let node_id = |name: &str| store.nodes.values().find(|n| n.name == name).map(|n| n.id);
            let (Some(out_id), Some(in_id)) = (node_id(&output_node), node_id(&input_node))
            else {
                return Err(PatchbayError::not_found(
                    "node",
                    format!("{output_node} or {input_node}"),
                ));
            };
            store
                .links
                .values()
                .filter(|l| l.output_node == out_id && l.input_node == in_id)
                .map(|l| l.id)
                .collect()
        };
        for id in &link_ids {
            self.inner
                .engine
                .send(Command::DestroyLink { id: *id })
                .map_err(PatchbayError::EngineUnavailable)?;
        }
        Ok(link_ids.len() as u32)
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

    async fn import_inferno_names(
        &self,
        node: String,
        device: String,
        direction: String,
    ) -> Result<u32, PatchbayError> {
        let want_rx = match direction.trim().to_lowercase().as_str() {
            "rx" | "in" | "input" | "capture" => true,
            "tx" | "out" | "output" | "playback" => false,
            other => {
                return Err(PatchbayError::Internal(format!(
                    "direction must be 'rx' or 'tx', got '{other}'"
                )));
            }
        };
        // Live ARC scan (mDNS + per-device channel query — seconds).
        let devices = self.inner.dante.network().await?;
        let dev = if device.trim().is_empty() {
            devices.first()
        } else {
            devices.iter().find(|d| d.name == device)
        }
        .ok_or_else(|| {
            PatchbayError::not_found(
                "dante device",
                if device.trim().is_empty() { "<any>" } else { &device },
            )
        })?;
        // channel number → name, from the chosen direction's channel list.
        let names: std::collections::BTreeMap<u32, String> =
            if want_rx { &dev.rx } else { &dev.tx }
                .iter()
                .filter(|c| !c.name.trim().is_empty())
                .map(|c| (c.number, c.name.clone()))
                .collect();
        // Match ports on `node` by numeric suffix (same as import_chanmap).
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

    async fn virtual_sinks(&self) -> Result<Vec<VirtualSink>, PatchbayError> {
        Ok(self.inner.presets.virtual_sinks())
    }

    async fn add_virtual_sink(&self, sink: VirtualSink) -> Result<(), PatchbayError> {
        if sink.name.trim().is_empty() {
            return Err(PatchbayError::Internal("sink name is empty".into()));
        }
        if !(1..=64).contains(&sink.channels) {
            return Err(PatchbayError::Internal(format!(
                "channel count {} out of range (1–64)",
                sink.channels
            )));
        }
        self.inner.presets.add_virtual_sink(sink);
        ensure_virtual_sinks(&self.inner.store, &self.inner.presets, &self.inner.engine);
        Ok(())
    }

    async fn remove_virtual_sink(&self, name: String) -> Result<(), PatchbayError> {
        if !self.inner.presets.remove_virtual_sink(&name) {
            return Err(PatchbayError::not_found("virtual sink", &name));
        }
        // Destroy the live node too — but ONLY if it carries the
        // patchbay.virtual tag (never an arbitrary node).
        let node_name = sink_node_name(&name);
        let live_id = self
            .inner
            .store
            .read()
            .nodes
            .values()
            .find(|n| n.name == node_name && n.virtual_sink)
            .map(|n| n.id);
        if let Some(id) = live_id {
            self.inner
                .engine
                .send(Command::DestroyNode { id })
                .map_err(PatchbayError::EngineUnavailable)?;
        }
        Ok(())
    }

    async fn colors(&self) -> Result<Vec<ColorEntry>, PatchbayError> {
        Ok(self.inner.presets.colors())
    }

    async fn set_color(&self, target: String, color: String) -> Result<(), PatchbayError> {
        self.inner.presets.set_color(target, color);
        Ok(())
    }

    async fn icons(&self, names: Vec<String>) -> Result<Vec<IconEntry>, PatchbayError> {
        // Disk lookups + reads — off the executor.
        let this = self.clone();
        tokio::task::spawn_blocking(move || {
            names
                .into_iter()
                .filter_map(|name| {
                    this.inner.icons.data_uri(&name).map(|data_uri| IconEntry {
                        icon_name: name,
                        data_uri,
                    })
                })
                .collect()
        })
        .await
        .map_err(|e| PatchbayError::Internal(e.to_string()))
    }

    async fn clock(&self) -> Result<ClockInfo, PatchbayError> {
        // Shells out — keep it off the async executor.
        tokio::task::spawn_blocking(crate::clock::clock_info)
            .await
            .map_err(|e| PatchbayError::Internal(e.to_string()))
    }

    async fn force_quantum(&self, frames: u32) -> Result<(), PatchbayError> {
        tokio::task::spawn_blocking(move || crate::clock::force_quantum(frames))
            .await
            .map_err(|e| PatchbayError::Internal(e.to_string()))
    }

    async fn clock_defaults(&self) -> Result<ClockDefaults, PatchbayError> {
        tokio::task::spawn_blocking(crate::clock::clock_defaults)
            .await
            .map_err(|e| PatchbayError::Internal(e.to_string()))
    }

    async fn set_clock_defaults(&self, defaults: ClockDefaults) -> Result<(), PatchbayError> {
        tokio::task::spawn_blocking(move || crate::clock::set_clock_defaults(defaults))
            .await
            .map_err(|e| PatchbayError::Internal(e.to_string()))?
            .map_err(PatchbayError::Internal)
    }

    async fn latency_rules(&self) -> Result<Vec<LatencyRule>, PatchbayError> {
        Ok(self.inner.presets.latency_rules())
    }

    async fn set_latency_rule(&self, rule: LatencyRule) -> Result<(), PatchbayError> {
        if !(16..=8192).contains(&rule.quantum) {
            return Err(PatchbayError::Internal(format!(
                "quantum {} out of range",
                rule.quantum
            )));
        }
        let rules = self.inner.presets.set_latency_rule(rule);
        self.write_latency_dropin(rules).await
    }

    async fn remove_latency_rule(&self, pattern: String) -> Result<(), PatchbayError> {
        let rules = self
            .inner
            .presets
            .remove_latency_rule(&pattern)
            .ok_or_else(|| PatchbayError::not_found("latency rule", &pattern))?;
        self.write_latency_dropin(rules).await
    }

    async fn dante_status(&self) -> Result<DanteStatus, PatchbayError> {
        tokio::task::spawn_blocking(crate::dante::status)
            .await
            .map_err(|e| PatchbayError::Internal(e.to_string()))
    }

    async fn set_dante(&self, on: bool) -> Result<(), PatchbayError> {
        tokio::task::spawn_blocking(move || crate::dante::set(on))
            .await
            .map_err(|e| PatchbayError::Internal(e.to_string()))?
            .map_err(PatchbayError::Internal)
    }

    async fn services(&self) -> Result<Vec<ServiceStatus>, PatchbayError> {
        tokio::task::spawn_blocking(crate::units::status_all)
            .await
            .map_err(|e| PatchbayError::Internal(e.to_string()))
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
