//! The PipeWire main-loop thread.
//!
//! Helvum's engine design, headless: one OS thread owns the PipeWire
//! `MainLoop`; a registry listener mirrors nodes/ports/links into the
//! shared [`GraphStore`] and emits [`GraphEvent`]s; inbound commands
//! (create/destroy link) arrive over `pipewire::channel`, which is the
//! only channel type that can wake the main loop.
//!
//! Links are created via the `link-factory` with `object.linger` so the
//! wiring survives this process — the patchbay edits the system's graph,
//! it doesn't own it.

use std::sync::Arc;
use std::sync::mpsc::Sender;

use parking_lot::RwLock;
use patchbay_proto::GraphEvent;

use crate::store::GraphStore;

/// Commands the service sends into the PipeWire thread.
pub(crate) enum Command {
    /// All four ids resolved by the service from the store (the link
    /// factory wants node AND port ids on both ends).
    CreateLink {
        output_node: u32,
        output_port: u32,
        input_node: u32,
        input_port: u32,
    },
    DestroyLink {
        id: u32,
    },
    #[allow(dead_code)]
    Terminate,
}

pub(crate) struct EngineHandle {
    #[cfg(target_os = "linux")]
    cmd_tx: pipewire::channel::Sender<Command>,
}

impl EngineHandle {
    pub fn send(&self, cmd: Command) -> Result<(), String> {
        #[cfg(target_os = "linux")]
        {
            self.cmd_tx
                .send(cmd)
                .map_err(|_| "pipewire engine thread is gone".to_string())
        }
        #[cfg(not(target_os = "linux"))]
        {
            let _ = cmd;
            Err("pipewire is linux-only".to_string())
        }
    }
}

/// Spawn the engine thread. Returns immediately; if the PipeWire
/// connection fails the thread logs and exits, and later commands
/// error with "engine thread is gone".
pub(crate) fn spawn(
    store: Arc<RwLock<GraphStore>>,
    events: Sender<GraphEvent>,
) -> EngineHandle {
    #[cfg(target_os = "linux")]
    {
        let (cmd_tx, cmd_rx) = pipewire::channel::channel::<Command>();
        std::thread::Builder::new()
            .name("patchbay-pw".into())
            .spawn(move || {
                if let Err(e) = linux::thread_main(store, events, cmd_rx) {
                    tracing::error!("pipewire engine thread died: {e}");
                }
            })
            .expect("spawn patchbay-pw thread");
        EngineHandle { cmd_tx }
    }
    #[cfg(not(target_os = "linux"))]
    {
        let _ = (store, events);
        EngineHandle {}
    }
}

#[cfg(target_os = "linux")]
mod linux {
    use std::cell::RefCell;
    use std::collections::HashMap;
    use std::rc::Rc;
    use std::sync::Arc;
    use std::sync::mpsc::Sender;

    use parking_lot::RwLock;
    use pipewire::link::{Link, LinkChangeMask, LinkListener, LinkState};
    use pipewire::properties::properties;
    use pipewire::registry::GlobalObject;
    use pipewire::types::ObjectType;
    use pipewire::{context::ContextRc, main_loop::MainLoopRc};

    use patchbay_proto::{GraphEvent, MediaKind, PortDirection, PwLink, PwNode, PwPort};

    use super::Command;
    use crate::store::GraphStore;

    /// Link proxies + their info listeners must stay alive to keep
    /// receiving state changes (helvum's `proxies` map).
    struct LinkProxy {
        _proxy: Link,
        _listener: LinkListener,
    }

    pub(super) fn thread_main(
        store: Arc<RwLock<GraphStore>>,
        events: Sender<GraphEvent>,
        cmd_rx: pipewire::channel::Receiver<Command>,
    ) -> Result<(), pipewire::Error> {
        let mainloop = MainLoopRc::new(None)?;
        let context = ContextRc::new(&mainloop, None)?;
        let core = context.connect_rc(None)?;
        let registry = core.get_registry_rc()?;

        let link_proxies: Rc<RefCell<HashMap<u32, LinkProxy>>> =
            Rc::new(RefCell::new(HashMap::new()));

        // ── Inbound commands ────────────────────────────────────────
        let _cmd_receiver = {
            let core = core.clone();
            let registry = registry.clone();
            let mainloop_quit = mainloop.clone();
            cmd_rx.attach(mainloop.loop_(), move |cmd| match cmd {
                Command::CreateLink {
                    output_node,
                    output_port,
                    input_node,
                    input_port,
                } => {
                    let res = core.create_object::<Link>(
                        "link-factory",
                        &properties! {
                            "link.output.node" => output_node.to_string(),
                            "link.output.port" => output_port.to_string(),
                            "link.input.node" => input_node.to_string(),
                            "link.input.port" => input_port.to_string(),
                            // Survive this app exiting — the patchbay
                            // edits the system graph, it doesn't own it.
                            "object.linger" => "1",
                        },
                    );
                    if let Err(e) = res {
                        tracing::warn!(
                            output_port,
                            input_port,
                            "link-factory create failed: {e}"
                        );
                    }
                }
                Command::DestroyLink { id } => {
                    registry.destroy_global(id).into_result().map(|_| ()).unwrap_or_else(|e| {
                        tracing::warn!(id, "destroy_global failed: {e}");
                    });
                }
                Command::Terminate => mainloop_quit.quit(),
            })
        };

        // ── Registry listener: mirror the graph ─────────────────────
        let _registry_listener = {
            let store_add = store.clone();
            let events_add = events.clone();
            let registry_bind = registry.clone();
            let proxies_add = link_proxies.clone();
            let store_rm = store.clone();
            let events_rm = events.clone();
            let proxies_rm = link_proxies.clone();
            registry
                .add_listener_local()
                .global(move |global| match global.type_ {
                    ObjectType::Node => handle_node(global, &store_add, &events_add),
                    ObjectType::Port => handle_port(global, &store_add, &events_add),
                    ObjectType::Link => handle_link(
                        global,
                        &registry_bind,
                        &proxies_add,
                        &store_add,
                        &events_add,
                    ),
                    _ => {}
                })
                .global_remove(move |id| {
                    let removed = {
                        let mut s = store_rm.write();
                        if let Some(n) = s.nodes.remove(&id) {
                            let _ = n;
                            Some(GraphEvent::NodeRemoved { id })
                        } else if let Some(p) = s.ports.remove(&id) {
                            Some(GraphEvent::PortRemoved {
                                id,
                                node_id: p.node_id,
                            })
                        } else if s.links.remove(&id).is_some() {
                            Some(GraphEvent::LinkRemoved { id })
                        } else {
                            None
                        }
                    };
                    proxies_rm.borrow_mut().remove(&id);
                    if let Some(ev) = removed {
                        let _ = events_rm.send(ev);
                    }
                })
                .register()
        };

        mainloop.run();
        Ok(())
    }

    fn media_kind(media_class: &str) -> MediaKind {
        if media_class.contains("Audio") {
            MediaKind::Audio
        } else if media_class.contains("Video") {
            MediaKind::Video
        } else if media_class.contains("Midi") {
            MediaKind::Midi
        } else {
            MediaKind::Other
        }
    }

    fn handle_node(
        global: &GlobalObject<&pipewire::spa::utils::dict::DictRef>,
        store: &Arc<RwLock<GraphStore>>,
        events: &Sender<GraphEvent>,
    ) {
        let Some(props) = global.props else { return };
        let name = props.get("node.name").unwrap_or_default().to_string();
        let label = props
            .get("node.nick")
            .or_else(|| props.get("node.description"))
            .or_else(|| props.get("node.name"))
            .unwrap_or_default()
            .to_string();
        let media_class = props.get("media.class").unwrap_or_default().to_string();
        let node = PwNode {
            id: global.id,
            name,
            label,
            media_kind: media_kind(&media_class),
            media_class,
        };
        store.write().nodes.insert(global.id, node.clone());
        let _ = events.send(GraphEvent::NodeAdded(node));
    }

    fn handle_port(
        global: &GlobalObject<&pipewire::spa::utils::dict::DictRef>,
        store: &Arc<RwLock<GraphStore>>,
        events: &Sender<GraphEvent>,
    ) {
        let Some(props) = global.props else { return };
        let Some(node_id) = props.get("node.id").and_then(|s| s.parse::<u32>().ok()) else {
            return;
        };
        let direction = match props.get("port.direction") {
            Some("in") => PortDirection::Input,
            _ => PortDirection::Output,
        };
        // Ports inherit their node's media kind (helvum's approach);
        // `format.dsp` refines MIDI ports on audio nodes.
        let node_kind = store
            .read()
            .nodes
            .get(&node_id)
            .map(|n| n.media_kind)
            .unwrap_or(MediaKind::Other);
        let kind = match props.get("format.dsp") {
            Some(f) if f.contains("midi") => MediaKind::Midi,
            Some(f) if f.contains("audio") => MediaKind::Audio,
            _ => node_kind,
        };
        let port = PwPort {
            id: global.id,
            node_id,
            name: props.get("port.name").unwrap_or_default().to_string(),
            direction,
            media_kind: kind,
        };
        store.write().ports.insert(global.id, port.clone());
        let _ = events.send(GraphEvent::PortAdded(port));
    }

    fn handle_link(
        global: &GlobalObject<&pipewire::spa::utils::dict::DictRef>,
        registry: &pipewire::registry::RegistryRc,
        proxies: &Rc<RefCell<HashMap<u32, LinkProxy>>>,
        store: &Arc<RwLock<GraphStore>>,
        events: &Sender<GraphEvent>,
    ) {
        // Endpoints aren't on the raw global — bind a proxy and wait for
        // the first info event (helvum's pattern). The listener also
        // tracks later active/inactive flips.
        let proxy: Link = match registry.bind(global) {
            Ok(p) => p,
            Err(e) => {
                tracing::warn!(id = global.id, "bind link failed: {e}");
                return;
            }
        };
        let store = store.clone();
        let events = events.clone();
        let listener = proxy
            .add_listener_local()
            .info(move |info| {
                let id = info.id();
                let active = matches!(info.state(), LinkState::Active);
                let known = store.read().links.contains_key(&id);
                if known {
                    if info.change_mask().contains(LinkChangeMask::STATE) {
                        if let Some(l) = store.write().links.get_mut(&id) {
                            l.active = active;
                        }
                        let _ = events.send(GraphEvent::LinkStateChanged { id, active });
                    }
                } else {
                    let link = PwLink {
                        id,
                        output_node: info.output_node_id(),
                        output_port: info.output_port_id(),
                        input_node: info.input_node_id(),
                        input_port: info.input_port_id(),
                        active,
                    };
                    store.write().links.insert(id, link.clone());
                    let _ = events.send(GraphEvent::LinkAdded(link));
                }
            })
            .register();
        proxies.borrow_mut().insert(
            global.id,
            LinkProxy {
                _proxy: proxy,
                _listener: listener,
            },
        );
    }
}
