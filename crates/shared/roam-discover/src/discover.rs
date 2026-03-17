//! Peer discovery via mDNS browsing.

use std::collections::HashMap;

use mdns_sd::{ServiceDaemon, ServiceEvent};
use tokio::sync::mpsc;
use tracing::{debug, info, warn};

use crate::types::{to_dns_sd_type, Peer, PeerEvent};

/// Start discovering roam services of the given type on the local network.
///
/// Returns a receiver that yields [`PeerEvent`]s as peers appear and disappear.
/// Discovery runs in a background thread (mDNS daemon) and forwards events
/// through a tokio channel.
///
/// The discovery stops when the returned receiver is dropped.
pub async fn discover(service_type: &str) -> mpsc::Receiver<PeerEvent> {
    let (tx, rx) = mpsc::channel(64);
    let dns_sd_type = to_dns_sd_type(service_type);
    let service_type_owned = service_type.to_string();

    // Spawn the mDNS browse loop on a blocking thread (mdns-sd uses its own threads)
    let dns_sd_type_clone = dns_sd_type.clone();
    std::thread::Builder::new()
        .name(format!("mdns-discover-{}", service_type))
        .spawn(move || {
            let daemon = match ServiceDaemon::new() {
                Ok(d) => d,
                Err(e) => {
                    warn!("Failed to create mDNS daemon for discovery: {}", e);
                    return;
                }
            };

            let browse_rx = match daemon.browse(&dns_sd_type_clone) {
                Ok(rx) => rx,
                Err(e) => {
                    warn!(
                        "Failed to start mDNS browse for {}: {}",
                        dns_sd_type_clone, e
                    );
                    return;
                }
            };

            info!(
                service_type = %service_type_owned,
                dns_sd = %dns_sd_type_clone,
                "mDNS discovery started"
            );

            while let Ok(event) = browse_rx.recv() {
                match event {
                    ServiceEvent::ServiceResolved(info) => {
                        let mut metadata = HashMap::new();
                        for prop in info.get_properties().iter() {
                            let val = prop.val_str();
                            metadata.insert(prop.key().to_string(), val.to_string());
                        }

                        let peer = Peer {
                            instance_name: info.get_fullname().to_string(),
                            service_type: info.get_type().to_string(),
                            host: info.get_hostname().to_string(),
                            addresses: info.get_addresses().iter().copied().collect(),
                            port: info.get_port(),
                            metadata,
                        };

                        debug!(
                            instance = %peer.instance_name,
                            host = %peer.host,
                            port = peer.port,
                            addrs = ?peer.addresses,
                            "Peer found"
                        );

                        if tx.blocking_send(PeerEvent::Found(peer)).is_err() {
                            debug!("Discovery receiver dropped, stopping browse");
                            break;
                        }
                    }
                    ServiceEvent::ServiceRemoved(_, fullname) => {
                        debug!(fullname = %fullname, "Peer lost");
                        if tx.blocking_send(PeerEvent::Lost(fullname)).is_err() {
                            break;
                        }
                    }
                    ServiceEvent::SearchStarted(_) => {
                        debug!("mDNS browse search started");
                    }
                    _ => {}
                }
            }

            debug!("mDNS discovery loop exiting");
            let _ = daemon.shutdown();
        })
        .expect("failed to spawn mDNS discovery thread");

    rx
}
