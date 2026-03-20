//! Service advertisement via mDNS.

use mdns_sd::{ServiceDaemon, ServiceInfo as MdnsServiceInfo};
use tracing::{debug, info};

use crate::types::{to_dns_sd_type, ServiceInfo};
use crate::{Error, Result};

/// Advertise a roam service on the local network via mDNS.
///
/// Returns an [`AdvertiseGuard`] that keeps the advertisement alive.
/// When dropped, the service is unregistered from the network.
pub fn advertise(info: ServiceInfo) -> Result<AdvertiseGuard> {
    let service_type = to_dns_sd_type(info.service_type);

    // Build TXT record properties as HashMap
    let properties: std::collections::HashMap<String, String> =
        info.metadata.iter().cloned().collect();

    // mdns-sd requires hostnames to end with ".local."
    let hostname = format!("{}.local.", info.instance_name);
    let mdns_info = MdnsServiceInfo::new(
        &service_type,
        &info.instance_name,
        &hostname,
        "",  // empty = auto-detect IP
        info.port,
        properties,
    )
    .map_err(Error::Mdns)?;

    let daemon = ServiceDaemon::new().map_err(Error::Mdns)?;
    let fullname = mdns_info.get_fullname().to_string();

    daemon.register(mdns_info).map_err(Error::Mdns)?;

    info!(
        service_type = info.service_type,
        instance = %info.instance_name,
        port = info.port,
        "mDNS service advertised"
    );

    Ok(AdvertiseGuard {
        daemon,
        fullname,
        service_type: info.service_type,
        instance_name: info.instance_name,
    })
}

/// RAII guard that keeps a service advertised on the network.
///
/// Unregisters the service when dropped.
pub struct AdvertiseGuard {
    daemon: ServiceDaemon,
    fullname: String,
    service_type: &'static str,
    instance_name: String,
}

impl Drop for AdvertiseGuard {
    fn drop(&mut self) {
        debug!(
            service_type = self.service_type,
            instance = %self.instance_name,
            "Unregistering mDNS service"
        );
        let _ = self.daemon.unregister(&self.fullname);
        let _ = self.daemon.shutdown();
    }
}
