//! Hello World Extension - Demonstrates bidirectional RPC
//!
//! This extension:
//! - Implements GuestService (called by host)
//! - Calls HostService (callbacks to host)
//! - Uses run_extension! macro for all boilerplate

use hello_world_proto::{
    GuestService, GuestServiceDispatcher, GuestStatusResult, HostServiceClient, PingResult,
};
use extension_runtime::{run_extension, ConnectionHandle, Context};
use std::sync::Arc;
use tracing::{debug, info};

#[derive(Clone)]
pub struct GuestServiceImpl {
    start_time: std::time::Instant,
    handle: Arc<std::sync::OnceLock<ConnectionHandle>>,
}

impl GuestServiceImpl {
    fn new(handle: Arc<std::sync::OnceLock<ConnectionHandle>>) -> Self {
        Self {
            start_time: std::time::Instant::now(),
            handle,
        }
    }

    fn host_client(&self) -> HostServiceClient {
        HostServiceClient::new(self.handle.get().expect("handle not initialized").clone())
    }
}

impl GuestService for GuestServiceImpl {
    async fn ping(&self, _cx: &Context) -> PingResult {
        info!("Guest received ping from host");

        // Example: Call the DAW transport service through the host
        // This demonstrates extension-to-extension communication via host mediation
        let host = self.host_client();

        // Log that we're about to call the DAW service
        let _ = host
            .log_message("info".to_string(), "Calling DAW Transport.get_state()...".to_string())
            .await;

        // Forward a call to the "daw" service's get_state method
        // In a real implementation, you'd use the generated client or encode the call properly
        // For now, this shows the architecture - extensions call HostService.forward_call()
        // to reach other extensions
        info!("Extension-to-extension call pattern: hello-world → host → daw");

        PingResult::Pong {
            message: "pong from guest v7.0 (with DAW-agnostic architecture!)".to_string(),
        }
    }

    async fn get_status(&self, _cx: &Context) -> GuestStatusResult {
        let uptime = self.start_time.elapsed().as_secs();
        debug!("Guest status requested: uptime={}s", uptime);

        GuestStatusResult::Success {
            uptime_secs: uptime,
            events_processed: 0,
        }
    }
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    run_extension!("hello-world", |handle| {
        let service = GuestServiceImpl::new(handle);
        GuestServiceDispatcher::new(service)
    })
}
