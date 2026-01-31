//! DAW Standalone Extension - Transport Only
//!
//! Run this as a standalone extension for testing without REAPER.

use daw_proto::*;
use daw_standalone::DawStandaloneTransport;
use extension_runtime::{run_extension, Context};

// Re-export the transport service so the extension can use it
use std::sync::Arc;
use tracing::info;

#[derive(Clone)]
struct TransportImpl {
    inner: DawStandaloneTransport,
}

impl TransportImpl {
    fn new() -> Self {
        Self {
            inner: DawStandaloneTransport::new(),
        }
    }
}

impl Transport for TransportImpl {
    async fn play(&self, cx: &Context) -> TransportResult {
        info!("Extension: play");
        self.inner.play(cx).await
    }

    async fn stop(&self, cx: &Context) -> TransportResult {
        info!("Extension: stop");
        self.inner.stop(cx).await
    }

    async fn pause(&self, cx: &Context) -> TransportResult {
        info!("Extension: pause");
        self.inner.pause(cx).await
    }

    async fn record(&self, cx: &Context) -> TransportResult {
        info!("Extension: record");
        self.inner.record(cx).await
    }

    async fn get_state(&self, cx: &Context) -> PlaybackState {
        self.inner.get_state(cx).await
    }

    async fn set_position(&self, cx: &Context, seconds: f64) -> TransportResult {
        info!("Extension: set_position");
        self.inner.set_position(cx, seconds).await
    }
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    run_extension!("daw-standalone", |_handle| {
        TransportDispatcher::new(TransportImpl::new())
    })
}
