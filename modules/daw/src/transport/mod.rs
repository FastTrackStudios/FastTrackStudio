mod actions;
mod application;
mod core;
mod infra;
#[cfg(not(target_arch = "wasm32"))]
pub mod mock;
pub mod service;

#[cfg(not(target_arch = "wasm32"))]
pub mod reactive;

pub use core::transport::{PlayState, RecordMode, Tempo, Transport};
pub use core::{TransportActions, TransportError};
#[cfg(not(target_arch = "wasm32"))]
pub use infra::create_transport_http_router;
#[cfg(all(not(target_arch = "wasm32"), feature = "iroh"))]
pub use infra::irpc::{
    SubscribeTransport, TransportStateProvider, TransportStreamApi, TransportStreamMessage,
    TransportStreamProtocol, TransportUpdateMessage as TransportStreamUpdateMessage,
};
#[cfg(not(target_arch = "wasm32"))]
pub use infra::stream::{
    TransportStream, TransportStreamBackend, TransportStreamError, TransportStreamReceiver,
    TransportUpdate, create_transport_stream_from_socket,
};
#[cfg(not(target_arch = "wasm32"))]
pub use mock::MockTransport;
#[cfg(all(not(target_arch = "wasm32"), feature = "iroh"))]
pub use reactive::irpc::{TransportApi, TransportProtocol, TransportUpdateMessage};
#[cfg(not(target_arch = "wasm32"))]
pub use reactive::{
    DefaultTransportReactiveService, EventStreamSubject as TransportEventStreamSubject,
    TransportReactiveService, TransportReactiveState, TransportStreams,
};
pub use service::{TransportCommand, TransportEvent, TransportService};
