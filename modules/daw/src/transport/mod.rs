mod application;
mod core;
mod infra;
pub mod reactive;

pub use core::transport::{PlayState, RecordMode, Tempo, Transport};
pub use core::{TransportActions, TransportError};
pub use infra::create_transport_http_router;
#[cfg(all(not(target_arch = "wasm32"), feature = "iroh"))]
pub use infra::irpc::{
    SubscribeTransport, TransportStateProvider, TransportStreamApi, TransportStreamMessage,
    TransportStreamProtocol, TransportUpdateMessage as TransportStreamUpdateMessage,
};
pub use infra::stream::{
    TransportStream, TransportStreamBackend, TransportStreamError, TransportStreamReceiver,
    TransportUpdate, create_transport_stream_from_socket,
};
#[cfg(feature = "iroh")]
pub use reactive::irpc::{TransportApi, TransportProtocol, TransportUpdateMessage};
pub use reactive::{
    DefaultTransportReactiveService, EventStreamSubject as TransportEventStreamSubject,
    TransportReactiveService, TransportReactiveState, TransportStreams,
};
