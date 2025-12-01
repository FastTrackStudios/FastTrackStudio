//! IRPC protocol for chords reactive service
//!
//! Exposes chords reactive streams over IRPC so they can be reactive over the network.

use crate::chords::types::ChordsData;
use crate::chords::reactive::ChordsReactiveService;
use irpc::{
    channel::mpsc,
    rpc::RemoteService,
    rpc_requests, Client, WithChannels,
};
use serde::{Deserialize, Serialize};
use iroh::{protocol::ProtocolHandler, Endpoint, EndpointAddr};
use irpc_iroh::{IrohLazyRemoteConnection, IrohProtocol};
use rxrust::prelude::*;
use tokio::sync::broadcast;

/// Chords changed message
/// Note: ChordsData cannot be serialized because it contains keyflow types that don't implement Serialize
#[derive(Debug, Clone)]
pub struct ChordsChangedMessage {
    pub project_name: String,
    pub chords: ChordsData,
}

/// All chords update messages
/// Note: Cannot use Serialize/Deserialize because ChordsData contains keyflow types
#[derive(Debug, Clone)]
pub enum ChordsUpdateMessage {
    ChordsChanged(ChordsChangedMessage),
}

/// Request to subscribe to chords updates
#[derive(Debug, Serialize, Deserialize)]
pub struct SubscribeChords;

/// IRPC protocol for chords service
/// 
/// NOTE: Currently disabled because ChordsData contains keyflow types (Chord, MusicalNote, Key)
/// that don't implement Serialize/Deserialize. To enable IRPC, we need to create a serializable
/// representation of ChordsData (e.g., convert to strings or custom serializable structs).
/// 
/// For now, chords updates are only available through in-process reactive streams.
// FIXME: Create serializable representation of ChordsData for IRPC
// #[rpc_requests(message = ChordsMessage)]
// #[derive(Serialize, Deserialize, Debug)]
// pub enum ChordsProtocol {
//     /// Subscribe to chords updates (server streaming)
//     #[rpc(tx = mpsc::Sender<ChordsUpdateMessage>)]
//     SubscribeChords(SubscribeChords),
// }

/// Chords API client
/// 
/// NOTE: Currently disabled - see ChordsProtocol comment above.
/// This struct is kept for future implementation when serializable representation is available.
pub struct ChordsApi {
    // inner: Client<ChordsProtocol>,  // Disabled until serialization is available
    pub(crate) handler_data: Option<(mpsc::Receiver<()>, broadcast::Sender<ChordsUpdateMessage>)>,  // Placeholder
}

impl ChordsApi {
    pub const ALPN: &[u8] = b"irpc-iroh/chords/1";

    /// Create a new chords reactive API (server-side)
    /// 
    /// NOTE: Currently creates a minimal implementation that only works in-process.
    /// IRPC serialization is disabled until ChordsData can be serialized.
    pub fn spawn(service: Box<dyn ChordsReactiveService>) -> Self {
        let (_tx, rx) = mpsc::channel(16);
        let streams = service.streams().clone();

        // Create broadcast channel for chords stream
        let (chords_broadcast, _) = broadcast::channel(1000);

        // Subscribe to reactive streams synchronously (on current thread)
        // This subscription will forward to broadcast channel
        {
            let chords_subject = streams.chords_changed.borrow().clone();
            let tx = chords_broadcast.clone();
            chords_subject.subscribe(move |(project_name, chords)| {
                let _ = tx.send(ChordsUpdateMessage::ChordsChanged(ChordsChangedMessage {
                    project_name,
                    chords,
                }));
            });
        }

        // Store channels for later spawning in tokio runtime
        let handler_data = (rx, chords_broadcast);

        ChordsApi {
            handler_data: Some(handler_data),
        }
    }

    /// Extract handler data for spawning in tokio runtime
    pub fn take_handler_data(&mut self) -> Option<(mpsc::Receiver<()>, broadcast::Sender<ChordsUpdateMessage>)> {
        self.handler_data.take()
    }

    // NOTE: connect() and expose() are disabled until serialization is available
    // pub fn connect(...) { ... }
    // pub fn expose(...) { ... }
    // pub async fn subscribe(...) { ... }
}

