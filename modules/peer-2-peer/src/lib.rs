//! Peer-to-peer networking module
//!
//! Provides:
//! - iroh-based P2P networking with irpc and gossip
//! - REAPER Extension RPC API (using irpc over QUIC)

pub mod iroh_connection;
pub mod reaper_api;
