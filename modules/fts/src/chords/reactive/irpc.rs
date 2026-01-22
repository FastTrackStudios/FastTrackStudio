//! IRPC protocol for chart reactive service
//!
//! Exposes chart reactive streams over IRPC so they can be reactive over the network.
//! The Chart includes chords, sections, tempo, time signatures, and all other chart data.
//!
//! ## Web Viewer Protocol
//!
//! For web clients viewing charts via the browser:
//! - Client connects using endpoint ID from URL (64-char hex)
//! - On connect: Server sends `ChartStreamMessage::ChartUpdate` with full chart and beat positions
//! - During playback: Server streams `ChartStreamMessage::PlaybackPosition` at ~30Hz
//! - On chart change: Server sends new `ChartStreamMessage::ChartUpdate`
//!
//! The beat positions enable client-side cursor rendering without needing the full layout engine.

use crate::chords::reactive::ChordsReactiveService;
use facet::Facet;
use iroh::{Endpoint, EndpointAddr, protocol::ProtocolHandler};
use irpc::{Client, channel::mpsc, rpc::RemoteService, rpc_requests};
use irpc_iroh::{IrohLazyRemoteConnection, IrohProtocol};
use keyflow::Chart;
use serde::{Deserialize, Serialize};
use tokio::sync::broadcast;

// ============================================================================
// Chart Update Messages (existing)
// ============================================================================

/// Chart changed message
#[derive(Debug, Clone, Serialize, Deserialize, Facet)]
pub struct ChartChangedMessage {
    pub project_name: String,
    pub chart: Chart,
}

/// All chart update messages
#[derive(Debug, Clone, Serialize, Deserialize, Facet)]
#[repr(u8)]
pub enum ChartUpdateMessage {
    ChartChanged(ChartChangedMessage),
}

// ============================================================================
// Web Viewer Protocol Messages
// ============================================================================

/// Serializable beat position for cursor/highlight rendering on clients.
///
/// This mirrors `engraver::layout::chart::BeatPosition` but is serializable
/// for network transmission. Clients use this to render playback cursor
/// without needing the full layout engine.
#[derive(Debug, Clone, Serialize, Deserialize, Facet)]
pub struct SerializableBeatPosition {
    /// Page number (1-indexed).
    pub page: u32,
    /// System index on this page (0-indexed).
    pub system: usize,
    /// Global measure index (0-indexed across entire chart).
    pub measure: usize,
    /// Beat/segment index within measure (0-indexed).
    pub beat: usize,
    /// Absolute tick position from song start (for tempo-independent lookup).
    /// Uses 480 ticks per quarter note (MIDI standard).
    pub absolute_tick: i64,
    /// Duration in ticks.
    pub duration_ticks: i32,
    /// Absolute X position on page (in points).
    pub x: f64,
    /// Width of this beat segment (in points).
    pub width: f64,
    /// Y position of the staff top line (in points).
    pub staff_y: f64,
    /// Staff height (in points, for cursor rendering).
    pub staff_height: f64,
    /// Time in seconds from song start.
    pub time_start: f64,
    /// Time in seconds when this beat ends.
    pub time_end: f64,
    /// Glyph codepoint for the primary element (notehead/rest) for highlighting.
    pub glyph_codepoint: Option<char>,
    /// Size of the glyph in spatiums.
    pub glyph_size: f64,
}

impl SerializableBeatPosition {
    /// Check if a given absolute tick falls within this beat.
    #[must_use]
    pub fn contains_tick(&self, tick: i64) -> bool {
        tick >= self.absolute_tick && tick < self.absolute_tick + self.duration_ticks as i64
    }

    /// Get interpolated X position for an absolute tick within this beat.
    #[must_use]
    pub fn x_at_tick(&self, tick: i64) -> f64 {
        if self.duration_ticks <= 0 {
            return self.x;
        }
        let progress =
            ((tick - self.absolute_tick) as f64 / self.duration_ticks as f64).clamp(0.0, 1.0);
        self.x + self.width * progress
    }

    /// Check if a given time falls within this beat.
    #[must_use]
    pub fn contains_time(&self, time: f64) -> bool {
        time >= self.time_start && time < self.time_end
    }

    /// Get interpolated X position for a time within this beat.
    #[must_use]
    pub fn x_at_time(&self, time: f64) -> f64 {
        if self.time_end <= self.time_start {
            return self.x;
        }
        let progress =
            ((time - self.time_start) / (self.time_end - self.time_start)).clamp(0.0, 1.0);
        self.x + self.width * progress
    }
}

/// Page layout information for multi-page charts.
#[derive(Debug, Clone, Serialize, Deserialize, Facet)]
pub struct SerializablePageLayout {
    /// Page number (1-indexed).
    pub number: u32,
    /// Page width in points.
    pub width: f64,
    /// Page height in points.
    pub height: f64,
}

/// Chart data with playback information for web viewers.
///
/// Sent on initial connection and when the chart changes.
/// Contains everything needed for client-side layout and rendering.
#[derive(Debug, Clone, Serialize, Deserialize, Facet)]
pub struct ChartWithPlayback {
    /// Project/song name for display.
    pub project_name: String,
    /// The parsed chart data.
    pub chart: Chart,
    /// Pre-computed beat positions for cursor rendering.
    /// Sorted by absolute_tick for efficient binary search.
    pub beat_positions: Vec<SerializableBeatPosition>,
    /// Page layouts (dimensions for multi-page rendering).
    pub pages: Vec<SerializablePageLayout>,
    /// Current tempo in BPM.
    pub tempo: f64,
    /// Song start time in project seconds (for relative position calculation).
    pub song_start_seconds: f64,
    /// Song end time in project seconds.
    pub song_end_seconds: f64,
    /// Time signature numerator.
    pub time_sig_num: u8,
    /// Time signature denominator.
    pub time_sig_denom: u8,
}

impl ChartWithPlayback {
    /// Find the beat position at a given absolute tick.
    /// Uses binary search for efficiency.
    #[must_use]
    pub fn beat_at_tick(&self, tick: i64) -> Option<&SerializableBeatPosition> {
        let idx = self
            .beat_positions
            .partition_point(|b| b.absolute_tick + b.duration_ticks as i64 <= tick);
        self.beat_positions
            .get(idx)
            .filter(|b| b.contains_tick(tick))
    }

    /// Find the beat position at a given time.
    #[must_use]
    pub fn beat_at_time(&self, time: f64) -> Option<&SerializableBeatPosition> {
        let idx = self.beat_positions.partition_point(|b| b.time_end <= time);
        self.beat_positions
            .get(idx)
            .filter(|b| b.contains_time(time))
    }
}

/// Lightweight playback position update for streaming during playback.
///
/// Sent at ~30Hz during playback to update cursor position.
/// Small payload (~32 bytes) for minimal latency.
#[derive(Debug, Clone, Serialize, Deserialize, Facet)]
pub struct PlaybackPositionMessage {
    /// Current playback position in project seconds.
    pub position_seconds: f64,
    /// Whether playback is active.
    pub is_playing: bool,
    /// Whether playback is paused.
    pub is_paused: bool,
    /// Current tempo in BPM (may change during playback with tempo track).
    pub tempo: f64,
}

/// Stream messages for web chart viewer.
///
/// Clients subscribe and receive a continuous stream of these messages.
#[derive(Debug, Clone, Serialize, Deserialize, Facet)]
#[repr(u8)]
pub enum ChartStreamMessage {
    /// Full chart update with playback data (sent on connect and when chart changes).
    ChartUpdate(ChartWithPlayback),
    /// Lightweight playback position update (sent during playback at ~30Hz).
    PlaybackPosition(PlaybackPositionMessage),
    /// Chart was closed/unloaded.
    ChartClosed,
}

/// Request to subscribe to chart updates
#[derive(Debug, Serialize, Deserialize, Facet)]
pub struct SubscribeChart;

/// IRPC protocol for chart service
#[rpc_requests(message = ChartMessage)]
#[derive(Serialize, Deserialize, Debug, Facet)]
#[repr(u8)]
pub enum ChartProtocol {
    /// Subscribe to chart updates (server streaming)
    #[rpc(tx = mpsc::Sender<ChartUpdateMessage>)]
    SubscribeChart(SubscribeChart),
}

/// Chart API client
pub struct ChartApi {
    inner: Client<ChartProtocol>,
    pub(crate) handler_data: Option<(
        mpsc::Receiver<ChartMessage>,
        broadcast::Sender<ChartUpdateMessage>,
    )>,
}

impl ChartApi {
    pub const ALPN: &[u8] = b"irpc-iroh/chart/1";

    /// Create a new chart reactive API (server-side)
    ///
    /// Note: This currently uses ChordsReactiveService, but in the future
    /// we should create a ChartReactiveService that directly streams Chart updates.
    /// For now, we'll need to convert ChordsData to Chart or extend the service.
    pub fn spawn(_service: Box<dyn ChordsReactiveService>) -> Self {
        let (tx, rx) = mpsc::channel(16);

        // Create broadcast channel for chart stream
        let (chart_broadcast, _) = broadcast::channel(1000);

        // Subscribe to reactive streams synchronously (on current thread)
        // This subscription will forward to broadcast channel
        // TODO: When we have ChartReactiveService, subscribe to chart_changed instead
        // For now, we'll need to handle the conversion from ChordsData to Chart
        // or extend the service to also stream Chart updates
        {
            // Placeholder: We'll need to implement chart streaming
            // For now, this is a placeholder that will need to be updated
            // when we have Chart data available from the service
        }

        // Store channels for later spawning in tokio runtime
        let handler_data = (rx, chart_broadcast);

        ChartApi {
            inner: Client::local(tx),
            handler_data: Some(handler_data),
        }
    }

    /// Extract handler data for spawning in tokio runtime
    pub fn take_handler_data(
        &mut self,
    ) -> Option<(
        mpsc::Receiver<ChartMessage>,
        broadcast::Sender<ChartUpdateMessage>,
    )> {
        self.handler_data.take()
    }

    /// Connect to a remote chart service
    pub fn connect(
        endpoint: Endpoint,
        addr: impl Into<EndpointAddr>,
    ) -> Result<Self, anyhow::Error> {
        let conn = IrohLazyRemoteConnection::new(endpoint, addr.into(), Self::ALPN.to_vec());
        Ok(ChartApi {
            inner: Client::boxed(conn),
            handler_data: None,
        })
    }

    /// Expose this service as a protocol handler for IROH router
    pub fn expose(&self) -> Result<impl ProtocolHandler, anyhow::Error> {
        use anyhow::Context;
        let local = self
            .inner
            .as_local()
            .context("cannot listen on remote service")?;
        Ok(IrohProtocol::new(ChartProtocol::remote_handler(local)))
    }

    /// Subscribe to chart updates
    pub async fn subscribe(&self) -> irpc::Result<mpsc::Receiver<ChartUpdateMessage>> {
        self.inner.server_streaming(SubscribeChart, 32).await
    }
}

// ============================================================================
// Chart Stream Protocol (for Web Viewers with Playback)
// ============================================================================

/// Request to subscribe to chart stream with playback position updates.
///
/// This is used by web viewers to receive both chart data and real-time
/// playback position updates for cursor rendering.
#[derive(Debug, Serialize, Deserialize, Facet)]
pub struct SubscribeChartStream;

/// IRPC protocol for chart stream service (web viewers).
///
/// Separate from ChartProtocol to support playback position streaming.
#[rpc_requests(message = ChartStreamMessageWrapper)]
#[derive(Serialize, Deserialize, Debug, Facet)]
#[repr(u8)]
pub enum ChartStreamProtocol {
    /// Subscribe to chart stream with playback (server streaming).
    #[rpc(tx = mpsc::Sender<ChartStreamMessage>)]
    SubscribeChartStream(SubscribeChartStream),
}

/// Chart Stream API for web viewer clients.
///
/// Provides access to chart data with real-time playback position updates.
pub struct ChartStreamApi {
    inner: Client<ChartStreamProtocol>,
    pub(crate) handler_data: Option<(
        mpsc::Receiver<ChartStreamMessageWrapper>,
        broadcast::Sender<ChartStreamMessage>,
    )>,
}

impl ChartStreamApi {
    /// ALPN protocol identifier for chart stream service.
    pub const ALPN: &[u8] = b"irpc-iroh/chart-stream/1";

    /// Create a new chart stream API (server-side).
    pub fn spawn() -> Self {
        let (tx, rx) = mpsc::channel(16);

        // Create broadcast channel for chart stream with playback
        // Buffer size of 100 handles ~3 seconds of position updates at 30Hz
        let (stream_broadcast, _) = broadcast::channel(100);

        // Store channels for later spawning in tokio runtime
        let handler_data = (rx, stream_broadcast);

        ChartStreamApi {
            inner: Client::local(tx),
            handler_data: Some(handler_data),
        }
    }

    /// Extract handler data for spawning in tokio runtime.
    pub fn take_handler_data(
        &mut self,
    ) -> Option<(
        mpsc::Receiver<ChartStreamMessageWrapper>,
        broadcast::Sender<ChartStreamMessage>,
    )> {
        self.handler_data.take()
    }

    /// Get a sender to broadcast chart stream messages.
    ///
    /// Used by the server to send chart updates and playback positions.
    pub fn broadcast_sender(&self) -> Option<broadcast::Sender<ChartStreamMessage>> {
        self.handler_data.as_ref().map(|(_, sender)| sender.clone())
    }

    /// Connect to a remote chart stream service (client-side).
    ///
    /// Used by web viewers to connect to REAPER extension.
    pub fn connect(
        endpoint: Endpoint,
        addr: impl Into<EndpointAddr>,
    ) -> Result<Self, anyhow::Error> {
        let conn = IrohLazyRemoteConnection::new(endpoint, addr.into(), Self::ALPN.to_vec());
        Ok(ChartStreamApi {
            inner: Client::boxed(conn),
            handler_data: None,
        })
    }

    /// Expose this service as a protocol handler for IROH router.
    pub fn expose(&self) -> Result<impl ProtocolHandler, anyhow::Error> {
        use anyhow::Context;
        let local = self
            .inner
            .as_local()
            .context("cannot listen on remote service")?;
        Ok(IrohProtocol::new(ChartStreamProtocol::remote_handler(
            local,
        )))
    }

    /// Subscribe to chart stream with playback updates.
    ///
    /// Returns a receiver that yields:
    /// - `ChartStreamMessage::ChartUpdate` on connect and when chart changes
    /// - `ChartStreamMessage::PlaybackPosition` during playback (~30Hz)
    /// - `ChartStreamMessage::ChartClosed` when chart is unloaded
    pub async fn subscribe(&self) -> irpc::Result<mpsc::Receiver<ChartStreamMessage>> {
        self.inner.server_streaming(SubscribeChartStream, 64).await
    }
}
