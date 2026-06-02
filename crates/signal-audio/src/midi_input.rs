//! System MIDI input — connects to all available MIDI ports and forwards
//! note-on / note-off events to a crossbeam channel.
//!
//! # Usage
//!
//! ```rust,no_run
//! let midi = MidiInput::open_all().expect("no MIDI ports");
//! // Provide via Dioxus context so the sampler view can read events.
//! // In the UI component:
//! let rx = use_context::<MidiInput>();
//! while let Ok(ev) = rx.try_recv() { ... }
//! ```

use crossbeam_channel::{Receiver, Sender, bounded};
use midir::{MidiInput as MidirInput, MidiInputConnection};

/// A MIDI event: (status, note, velocity).
/// - note-on:  status = 0x90 | channel
/// - note-off: status = 0x80 | channel
#[derive(Clone, Copy, Debug)]
pub struct MidiEvent {
    pub status: u8,
    pub note: u8,
    pub velocity: u8,
}

impl MidiEvent {
    pub fn is_note_on(self) -> bool {
        (self.status & 0xF0) == 0x90 && self.velocity > 0
    }
    pub fn is_note_off(self) -> bool {
        (self.status & 0xF0) == 0x80 || ((self.status & 0xF0) == 0x90 && self.velocity == 0)
    }
    /// Control Change (status 0xB0). For CC, data byte 1 (`note`) is the
    /// controller number and data byte 2 (`velocity`) is its value — e.g.
    /// CC64 is the sustain (damper) pedal.
    pub fn is_cc(self) -> bool {
        (self.status & 0xF0) == 0xB0
    }
    /// Controller number for a CC message (data byte 1).
    pub fn controller(self) -> u8 {
        self.note
    }
    /// Controller value for a CC message (data byte 2).
    pub fn cc_value(self) -> u8 {
        self.velocity
    }
}

/// Holds open MIDI input connections and exposes a receiver channel.
///
/// Clone to share the receiver across components — the channel is
/// multi-producer / single-consumer via crossbeam.  Cloning [`MidiInput`]
/// only clones the [`Receiver`] handle; the underlying connection stays alive
/// as long as the original [`MidiInput`] exists.  Keep it in a top-level
/// signal or Dioxus context to ensure it lives for the app lifetime.
#[derive(Clone)]
pub struct MidiInput {
    rx: Receiver<MidiEvent>,
    // Kept alive by being held in the struct; dropped when MidiInput is dropped.
    _connections: std::sync::Arc<Vec<MidiInputConnection<()>>>,
}

impl MidiInput {
    /// Open all available MIDI input ports and start forwarding events.
    /// Returns `None` if no MIDI input ports are available.
    pub fn open_all() -> Option<Self> {
        let midi_in = MidirInput::new("signal-desktop").ok()?;
        let ports = midi_in.ports();
        if ports.is_empty() {
            return None;
        }

        let (tx, rx) = bounded::<MidiEvent>(256);
        let mut connections = Vec::new();

        for port in &ports {
            let port_name = midi_in.port_name(port).unwrap_or_else(|_| "unknown".into());
            tracing::info!("MIDI: opening port \"{}\"", port_name);

            // Each port needs its own MidirInput handle.
            let Ok(inp) = MidirInput::new("signal-desktop") else {
                continue;
            };
            let tx2: Sender<MidiEvent> = tx.clone();

            match inp.connect(
                port,
                "signal-rx",
                move |_ts, msg, _| {
                    if msg.len() >= 3 {
                        let ev = MidiEvent {
                            status: msg[0],
                            note: msg[1],
                            velocity: msg[2],
                        };
                        let _ = tx2.try_send(ev);
                    }
                },
                (),
            ) {
                Ok(conn) => connections.push(conn),
                Err(e) => tracing::warn!("MIDI: failed to open \"{}\": {}", port_name, e),
            }
        }

        if connections.is_empty() {
            return None;
        }

        Some(Self {
            rx,
            _connections: std::sync::Arc::new(connections),
        })
    }

    /// Non-blocking drain of all pending MIDI events.
    pub fn drain(&self) -> impl Iterator<Item = MidiEvent> + '_ {
        self.rx.try_iter()
    }
}
