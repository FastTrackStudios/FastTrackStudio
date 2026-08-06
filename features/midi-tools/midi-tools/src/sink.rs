//! Where a velocity edit goes.
//!
//! The panel knows what shape you dialled in; it deliberately does not
//! know how to reach a DAW. That's this seam.
//!
//! The trait lives here rather than in the UI crate — as chord-tool has
//! it — so that `midi-tools-ui` and `midi-tools-daw` are siblings that
//! both depend on this crate and not on each other. A DAW backend has no
//! business linking Dioxus to find out what a sink is.

use crate::velocity::Session;

/// A take that velocity edits can be read from and written to.
pub trait VelocitySink: Send + Sync + 'static {
    /// Bind to the user's current target and read its notes into a fresh
    /// session.
    ///
    /// Returns a message on failure rather than an error type: the only
    /// consumer is a status line, and a panel should say "select a MIDI
    /// item first" rather than swallow it.
    fn open(&self) -> Result<Session, String>;

    /// Push the session's result to the take. Returns how many notes moved.
    fn commit(&self, session: &Session) -> Result<usize, String>;

    /// Put the take back exactly as [`VelocitySink::open`] found it.
    fn revert(&self, session: &Session) -> Result<usize, String>;

    /// Re-read the bound take into `session`, keeping its parameters.
    fn resync(&self, session: &mut Session) -> Result<(), String>;
}

/// The no-DAW default: reports what *would* happen.
///
/// Used by the standalone example, and by the panel when no sink is
/// provided. It hands back a synthetic take so the panel has something
/// to shape — a velocity tool with no notes in it is impossible to
/// iterate on, and every control would be greyed out.
pub struct DemoSink {
    notes: Vec<crate::velocity::Note>,
}

impl Default for DemoSink {
    fn default() -> Self {
        Self::sixteenths(32)
    }
}

impl DemoSink {
    /// `count` notes with a plausible played-hi-hat velocity shape:
    /// accented downbeats, a little drift, nothing perfectly even.
    pub fn sixteenths(count: usize) -> Self {
        let notes = (0..count)
            .map(|i| {
                let accent = match i % 4 {
                    0 => 104,
                    2 => 88,
                    _ => 68,
                };
                // A slow sway, so the demo take isn't a repeating
                // sawtooth that makes every engine look like it works.
                let sway = ((i as f64) * 0.4).sin() * 9.0;
                crate::velocity::Note::new(i as u32, (f64::from(accent) + sway).round() as u8)
            })
            .collect();
        Self { notes }
    }
}

impl VelocitySink for DemoSink {
    fn open(&self) -> Result<Session, String> {
        Ok(Session::new(self.notes.clone()))
    }

    fn commit(&self, session: &Session) -> Result<usize, String> {
        let n = session.edits().len();
        tracing::debug!(notes = n, "commit (no DAW attached)");
        Err(format!("no DAW attached — would write {n} notes"))
    }

    fn revert(&self, session: &Session) -> Result<usize, String> {
        Err(format!(
            "no DAW attached — would restore {} notes",
            session.baseline().len()
        ))
    }

    fn resync(&self, _session: &mut Session) -> Result<(), String> {
        Ok(())
    }
}
