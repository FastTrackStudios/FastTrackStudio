//! Reads notes out of a DAW project and writes velocities back in.
//!
//! The half of midi-tools that knows what a project is. `midi-tools`
//! itself is arithmetic over `&[Note]` and depends on nothing, so the
//! panel can run in a desktop window with no DAW behind it; this crate
//! is what you provide when there *is* one.
//!
//! Everything goes through [`daw::service::Midi`], never through
//! `reaper_medium` directly, so the same tool drives REAPER,
//! daw-standalone, or a test backend without knowing which it has.

use std::sync::Mutex;

use daw::service::{Items, Midi, MidiTakeLocation, ProjectContext};
use daw::service::item::ItemRef;
use midi_tools::velocity::{Note, Session, VelocityEdit};

/// What velocity editing needs from a backend.
pub trait VelocityDaw: Items + Midi + Send + Sync + 'static {}
impl<T> VelocityDaw for T where T: Items + Midi + Send + Sync + 'static {}

/// Where a velocity edit lands, and how it gets there.
pub struct DawVelocitySink<D> {
    daw: D,
    /// The take the session was opened on. Held so every write goes to
    /// the same place the notes were read from — re-resolving per write
    /// would silently retarget mid-drag if the user clicked another item.
    location: Mutex<Option<MidiTakeLocation>>,
}

impl<D> DawVelocitySink<D> {
    pub fn new(daw: D) -> Self {
        Self {
            daw,
            location: Mutex::new(None),
        }
    }

    /// The take currently bound, if [`DawVelocitySink::open`] has run.
    pub fn location(&self) -> Option<MidiTakeLocation> {
        self.location.lock().ok().and_then(|l| l.clone())
    }
}

impl<D: VelocityDaw> DawVelocitySink<D> {
    /// Bind to the first selected item's active take and read its notes
    /// into a fresh [`Session`].
    ///
    /// The selected item rather than the active MIDI editor's take: the
    /// `daw` facade has no notion of an editor window (deliberately — it
    /// is DAW-agnostic, and "the MIDI editor" is a REAPER concept), and
    /// in REAPER opening a take in the editor selects its item anyway.
    /// The convention also means the tool works with no editor open at
    /// all, which MVelocity refuses to do.
    ///
    /// The *first* selected item, not all of them: the pattern and curve
    /// engines index notes by their position in the take, and spanning
    /// several items would make a pattern's phase depend on which items
    /// happened to be selected together.
    pub fn open(&self) -> Result<Session, String> {
        let project = ProjectContext::Current;

        let item = self
            .daw
            .get_selected_items(project.clone())
            .into_iter()
            .next()
            .ok_or_else(|| "select a MIDI item first".to_string())?;

        let location = MidiTakeLocation::active(project, ItemRef::Guid(item.guid));
        let notes = self.read(&location)?;

        *self
            .location
            .lock()
            .map_err(|_| "sink is poisoned".to_string())? = Some(location);

        Ok(Session::new(notes))
    }

    /// Re-read the bound take, keeping the session's parameters.
    ///
    /// For when the take changed underneath the panel — notes added or
    /// deleted, or the selection moved. The panel polls this; nothing
    /// here watches the project.
    pub fn resync(&self, session: &mut Session) -> Result<(), String> {
        let location = self
            .location()
            .ok_or_else(|| "no take bound — open the tool on an item".to_string())?;
        session.resync(self.read(&location)?);
        Ok(())
    }

    /// Whether the bound take's notes still match `session`'s baseline in
    /// count and selection.
    ///
    /// Cheap enough to poll: it compares what the backend already had to
    /// return. A `false` means [`DawVelocitySink::resync`] is due.
    pub fn is_current(&self, session: &Session) -> bool {
        let Some(location) = self.location() else {
            return false;
        };
        match self.read(&location) {
            Ok(now) => {
                now.len() == session.baseline().len()
                    && now
                        .iter()
                        .zip(session.baseline())
                        .all(|(a, b)| a.index == b.index && a.selected == b.selected)
            }
            Err(_) => false,
        }
    }

    /// Write the session's result to the bound take.
    ///
    /// Only notes that actually moved are written — [`Session::edits`]
    /// filters against the baseline, so a session sitting at neutral
    /// costs nothing rather than rewriting every note with its own value.
    /// That matters here: `set_note_velocity` is one call per note, and
    /// over a wire transport a 600-note take at neutral would otherwise
    /// be 600 round-trips per frame of slider drag.
    ///
    /// Returns how many notes were written.
    pub fn commit(&self, session: &Session) -> Result<usize, String> {
        let location = self
            .location()
            .ok_or_else(|| "no take bound — open the tool on an item".to_string())?;
        let edits = session.edits();
        self.write(&location, &edits);
        Ok(edits.len())
    }

    /// Put the take back exactly as the session found it.
    ///
    /// Not "set every control to neutral and commit" — that writes
    /// nothing, because a neutral session's edits are empty. Restoring
    /// means writing the baseline back over whatever the last commit
    /// left behind.
    pub fn revert(&self, session: &Session) -> Result<usize, String> {
        let location = self
            .location()
            .ok_or_else(|| "no take bound — open the tool on an item".to_string())?;
        let edits: Vec<VelocityEdit> = session
            .baseline()
            .iter()
            .map(|n| VelocityEdit {
                index: n.index,
                velocity: n.velocity,
            })
            .collect();
        self.write(&location, &edits);
        Ok(edits.len())
    }

    fn read(&self, location: &MidiTakeLocation) -> Result<Vec<Note>, String> {
        let notes: Vec<Note> = self
            .daw
            .notes(location.clone())
            .into_iter()
            .map(|n| Note {
                index: n.index,
                velocity: n.velocity,
                selected: n.selected,
            })
            .collect();
        if notes.is_empty() {
            return Err("that item has no MIDI notes".to_string());
        }
        Ok(notes)
    }

    fn write(&self, location: &MidiTakeLocation, edits: &[VelocityEdit]) {
        for edit in edits {
            self.daw
                .set_note_velocity(location.clone(), edit.index, edit.velocity);
        }
    }
}

/// The panel's seam, satisfied by a real project.
///
/// The inherent methods above carry the docs and are what a Rust caller
/// wants; this is the object-safe view the UI holds in a
/// `Arc<dyn VelocitySink>`.
impl<D: VelocityDaw> midi_tools::VelocitySink for DawVelocitySink<D> {
    fn open(&self) -> Result<Session, String> {
        DawVelocitySink::open(self)
    }

    fn commit(&self, session: &Session) -> Result<usize, String> {
        DawVelocitySink::commit(self, session)
    }

    fn revert(&self, session: &Session) -> Result<usize, String> {
        DawVelocitySink::revert(self, session)
    }

    fn resync(&self, session: &mut Session) -> Result<(), String> {
        DawVelocitySink::resync(self, session)
    }
}
