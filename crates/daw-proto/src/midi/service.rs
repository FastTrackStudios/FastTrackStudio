//! MIDI editing service — notes, CCs, pitch bends, program changes,
//! SysEx in MIDI takes.
//!
//! For realtime MIDI I/O see `LiveMidi`.

use super::{MidiCC, MidiNote, MidiNoteCreate, MidiPitchBend, MidiProgramChange, MidiSysEx};
use crate::TrackRef;
use crate::item::{ItemRef, TakeRef};
use crate::project::ProjectContext;
use facet::Facet;

/// Location of a MIDI take (project + item + take).
#[derive(Clone, Debug, Facet)]
pub struct MidiTakeLocation {
    pub project: ProjectContext,
    pub item: ItemRef,
    pub take: TakeRef,
}

impl MidiTakeLocation {
    pub fn new(project: ProjectContext, item: ItemRef, take: TakeRef) -> Self {
        Self {
            project,
            item,
            take,
        }
    }

    /// For the active take of an item.
    pub fn active(project: ProjectContext, item: ItemRef) -> Self {
        Self::new(project, item, TakeRef::Active)
    }
}

/// PPQ range for queries.
#[derive(Clone, Debug, Facet)]
pub struct PpqRange {
    pub start: f64,
    pub end: f64,
}

impl PpqRange {
    pub fn new(start: f64, end: f64) -> Self {
        Self { start, end }
    }
}

/// Quantize parameters.
#[derive(Clone, Debug, Facet)]
pub struct QuantizeParams {
    /// Empty = selected notes.
    pub indices: Vec<u32>,
    /// Grid size in PPQ (1.0 = quarter note).
    pub grid_ppq: f64,
    /// Strength (0.0 = no change, 1.0 = full snap).
    pub strength: f64,
}

/// Humanize parameters.
#[derive(Clone, Debug, Facet)]
pub struct HumanizeParams {
    /// Empty = selected notes.
    pub indices: Vec<u32>,
    /// Random timing variation range in PPQ.
    pub timing_range_ppq: f64,
    /// Random velocity variation range.
    pub velocity_range: u8,
}

/// CC event create parameters.
#[derive(Clone, Debug, Facet)]
pub struct MidiCCCreate {
    pub channel: u8,
    pub controller: u8,
    pub value: u8,
    pub position_ppq: f64,
}

impl MidiCCCreate {
    pub fn new(channel: u8, controller: u8, value: u8, position_ppq: f64) -> Self {
        Self {
            channel: channel & 0x0F,
            controller: controller & 0x7F,
            value: value & 0x7F,
            position_ppq,
        }
    }
}

/// Pitch bend event create parameters.
#[derive(Clone, Debug, Facet)]
pub struct MidiPitchBendCreate {
    pub channel: u8,
    /// Pitch bend value (-8192 to 8191).
    pub value: i16,
    pub position_ppq: f64,
}

impl MidiPitchBendCreate {
    pub fn new(channel: u8, value: i16, position_ppq: f64) -> Self {
        Self {
            channel: channel & 0x0F,
            value: value.clamp(-8192, 8191),
            position_ppq,
        }
    }
}

#[architect_rpc_derive::rpc]
pub trait Midi {
    // ── Notes ──────────────────────────────────────────────────────

    fn notes(&self, location: MidiTakeLocation) -> Vec<MidiNote>;

    fn notes_in_range(&self, location: MidiTakeLocation, range: PpqRange) -> Vec<MidiNote>;

    fn selected_notes(&self, location: MidiTakeLocation) -> Vec<MidiNote>;
    fn note_count(&self, location: MidiTakeLocation) -> u32;

    /// Create a new empty MIDI item on a track. Returns the take
    /// location of the new item's active take.
    fn create_midi_item(
        &self,
        project: ProjectContext,
        track: TrackRef,
        start_seconds: f64,
        end_seconds: f64,
    ) -> Option<MidiTakeLocation>;

    /// Add a note. Returns the note index.
    fn add_note(&self, location: MidiTakeLocation, note: MidiNoteCreate) -> u32;

    fn add_notes(&self, location: MidiTakeLocation, notes: Vec<MidiNoteCreate>) -> Vec<u32>;
    fn delete_note(&self, location: MidiTakeLocation, index: u32);
    fn delete_notes(&self, location: MidiTakeLocation, indices: Vec<u32>);
    fn delete_selected_notes(&self, location: MidiTakeLocation);

    fn set_note_pitch(&self, location: MidiTakeLocation, index: u32, pitch: u8);
    fn set_note_velocity(&self, location: MidiTakeLocation, index: u32, velocity: u8);
    fn set_note_position(&self, location: MidiTakeLocation, index: u32, start_ppq: f64);
    fn set_note_length(&self, location: MidiTakeLocation, index: u32, length_ppq: f64);
    fn set_note_channel(&self, location: MidiTakeLocation, index: u32, channel: u8);
    fn set_note_selected(&self, location: MidiTakeLocation, index: u32, selected: bool);
    fn set_note_muted(&self, location: MidiTakeLocation, index: u32, muted: bool);

    // ── Batch ops ──────────────────────────────────────────────────

    fn select_all_notes(&self, location: MidiTakeLocation, selected: bool);
    fn transpose_notes(&self, location: MidiTakeLocation, indices: Vec<u32>, semitones: i8);
    fn quantize_notes(&self, location: MidiTakeLocation, params: QuantizeParams);
    fn humanize_notes(&self, location: MidiTakeLocation, params: HumanizeParams);

    // ── CCs ────────────────────────────────────────────────────────

    fn ccs(&self, location: MidiTakeLocation, controller: Option<u8>) -> Vec<MidiCC>;
    fn add_cc(&self, location: MidiTakeLocation, cc: MidiCCCreate) -> u32;
    fn delete_cc(&self, location: MidiTakeLocation, index: u32);
    fn set_cc_value(&self, location: MidiTakeLocation, index: u32, value: u8);

    // ── Other event types ─────────────────────────────────────────

    fn pitch_bends(&self, location: MidiTakeLocation) -> Vec<MidiPitchBend>;
    fn add_pitch_bend(&self, location: MidiTakeLocation, pb: MidiPitchBendCreate) -> u32;
    fn program_changes(&self, location: MidiTakeLocation) -> Vec<MidiProgramChange>;
    fn sysex(&self, location: MidiTakeLocation) -> Vec<MidiSysEx>;
}

#[cfg(feature = "vox")]
pub use MidiRpcDispatcher as Dispatcher;
#[cfg(feature = "vox")]
pub use midi_rpc_service_descriptor as descriptor;
