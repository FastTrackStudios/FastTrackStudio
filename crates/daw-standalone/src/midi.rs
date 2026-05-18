//! `impl Midi for Standalone` — every method panics with
//! `todo!("standalone: …")`. Standalone's `ProjectState` doesn't yet
//! model MIDI takes / notes / CCs / pitch bends / sysex — those need
//! a fresh in-memory schema before real impls land.

use daw_proto::TrackRef;
use daw_proto::midi::{
    HumanizeParams, Midi, MidiCC, MidiCCCreate, MidiNote, MidiNoteCreate, MidiPitchBend,
    MidiPitchBendCreate, MidiProgramChange, MidiSysEx, MidiTakeLocation, PpqRange, QuantizeParams,
};
use daw_proto::project::ProjectContext;

use crate::sync::Standalone;

impl Midi for Standalone {
    fn notes(&self, _location: MidiTakeLocation) -> Vec<MidiNote> {
        todo!("standalone: Midi::notes — MIDI take storage not yet modeled")
    }
    fn notes_in_range(&self, _location: MidiTakeLocation, _range: PpqRange) -> Vec<MidiNote> {
        todo!("standalone: Midi::notes_in_range")
    }
    fn selected_notes(&self, _location: MidiTakeLocation) -> Vec<MidiNote> {
        todo!("standalone: Midi::selected_notes")
    }
    fn note_count(&self, _location: MidiTakeLocation) -> u32 {
        todo!("standalone: Midi::note_count")
    }
    fn create_midi_item(
        &self,
        _project: ProjectContext,
        _track: TrackRef,
        _start_seconds: f64,
        _end_seconds: f64,
    ) -> Option<MidiTakeLocation> {
        todo!("standalone: Midi::create_midi_item")
    }
    fn add_note(&self, _location: MidiTakeLocation, _note: MidiNoteCreate) -> u32 {
        todo!("standalone: Midi::add_note")
    }
    fn add_notes(&self, _location: MidiTakeLocation, _notes: Vec<MidiNoteCreate>) -> Vec<u32> {
        todo!("standalone: Midi::add_notes")
    }
    fn delete_note(&self, _location: MidiTakeLocation, _index: u32) {
        todo!("standalone: Midi::delete_note")
    }
    fn delete_notes(&self, _location: MidiTakeLocation, _indices: Vec<u32>) {
        todo!("standalone: Midi::delete_notes")
    }
    fn delete_selected_notes(&self, _location: MidiTakeLocation) {
        todo!("standalone: Midi::delete_selected_notes")
    }
    fn set_note_pitch(&self, _location: MidiTakeLocation, _index: u32, _pitch: u8) {
        todo!("standalone: Midi::set_note_pitch")
    }
    fn set_note_velocity(&self, _location: MidiTakeLocation, _index: u32, _velocity: u8) {
        todo!("standalone: Midi::set_note_velocity")
    }
    fn set_note_position(&self, _location: MidiTakeLocation, _index: u32, _start_ppq: f64) {
        todo!("standalone: Midi::set_note_position")
    }
    fn set_note_length(&self, _location: MidiTakeLocation, _index: u32, _length_ppq: f64) {
        todo!("standalone: Midi::set_note_length")
    }
    fn set_note_channel(&self, _location: MidiTakeLocation, _index: u32, _channel: u8) {
        todo!("standalone: Midi::set_note_channel")
    }
    fn set_note_selected(&self, _location: MidiTakeLocation, _index: u32, _selected: bool) {
        todo!("standalone: Midi::set_note_selected")
    }
    fn set_note_muted(&self, _location: MidiTakeLocation, _index: u32, _muted: bool) {
        todo!("standalone: Midi::set_note_muted")
    }
    fn select_all_notes(&self, _location: MidiTakeLocation, _selected: bool) {
        todo!("standalone: Midi::select_all_notes")
    }
    fn transpose_notes(&self, _location: MidiTakeLocation, _indices: Vec<u32>, _semitones: i8) {
        todo!("standalone: Midi::transpose_notes")
    }
    fn quantize_notes(&self, _location: MidiTakeLocation, _params: QuantizeParams) {
        todo!("standalone: Midi::quantize_notes")
    }
    fn humanize_notes(&self, _location: MidiTakeLocation, _params: HumanizeParams) {
        todo!("standalone: Midi::humanize_notes")
    }
    fn ccs(&self, _location: MidiTakeLocation, _controller: Option<u8>) -> Vec<MidiCC> {
        todo!("standalone: Midi::ccs")
    }
    fn add_cc(&self, _location: MidiTakeLocation, _cc: MidiCCCreate) -> u32 {
        todo!("standalone: Midi::add_cc")
    }
    fn delete_cc(&self, _location: MidiTakeLocation, _index: u32) {
        todo!("standalone: Midi::delete_cc")
    }
    fn set_cc_value(&self, _location: MidiTakeLocation, _index: u32, _value: u8) {
        todo!("standalone: Midi::set_cc_value")
    }
    fn pitch_bends(&self, _location: MidiTakeLocation) -> Vec<MidiPitchBend> {
        todo!("standalone: Midi::pitch_bends")
    }
    fn add_pitch_bend(&self, _location: MidiTakeLocation, _pb: MidiPitchBendCreate) -> u32 {
        todo!("standalone: Midi::add_pitch_bend")
    }
    fn program_changes(&self, _location: MidiTakeLocation) -> Vec<MidiProgramChange> {
        todo!("standalone: Midi::program_changes")
    }
    fn sysex(&self, _location: MidiTakeLocation) -> Vec<MidiSysEx> {
        todo!("standalone: Midi::sysex")
    }
}
