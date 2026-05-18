//! `impl LiveMidi for Standalone` — every method panics with
//! `todo!("standalone: …")` so missing functionality surfaces at the
//! call site instead of returning silent defaults.

use daw_proto::live_midi::{
    LiveMidi, MidiInputDevice, MidiMessage, MidiOutputDevice, SendMidiTiming, StuffMidiTarget,
};

use crate::sync::Standalone;

impl LiveMidi for Standalone {
    fn input_devices(&self) -> Vec<MidiInputDevice> {
        todo!("standalone: LiveMidi::input_devices — no live MIDI device enumeration yet")
    }
    fn output_devices(&self) -> Vec<MidiOutputDevice> {
        todo!("standalone: LiveMidi::output_devices — no live MIDI device enumeration yet")
    }
    fn input_device(&self, _id: u32) -> Option<MidiInputDevice> {
        todo!("standalone: LiveMidi::input_device")
    }
    fn output_device(&self, _id: u32) -> Option<MidiOutputDevice> {
        todo!("standalone: LiveMidi::output_device")
    }
    fn open_input_device(&self, _id: u32) -> bool {
        todo!("standalone: LiveMidi::open_input_device")
    }
    fn close_input_device(&self, _id: u32) {
        todo!("standalone: LiveMidi::close_input_device")
    }
    fn open_output_device(&self, _id: u32) -> bool {
        todo!("standalone: LiveMidi::open_output_device")
    }
    fn close_output_device(&self, _id: u32) {
        todo!("standalone: LiveMidi::close_output_device")
    }
    fn send_midi(&self, _device_id: u32, _message: MidiMessage, _timing: SendMidiTiming) {
        todo!("standalone: LiveMidi::send_midi")
    }
    fn subscribe_input(&self, _device_id: u32) -> bool {
        todo!("standalone: LiveMidi::subscribe_input")
    }
    fn unsubscribe_input(&self, _device_id: u32) {
        todo!("standalone: LiveMidi::unsubscribe_input")
    }
    fn stuff_midi_message(&self, _target: StuffMidiTarget, _message: MidiMessage) {
        todo!("standalone: LiveMidi::stuff_midi_message")
    }
}
